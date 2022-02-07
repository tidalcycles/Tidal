import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Version
import Distribution.Package (PackageIdentifier (..))
import Distribution.PackageDescription (GenericPackageDescription (..), HookedBuildInfo, PackageDescription (..))
import Distribution.Simple
import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..), absoluteInstallDirs, bindir)
import Distribution.Simple.Program
import Distribution.Simple.Setup (BuildFlags (..), CopyFlags (..), ConfigFlags (..), ReplFlags (..), fromFlag, defaultCopyFlags)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.Executable (Executable (..))
import Distribution.Types.Library (Library (..))
import qualified Distribution.Verbosity as Verbosity
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Directory
import System.FilePath
import Distribution.Simple.BuildPaths (dllExtension)
import Distribution.System

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = linkBuildHook
        , replHook = linkReplHook
      }

isWindows :: LocalBuildInfo -> Bool
isWindows lbinfo =
  case hostPlatform lbinfo of
    (Platform _ Windows) -> True
    _                    -> False

linkBuildHook ::
  PackageDescription ->
  LocalBuildInfo ->
  UserHooks ->
  BuildFlags ->
  IO ()
linkBuildHook pdesc lbinfo uhooks bflags
  | isWindows lbinfo =
      buildTidalLink pdesc lbinfo >>
      buildHook simpleUserHooks (addTidalLinkToPackage pdesc lbinfo) lbinfo uhooks bflags
  | otherwise =
      buildHook simpleUserHooks pdesc lbinfo uhooks bflags

linkReplHook ::
  PackageDescription ->
  LocalBuildInfo ->
  UserHooks ->
  ReplFlags ->
  [String] ->
  IO ()
linkReplHook pdesc lbinfo uhooks rflags args
  | isWindows lbinfo =
    buildTidalLink pdesc lbinfo >>
    replHook simpleUserHooks (addTidalLinkToPackage pdesc lbinfo) lbinfo uhooks rflags args
  | otherwise =
    replHook simpleUserHooks pdesc lbinfo uhooks rflags args
sharedLibName :: String
sharedLibName = "tidallink"

binInstDir ::
  PackageDescription ->
  LocalBuildInfo ->
  String
binInstDir pdesc lbinfo = bindir $ absoluteInstallDirs pdesc lbinfo
                            (fromFlag $ copyDest defaultCopyFlags)

buildTidalLink ::
  PackageDescription ->
  LocalBuildInfo ->
  IO String
buildTidalLink pdesc lbinfo = do
  getDbProgramOutput
    Verbosity.normal
    gccProgram
    (withPrograms lbinfo)
    args
  where
    args :: [ProgArg]
    args =
      ["-xc++", "-static-libgcc", "-static-libstdc++", "-shared", "-o"
      , (binInstDir pdesc lbinfo) </> sharedLibName <.> (dllExtension $ hostPlatform lbinfo)
      , "-Isrc/c", "-Ilink/include"
      , "-Ilink/modules/asio-standalone/asio/include", "-Ilink/extensions/abl_link/include"
      , "link/extensions/abl_link/src/abl_link.cpp"
      , "-DLINK_PLATFORM_WINDOWS=1"
      , "-Wno-multichar", "-Wno-subobject-linkage", "-g", "-liphlpapi", "-lwinmm"
      , "-lws2_32", "-lstdc++"]

addTidalLinkToPackage ::
  PackageDescription ->
  LocalBuildInfo ->
  PackageDescription
addTidalLinkToPackage pdesc lbinfo =
  let newLib = addExtraToLibrary sharedLibName (binInstDir pdesc lbinfo) $ fromJust (library pdesc)
      newExecutables = addExtraToExecutables sharedLibName (binInstDir pdesc lbinfo) $ executables pdesc
   in pdesc {library = Just (newLib), executables = newExecutables}

addExtraToLibrary :: String -> FilePath -> Library -> Library
addExtraToLibrary s path lib = lib {libBuildInfo = addExtraToBuildInfo s path $ libBuildInfo lib}

addExtraToExecutables :: String -> FilePath -> [Executable] -> [Executable]
addExtraToExecutables s path exes = map f exes
  where
    newExecutableBuildInfo = addExtraToBuildInfo s path . buildInfo
    f exe = exe {buildInfo = newExecutableBuildInfo exe}

addExtraToBuildInfo :: String -> FilePath -> BuildInfo -> BuildInfo
addExtraToBuildInfo s path bi =
  let oldExtraLibs = extraLibs bi
      newExtraLibs = s : oldExtraLibs
      oldExtraLibDirs = extraLibDirs bi
      newExtraLibDirs = path : oldExtraLibDirs
   in bi {extraLibs = newExtraLibs, extraLibDirs = newExtraLibDirs}
