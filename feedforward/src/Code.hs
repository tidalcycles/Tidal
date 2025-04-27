module Code where

import           Sound.Tidal.Context     hiding (when)
import           Data.Maybe              (fromJust, fromMaybe, isJust, mapMaybe)
import           Data.List               ((\\))

type Tag = Int

data Status = Success | Error | Normal deriving (Show, Eq)

data Block = Block {bTag      :: Tag,
                    bModified :: Bool,
                    bStatus   :: Status,
                    bPattern  :: Maybe ControlPattern,
                    bMute     :: Bool,
                    bSolo     :: Bool
                   }
             deriving Show

data Line = Line {lBlock :: Maybe Block,
                  lText  :: String
                 }
             deriving Show

type Code = [Line]


lTag :: Line -> Maybe Tag
lTag l = bTag <$> lBlock l

lMuted :: Line -> Bool
lMuted l = fromMaybe False $ bMute <$> lBlock l

lModified :: Line -> Bool
lModified l = fromMaybe False $ bModified <$> lBlock l

lMute :: Line -> Bool
lMute Line {lBlock = Just Block {bMute = a}} = a
lMute _                                      = False

lStatus :: Line -> Maybe Status
lStatus l = bStatus <$> lBlock l

setTag :: Line -> Tag -> Line
setTag l@(Line {lBlock = Just b}) tag = l {lBlock = Just (b {bTag = tag})}
setTag l@(Line {lBlock = Nothing}) tag
  = l {lBlock = Just (Block {bTag = tag,
                             bModified=True,
                             bStatus = Normal,
                             bPattern = Nothing,
                             bMute = False,
                             bSolo = False
                            }
                     )
      }

hasChar :: Line -> Bool
hasChar = any (/= ' ') . lText

updateTags :: Code -> Code
updateTags ls = assignTags freeTags ls'
  where assignTags :: [Tag] -> Code -> Code
        assignTags [] (l:ls) = l:ls
        assignTags _ [] = []
        assignTags ids (l:ls) | lTag l == Just (-1) = setTag l (head ids):(assignTags (tail ids) ls)
                              | otherwise = l:(assignTags ids ls)
        freeTags = ([1 .. 9]++[0]) \\ tagIds
        tagIds = mapMaybe lTag ls'
        ls' = map tag toTag
        tag :: (Bool, Line) -> Line
        tag (False, l) = l {lBlock = Nothing}
        tag (True, l) | isJust (lTag l) = l
                      | otherwise = setTag l (-1) -- mark to tag
        toTag :: [(Bool, Line)]
        toTag = taggable True ls
        taggable :: Bool -> Code -> [(Bool, Line)]
        taggable _ [] = []
        taggable prevEmpty (l:ls) = (prevEmpty && (not empty), l):(taggable empty ls)
          where empty = not $ hasChar l

codeEvents :: Rational -> Code -> [(Int,Int,Int)]
codeEvents t ls = loop 0 ls
  where loop :: Int -> Code -> [(Int,Int,Int)]
        loop _ [] = []
        loop n ((Line {lBlock = Just (Block {bPattern = Just pat, bMute = False})}):ls)
          = (locs n pat) ++ (loop (n+1) ls)
        loop n (_:ls) = loop (n+1) ls
        locs :: Int -> ControlPattern -> [(Int, Int, Int)]
        locs n pat = concatMap (evToLocs n) $ queryArc pat (Arc t t)
        evToLocs n (Event {context = Context xs}) = map (toLoc n) xs
        -- assume an event doesn't span a line..
        toLoc n ((bx, by), (ex, _)) = (n+by, bx, ex)

withTag :: Code -> Tag -> (Line -> Line) -> Code
withTag [] _ _ = []
withTag (l:ls) t f | lTag l == Just t = (f l):ls
                   | otherwise = l:(withTag ls t f)

charAt :: Code -> (Int,Int) -> Char
charAt ls (y,x) = (lText $ ls !! y) !! x

lineLength :: Code -> Int -> Int
lineLength ls y = length $ lText $ ls !! y

findBlock :: Code -> Int -> (Int, Block)
findBlock ls n | hasBlock (ls !! n) = (0, fromJust $ lBlock $ ls !! n)
               | otherwise = (\(offset, b) -> (offset-1, b)) $ findBlock ls (n-1)

hasBlock :: Line -> Bool
hasBlock = isJust . lBlock

allBlocks :: Int -> Code -> [(Int, Code)]
allBlocks _ [] = []
allBlocks n (l:ls) | hasBlock l = (n,b):(allBlocks (n+(length b)+1) ls')
                   | otherwise = allBlocks (n+1) ls
  where b = takeWhile hasChar (l:ls)
        ls' = drop (length b) ls

unmutedBlocks :: Code -> [(Int, Code)]
unmutedBlocks ls = filter (not . lMuted . (!!0) . snd) $ allBlocks 0 ls

dumpCode :: Code -> String
dumpCode ls = unlines $ map lText ls

unDumpCode :: String -> Code
unDumpCode s = updateTags $ map (Line Nothing) $ lines s

withLineText :: Line -> (String -> String)  -> Line
withLineText (Line tag text) f = Line tag (f text )
