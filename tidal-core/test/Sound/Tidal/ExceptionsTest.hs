{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ExceptionsTest where

import Control.DeepSeq
import Control.Exception
import Data.Typeable ()
import Sound.Tidal.Pattern
import Test.Hspec
import Prelude hiding ((*>), (<*))

run :: Spec
run =
  describe "NFData, forcing and catching exceptions" $ do
    describe "instance NFData (Pattern a)" $ do
      it "rnf forces argument" $ do
        evaluate (rnf (Pattern undefined Nothing Nothing :: Pattern ()))
          `shouldThrow` anyException


anyErrorCall :: Selector ErrorCall
anyErrorCall = const True

errorCall :: String -> Selector ErrorCall
#if MIN_VERSION_base(4,9,0)
errorCall s (ErrorCallWithLocation msg _) = s == msg
#else
errorCall s (ErrorCall msg)               = s == msg
#endif

anyIOException :: Selector IOException
anyIOException = const True

anyArithException :: Selector ArithException
anyArithException = const True
