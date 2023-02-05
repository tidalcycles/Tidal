module Sound.Tidal.Listener.Parse where

isSeperator :: String -> Bool
isSeperator ('\n':xs) = case mungeWhite xs of
                                  ('\n':_) -> True
                                  _ -> False
                      where mungeWhite (' ':ys) = mungeWhite ys
                            mungeWhite ('\t':ys) = mungeWhite ys
                            mungeWhite x = x
isSeperator _ = False

oneBlock :: String -> (String,String)
oneBlock s = case isSeperator rest of
                      False -> case rest == "" of
                                    True -> (white++b++rest,"")
                                    False -> (white++b++r,t)
                      True -> (white++b,rest)
           where (white,s2) = break (\x -> not $ elem x " \t\n") s
                 (b,rest) = break (=='\n') s2
                 (r,t) = oneBlock rest

blocks :: String -> [String]
blocks s = case rest == "" of
                    True -> [b]
                    False -> b:(blocks rest)
         where (b,rest) = oneBlock s
