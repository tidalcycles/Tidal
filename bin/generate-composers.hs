import           Data.List
import           System.IO

whats :: [(String, String, [(String, String, String, String)])]
whats = [("(Pattern p, Unionable a) => p a -> p a -> p a",
          "(Unionable a) => Signal a -> Signal a -> Signal a",
          [
            ("set", "=", "flip union", ""),
            ("keep", ".", "union", "")
          ]
         ),
         ("(Pattern p, Unionable a) => p a -> p Bool -> p a",
          "(Unionable a) => Signal a -> Signal Bool -> Signal a",
          [
            ("keepif", "?", "\\a b -> if b then Just a else Nothing", "filterJusts $ ")
          ]
         ),
         ("(Pattern p, Num a) => p a -> p a -> p a",
          "(Num a) => Signal a -> Signal a -> Signal a",
          [
            ("add", "+", "+", ""),
            ("sub", "-", "-", ""),
            ("mul", "*", "*", "")
          ]
         ),
         ("(Pattern p, Fractional a) => p a -> p a -> p a",
          "(Fractional a) => Signal a -> Signal a -> Signal a",
          [
            ("div", "/", "/", "")
          ]
         ),
         ("(Pattern p, Integral a) => p a -> p a -> p a",
          "(Integral a) => Signal a -> Signal a -> Signal a",
          [
            ("mod", "%", "mod", ""),
            ("pow", "^", "^", "")
          ]
         ),
         ("(Pattern p, Floating a) => p a -> p a -> p a",
          "(Floating a) => Signal a -> Signal a -> Signal a",
          [
            ("powf", "**", "**", "")
          ]
         ),
         ("Pattern p => p String -> p String -> p String",
          "Signal String -> Signal String -> Signal String",
          [
            ("concat", "++", "++", "")
          ]
         ),
         ("(Pattern p, Bits a) => p a -> p a -> p a",
          "(Bits a) => Signal a -> Signal a -> Signal a",
          [
            ("band", ".&.", ".&.", ""),
            ("bor", ".|.", ".|.", ""),
            ("bxor", ".^.", "xor", "")
          ]
         ),
         ("(Pattern p, Bits a) => p a -> p Int -> p a",
          "(Bits a) => Signal a -> Signal Int -> Signal a",
          [
            ("bshiftl", ".<<.", "shiftL", ""),
            ("bshiftr", ".>>.", "shiftR", "")
          ]
         ),
         ("(Pattern p, Ord a) => p a -> p a -> p Bool",
          "(Ord a) => Signal a -> Signal a -> Signal Bool",
          [
            ("lt", "<", "<", ""),
            ("gt", ">", ">", ""),
            ("lte", "<=", "<=", ""),
            ("gte", ">=", ">=", "")
          ]
         ),
         ("(Pattern p, Eq a) => p a -> p a -> p Bool",
          "(Eq a) => Signal a -> Signal a -> Signal Bool",
          [
            ("eq", "==", "==", ""),
            ("ne", "/=", "/=", "")
          ]
         ),
         ("Pattern p => p Bool -> p Bool -> p Bool",
          "Signal Bool -> Signal Bool -> Signal Bool",
          [
            ("and", "&&", "&&", ""),
            ("or", ".||.", "||", "")
          ]
         )
        ]

hows :: [(String, (String -> String))]
hows = [("Mix",      (\x -> "|" ++ x ++ "|")),
        ("In",       ("|" ++)  ),
        ("Out",      (++ "|")  ),
        ("Squeeze",  ("||" ++) ),
        ("SqueezeOut",  (++ "||") ),
        ("Trig",     ("!" ++)  ),
        ("Trigzero", ("!!" ++) )
       ]

fwhat (sigpat, sigsig, ops) = concatMap fop ops
  where fop (name, tidalop, haskellop, munge) = "-- " ++ name ++ "\n\n" ++ concatMap fhow hows ++ "infixl 4 " ++ (intercalate ", " $ map fixity hows) ++ "\n\n"
          where fhow (howname, howfix) = (name ++ howname ++ ", " ++ "(" ++ howfix tidalop ++ ")" ++ " :: " ++ sig howname ++ "\n"
                                          ++ name ++ howname ++ " pata patb = " ++ munge ++ "op" ++ howname ++ " (" ++ haskellop ++ ") pata patb\n"
                                          ++ "(" ++ howfix tidalop ++ ") = " ++ name ++ howname ++ "\n\n"
                                         )
                fixity (_, howfix) = howfix tidalop
                sig "Mix" = sigpat
                sig "In"  = sigpat
                sig "Out" = sigpat
                sig _     = sigsig

header :: IO ()
header = do x <- openFile "composers-header.hs" ReadMode
            y <- hGetContents x
            putStr y

main = do header
          putStrLn $ concatMap fwhat whats

