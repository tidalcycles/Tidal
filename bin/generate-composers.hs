import System.IO
import Data.List

whats :: [(String, [(String, String, String)])]
whats = [("Unionable a => Signal a -> Signal a -> Signal a",
          [
            ("set", "=", "flip union"),
            ("keep", ".", "union")
          ]
         ),
         ("Num a => Signal a -> Signal a -> Signal a",
          [
            ("add", "+", "+"),
           ("sub", "-", "-"),
           ("mul", "*", "*")
          ]
         ),
         ("Fractional a => Signal a -> Signal a -> Signal a",
          [
            ("div", "/", "/")
          ]
         ),
         ("Integral a => Signal a -> Signal a -> Signal a",
          [
            ("mod", "%", "mod"),
            ("pow", "^", "^")
          ]
         ),
         ("Bits a => Signal a -> Signal a -> Signal a",
          [
            ("band", ".&.", ".&."),
            ("bor", ".|.", ".|."),
            ("bxor", ".^.", "xor")
          ]
         ),
         ("Bits a => Signal a -> Signal Int -> Signal a",
          [
            ("bshiftl", ".<<.", "shiftL"),
            ("bshiftr", ".>>.", "shiftR")
          ]
         ),
         ("Ord a => Signal a -> Signal a -> Signal Bool",
          [
            ("lt", "<", "<"),
            ("gt", ">", ">"),
            ("lte", "<=", "<="),
            ("gte", ">=", ">=")
          ]
         ),
         ("Eq a => Signal a -> Signal a -> Signal Bool",
          [
            ("eq", "==", "=="),
            ("ne", "/=", "/=")
          ]
         ),
         ("Signal Bool -> Signal Bool -> Signal Bool",
          [
            ("and", "&&", "&&"),
            ("or", ".||.", "||")
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
        ("TrigZero", ("!!" ++) )
       ]

fwhat (sig, ops) = concatMap fop ops
  where fop (name, tidalop, haskellop) = "-- " ++ name ++ "\n\n" ++ concatMap fhow hows ++ "infix 4 " ++ (intercalate ", " $ map fixity hows) ++ "\n\n"
          where fhow (howname, howfix) = (name ++ howname ++ ", " ++ "(" ++ howfix tidalop ++ ")" ++ " :: " ++ sig ++ "\n"
                                          ++ name ++ howname ++ " = op" ++ howname ++ " (" ++ haskellop ++ ")\n"
                                          ++ "(" ++ howfix tidalop ++ ") = " ++ name ++ howname ++ "\n\n"
                                         )
                fixity (_, howfix) = howfix tidalop

header :: IO ()
header = do x <- openFile "composers-header.hs" ReadMode
            y <- hGetContents x
            putStr y

main = do header
          putStrLn $ concatMap fwhat whats

