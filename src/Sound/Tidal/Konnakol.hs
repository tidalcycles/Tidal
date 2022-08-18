module Sound.Tidal.Konnakol where

import Sound.Tidal.Context hiding (s,n)
import System.Random ( getStdGen, Random(randomR), StdGen, mkStdGen )
import Data.String ( IsString(fromString) )
import Data.List ( findIndex, intercalate, intersperse, isPrefixOf, tails )
import System.CPUTime ()

-- | BeatCount consists of the three ways in counting a cycle. Dhruta is typically 2 beats, while
-- Anudhruta is a single beat. The Jati of the Thala defines the Dhruta
data BeatCount = Laghu | Dhruta | Anudhruta deriving(Read)

-- | Define conventional notation used to represent the Thalas
instance Show BeatCount where
    show Laghu = "|"
    show Dhruta = "O"
    show Anudhruta = "U"

-- | Thala is a combination of BeatCounts
newtype Thala = T [BeatCount] deriving (Read)

-- | Standard 7 thalas in the Suladi Sapta Thala system
dhruva::Thala
dhruva = T [Laghu, Dhruta, Laghu, Laghu]
matya::Thala
matya = T [Laghu, Dhruta, Laghu]
rupaka::Thala
rupaka = T [Dhruta, Laghu]
jhampe::Thala
jhampe = T [Laghu, Dhruta, Anudhruta]
thriputa::Thala
thriputa = T [Laghu, Dhruta, Dhruta]
atta::Thala
atta = T [Laghu, Laghu, Dhruta, Dhruta]
eka::Thala
eka = T [Laghu]

-- | Define display of Thala based on that of the BeatCount
instance Show Thala where
    show (T []) = ""
    show (T (x:xs)) = show x ++ show (T xs)

-- | Common class to define both the Jati and Gati
data JatiGati = Tisra | Chaturasra | Khanda | Misra | Sankirna deriving (Show, Eq, Read)

-- | Define instance of enum class by enumerating each Jati / Gati to be the number of beats they represent
instance Enum JatiGati where
    fromEnum a
        | a == Tisra = 3
        | a == Chaturasra = 4
        | a == Khanda = 5
        | a == Misra = 7
        | otherwise =  9
    toEnum a
        | a == 3 = Tisra
        | a == 4 = Chaturasra
        | a == 5 = Khanda
        | a == 7 = Misra
        | a == 9 = Sankirna
        | otherwise = error "Unspecified Length"

-- | Define a class for all syllables used in Konnakol
data Syllable = Tha | Ki | Ta |Di | Dhi | Gi |Jho | Na | Thom |Lan |Gu | Dhin | Ku | Ri | Ka | Tham  | Thak |Dhim | Nam |Mi |Nu| Gdot deriving ( Eq)

-- | Show instance for syllables, to display the gaps using dots
instance Show Syllable where
    show Gdot = "-"
    show Tha = "Tha"
    show Ki = "Ki"
    show Ta = "Ta"
    show Dhi = "Dhi"
    show Gi = "Gi"
    show Jho = "Jho"
    show Na = "Na"
    show Thom = "Thom"
    show Lan = "Lan"
    show Gu = "Gu"
    show Dhin = "Dhin"
    show Ku = "Ku"
    show Ri = "Ri"
    show Ka = "Ka"
    show Tham = "Tham"
    show Thak ="Thak"
    show Dhim = "Dhim"
    show Nam = "Nam"
    show Mi = "Mi"
    show Nu = "Nu"
    show Di = "Di"

-- | Define instance of enum class by enumerating each Jati / Gati to be the number of beats they represent
instance Enum Syllable where
  fromEnum a
    | a == Tha = 2
    | a ==  Ki = 3
    | a == Ta = 4
    | a == Dhi = 5
    | a == Gi = 6
    | a == Jho = 7
    | a == Na = 8
    | a == Thom = 9
    | a == Lan = 10
    | a ==  Gu = 11
    | a == Dhin = 12
    | a == Ku = 13
    | a ==  Ri = 14
    | a ==  Ka = 15
    | a ==  Tham = 16
    | a ==  Thak = 17
    | a ==  Dhim = 18
    | a ==  Nam = 19
    | a == Mi = 20
    | a == Nu = 21
    | a == Gdot  = 1
    | otherwise = 22
  toEnum a
    | a == 1 = Gdot
    | otherwise = Gdot

-- | Define phrase as a collection of syllables, to faciliate show instance
newtype Phrase = Phr [Syllable]

-- | Show instance for a phrase
instance Show Phrase where
    show (Phr []) = ""
    show (Phr (x:xs)) = show x ++ show (Phr xs)

-- | Define Composition as collection of phrases with changes in speeds
data Comp = C [([Syllable], Int)] | K JatiGati deriving(Show)

-- | Method to show the Thala in the form Notation - Count - Gati
showFinalThala :: JatiGati -> Thala -> JatiGati -> String
showFinalThala jati thala gati =
    let a = show thala
        b = " (" ++ show ( calculateCount jati thala) ++ ") "
        c = " <" ++ show (fromEnum gati )++ "> "
    in a ++ b ++ c

-- | Get the Counts per beat in a certain thala in a particular gati
getCountPerBeat::JatiGati->Int->Int
getCountPerBeat gati maxS
  | maxS == 1 = 1
  | gati == Chaturasra = 2^(maxS - 1)
  | otherwise = fromEnum gati * 2^ max (maxS-2) 0

-- | Method to calculate number of beats in a Thala based on its jati
calculateCount :: JatiGati -> Thala -> Int
calculateCount _ (T []) = 0
calculateCount g (T (x:xs)) =
    case x of Laghu -> fromEnum g + calculateCount g (T xs)
              Dhruta -> 2 + calculateCount g (T xs)
              Anudhruta -> 1 + calculateCount g (T xs)

-- | Method to get appropriate symbols for representation
getThalaSplitPoints :: JatiGati -> Thala  -> [String]
getThalaSplitPoints _ (T [])  = []
getThalaSplitPoints j (T (x:xs))  =
    case x of Laghu -> [" | "] ++ replicate (fromEnum j - 1) "^" ++ getThalaSplitPoints j (T xs)
              Dhruta -> " O ":" ^ ": getThalaSplitPoints j (T xs)
              Anudhruta ->" U " : getThalaSplitPoints j (T xs)

-- | Representing Compositions with changing speeds
getRepresentation:: [Comp] -> JatiGati ->Thala ->Int->String
getRepresentation ((K x):y:xs) jati thala pos  =
     let (a,b) = getStringComp y jati thala (K x) pos
     in "<" ++ show (fromEnum x) ++ ">" ++ a ++ getRepresentation xs jati thala b
getRepresentation _ _ _ _ =""

-- | Driver function for getting a virtual representation of a composition, after validation
getStringComp :: Comp->JatiGati->Thala->Comp->Int-> (String, Int)
getStringComp (C k) jati (T thala) (K gati) pos =
    let maxS = maximum $ map snd k
        countPerBeat = getCountPerBeat gati maxS
        b = calculateCount jati (T thala)
        countPerAvarta = countPerBeat * b
        a = convToList k countPerBeat gati
        d =
         --if mod (length a) countPerBeat == 0 then
            let c = replicate (calculateCount jati (T thala)) countPerBeat
                e = getThalaSplitPoints jati (T thala)
                in (finalDisp a (T thala) c pos countPerBeat e, pos + div (mod (length a) countPerAvarta) countPerBeat )
         --else ("Error", 0)
    in d
getStringComp _ _ _ _ _ = ("",0)

-- | Core function in obtaining string from Composition
convToList :: [([Syllable], Int)] -> Int ->JatiGati-> [Syllable]
convToList [] _ _= []
convToList listas maxs g =
   concatMap (\(x,d)-> concatMap (\t ->  t:replicate (div maxs (getCountPerBeat g d) -1) Gdot) x) listas

-- | Final display of a thala in lines with proper subdivisions
finalDisp :: [Syllable] ->Thala -> [Int] -> Int ->Int->[String]-> String
finalDisp s (T thala) arr n cPB e =
    if null s then ""
    else let pos = mod n (length arr)
             c = e!!pos
             b = if pos == length arr - 1 then "||\n" else ""
        in c ++ show (Phr (take cPB s)) ++ b ++ finalDisp (drop (arr !! pos) s) (T
        thala) arr (n+1) cPB e

-- | Define the standard phrases for different lengths
phrase4len :: Int -> [[Syllable]]
phrase4len 1 = [[Tha], [Dhi], [Thom], [Nam]]
phrase4len 2 = [[Tha, Ka], [Ki, Ta], [Dhi, Mi], [Tha, Ri], [Di, Na], [Gi, Na], [Jho, Nu]]
phrase4len 3 = [[Tha, Ki, Ta], [Tha, Di, Mi], [Tha, Tha, Ka], [Dhi, Na, Ka]]
phrase4len 4 = [[Ki,Ta, Tha, Ka], [Tha, Ri, Ki, Ta], [Tha, Ka, Di, Na], [Tha, Ka, Dhi, Mi], [Tha, Ka, Jho, Nu], [Tha, Lan, Gdot, Gu],
                [Gi, Na, Ki, Ta], [Tha, Di, Mi, Tha]]
phrase4len 5 = [[Tha, Di, Gi, Na, Thom], [Tha, Ka, Tha, Ki, Ta], [Tha, Ka, Tha, Di, Mi], [Tha, Dhi, Mi, Tha, Ka],
                 [Dhi, Na, Ka, Dhi, Mi],[Tha, Tha, Ka, Dhi, Mi]]
phrase4len 6 = [Tha, Dhi, Gdot, Gi, Na, Thom] : [ x++ y | x <- phrase4len 2, y <- phrase4len 4] ++
                [ x++ y | x <- phrase4len 4, y <- phrase4len 2] ++[ x++ y | x <- phrase4len 3, y <- phrase4len 3]
phrase4len 7 = [Tha, Gdot, Dhi, Gdot, Gi, Na, Thom]: [ x++ y | x <- phrase4len 3, y <- phrase4len 4] ++ [ x++ y | x <- phrase4len 4, y <- phrase4len 3]
                ++ [ x++ y | x <- phrase4len 2, y <- phrase4len 5] ++ [ x++ y | x <- phrase4len 5, y <- phrase4len 2]
phrase4len 8 = [[Tha, Dhi, Gdot, Gi, Gdot, Na, Gdot, Thom], [Tha, Ka, Tham, Gdot, Tha, Ri, Ki, Ta],[Dhi,Gdot, Gdot, Gdot, Tham, Gdot, Tha, Ka], [Dhi, Ku, Tha, Ri, Ki, Ta, Tha, Ka],[Tha,Gdot, Dhi, Gdot, Tha, Gdot, Ki, Ta]]++ [ x ++ y ++ z | x <- phrase4len 2, y <- phrase4len 3, z <- phrase4len 3]
                 ++ [ x++ y | x <- phrase4len 4, y <- phrase4len 4] ++ [ x++ y ++z| x <- phrase4len 3, y <- phrase4len 2, z<- phrase4len 3]
                 ++ [ x++ y ++ z  | x <- phrase4len 3, y <- phrase4len 3, z<-phrase4len 2]
phrase4len 9 = [Tha, Gdot, Dhi, Gdot, Gi, Gdot, Na, Gdot, Thom]: [ x++ y | x <- phrase4len 4, y <- phrase4len 5] ++
             [ x++ y | x <- phrase4len 5, y <- phrase4len 4] ++ [ x++ y ++ z  | x <- phrase4len 3, y <- phrase4len 3, z<-phrase4len 3]
             ++ [ x++ y ++ z  | x <- phrase4len 2, y <- phrase4len 3, z<-phrase4len 4]
phrase4len 16 = [[Dhi, Gdot,Gdot, Gdot, Tham, Gdot, Tha, Ka, Dhi, Ku, Tha, Ri, Ki, Ta, Tha, Ka],
                    [Tha, Ka, Thom, Gdot, Tham, Gdot, Tha, Ka, Dhi, Ku, Tha, Ri, Ki, Ta, Tha, Ka]]
phrase4len _ =[]

-- | Function to randomly select a phrase of a specified length. Defined recursively for phrases
-- of length greater than 9
phraseGenerator::Int-> StdGen-> ([Syllable], StdGen)
phraseGenerator x gen =
    if x < 10|| x==16 then let (y,z) =randomR (0, length(phrase4len x) - 1) gen
                    in  (phrase4len x !! y, z)
        else let (a, newgen) = randomR (2, x - 2) gen
                 (b, c) = genPhrase4Me a newgen
                 (d, e) = genPhrase4Me (x - a) c
            in if a > x - a then (d++b, e) else (b++d, e)

-- | Function to decide whether the phrase has to be generated with breaks or without them
genPhrase4Me ::Int-> StdGen-> ([Syllable], StdGen)
genPhrase4Me x tossgen =
    if x == 0 then ([], tossgen) else
  let (a,gen) = randomR (1,10) tossgen ::(Int, StdGen)
  in if a <= 8||x == 1
        then phraseGenerator x gen
    else let factor = head (filter (\y->mod x y==0) [2,3..])
             (one, two) = if factor >=4 then let (y, newgen) = randomR (2, x-2) gen
                                                 (b, c) = genPhrase4Me y newgen
                                                 (d, e) = genPhrase4Me (x - y) c
                                        in if y > x - y then (d ++ b, e) else (b ++ d, e)
                        else let (pha, c) = phraseGenerator (div x factor) gen
                                 b = concatMap (\t->t:replicate (factor - 1) Gdot) pha
                            in (b,c)
        in (one, two)

-- | To randomly generate different phrases
genValues :: [Int] -> StdGen -> ([[Syllable]], StdGen)
genValues [] gen = ([], gen)
genValues [x] gen =
    let (a,b) = genPhrase4Me x gen
        in ([a],b)
genValues (x:xs) gen =
    let (a,newgen) = genValues xs gen
        exLength = if null xs then 0 else head xs
        (b, newgen2) = genPhrase4Me (x - exLength) newgen
    in ((b++head a) :a, newgen2)

-- | Define the constant phrase of a Mohra with respect to the given length
mohrad::JatiGati -> [Syllable]
mohrad Tisra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot,Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot]
mohrad Chaturasra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Tha, Lan, Gdot,Gu, Dhin, Gdot, Gdot, Gdot]
mohrad Khanda = [Tha, Lan, Gdot, Gu, Dhin,Gdot,  Thak, Gdot, Dhin, Gdot, Tha, Lan, Gdot, Gu, Dhin, Gdot,Thak, Gdot, Dhin, Gdot]
mohrad Misra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak,Gdot, Tha, Lan, Gdot,Gu, Dhin, Gdot ]
mohrad Sankirna = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Tha, Lan, Gdot, Gu, Dhin, Gdot, Dhin, Gdot, Gdot, Gdot]

-- | Define one of the final constants used in the Mohra
mohraC1::JatiGati -> [Syllable]
mohraC1 Chaturasra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Gdot, Gdot]
mohraC1 Tisra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot]
mohraC1 Khanda = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot]
mohraC1 Misra = [Tha, Lan, Gdot, Gu, Dhin,Gdot, Gdot]
mohraC1 Sankirna = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Gdot, Gdot, Gdot]

-- | Define the constant used to end the Mohra
mohraC2 :: JatiGati -> [Syllable]
mohraC2 Tisra = concat (replicate 3 [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot, Gu, Gdot]) ++ replicate 6 Gdot
mohraC2 Chaturasra =concat (replicate 3 [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot]) ++ replicate 4 Gdot
mohraC2 Khanda = concat( replicate 3 [Tha, Lan, Gdot, Gu, Dhin, Gdot,Thak, Gdot, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gu, Gdot]) ++ [Gdot, Gdot]
mohraC2 Misra = concat (replicate 3 [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot]) ++ replicate 5 Gdot
mohraC2 Sankirna = concat (replicate 3 [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot, Gu, Gdot]) ++ replicate 3 Gdot

-- | To obtain the right speed for generation of Mohra with respect to its gati/ nadai
getMohraSpeed::JatiGati->Int
getMohraSpeed gati
    | gati == Chaturasra = 4
    | gati == Tisra = 4
    | otherwise = 3

-- | To get the appropriate separations for a required mohra generation
getMohraSeparation::Int->JatiGati->[Int]
getMohraSeparation count gati =
    let scnd = length $ mohrad gati
        frst =div (count - 2*scnd) 2
    in [frst, scnd, frst, scnd]

-- | To generate a Mohra on the basis of the jati, thala and gati
genMohra::JatiGati->Thala->JatiGati->StdGen->[Syllable]
genMohra jati thala gati gen=
    let sp = getMohraSpeed gati
        phd = mohrad gati
        overAllCount = if calculateCount jati thala<= 4 then 2*calculateCount jati thala
                        else calculateCount jati thala
        [a,b,_,_] = getMohraSeparation (getCountPerBeat gati sp*overAllCount) gati
        (pha, gen1) = if gati==Chaturasra || gati == Misra || gati == Sankirna|| mod a 2 ==1 then genPhrase4Me a gen else genPhrase4Me (div a 2) gen
        pham = if gati == Chaturasra || gati == Misra || gati == Sankirna|| mod a 2 == 1 then pha else intersperse Gdot pha ++ [Gdot]
        (phb, _ ) = if gati==Chaturasra || gati == Misra || gati == Sankirna || mod a 2 == 1 then genPhrase4Me b gen1 else genPhrase4Me (div b 2) gen1
        phbm = if gati == Chaturasra || gati == Misra || gati == Sankirna || mod a 2 == 1 then phb else intersperse Gdot phb ++ [Gdot]
        c1 = mohraC1 gati
        c2 = mohraC2 gati
        derMohra = concat [pham, phbm, pham, phd, pham, phbm, pham, phd, pham, phbm, pham, c1,pham, c1, pham,c2]
    in derMohra

-- | To read the thala from user input
getThala :: String->Thala
getThala "Dhruva" = dhruva
getThala "Matya" = matya
getThala "Eka" = eka
getThala "Rupaka" = rupaka
getThala "Thriputa" = thriputa
getThala "Atta" = atta
getThala "Jhampe" = jhampe
getThala _ = eka



getGPh :: Int -> [Syllable]
getGPh 0 = []
getGPh 1 = [Dhi]
getGPh 2 = [Dhin, Gdot]
getGPh 3 = [Dhin, Gdot, Gu]
getGPh 4 = [Dhin, Gdot, Dhin, Gdot]
getGPh 5 = getGPh 2 ++ getGPh 3
getGPh 6 = concat $ replicate 2 (getGPh 3)
getGPh 7 = getGPh 4 ++ getGPh 3
getGPh 8 = getGPh 3 ++ getGPh 5
getGPh _ = error "Undesired length"

-- composing a smaller subpattern for a Korvai without the extra gap
getSub:: Int-> StdGen -> ([Syllable], [JustNums])
getSub s gen =
    let vals1 = [([x, x + d..(x + (n -1)*d)],g) | x <- [1,2..(div s 3)], d <- [2,3..8], g<- 0:[2,3..8], n <-[3,4, 5], n*x + (n-1)*g + (div (n*(n-1)) 2) *d ==s]
        vals2 = map (\(a,b) -> (reverse a,b)) vals1
        vals = vals1 ++ vals2
        (pos, gen2 ) = randomR (0, length vals - 1) gen :: (Int, StdGen)
        (phs, gp) = if null vals then ([s],0) else vals !! pos
        (ph1, _, nList)
          | minimum phs > 15  =
             let fl = map (`getSub` gen2) phs
                 fl1 = map fst  fl
                 fl3 = map snd fl
            in (fl1, gen, fl3)
          | phs !! 1 < head phs =let (a,b) = genValues phs gen2 in (a,b,map (\x -> map P [x]) phs)
          | otherwise = let (a, b) = genValues (reverse phs) gen2 in (reverse a, b, map (\x ->map P [x]) phs)
        ph4 = getGPh gp
        gpNums = [G gp | gp /= 0]
        finPh = intercalate ph4 ph1
        finJustNums = intercalate gpNums nList
    in (finPh , finJustNums )

-- | To generate the purvardha for a given length for the Korvai
getPurvardha::Int->StdGen->([Syllable], StdGen, [JustNums])
getPurvardha s gen =
    let vals1 = [([x, x + d..(x + (n -1)*d)],g) | x <- [1,2..(div s 3)], d <- [2,3..8], g<- [2,3..8], n <-[3,4, 5], n*x + n*g + (div (n*(n-1)) 2) *d ==s]
        vals2 = map (\(a,b) -> (reverse a,b)) vals1
        vals3 = [([x,x,x], g) | x<- [1,2..(div s 3)], g<- 0:[2,3..8], 3*x + 3*g == s]
        vals4 = [(map (\k -> x*d^k) [0,1..(n-1)], g) | x <- [1,2..(div s 3)] , d <- [2,3,4,5], g <- [0,2,3,4,5], n <- [3,4,5], x*(div (d^n - 1) (d -1)) + n*g == s ]
        vals = vals1 ++ vals2 ++ vals3 ++ vals4
        (pos, gen2 ) = randomR (0, length vals - 1) gen :: (Int, StdGen)
        (phs, gp) = if null vals then ([s],0) else vals !! pos
        (ph1, newgen, nList)
          | minimum phs > 15  =
             let fl = map (`getSub` gen2) phs
                 fl1 = map fst  fl
                 fl3 = map snd fl
            in (fl1, gen, fl3)
          | phs !! 1 < head phs =let (a,b) = genValues phs gen2 in (a,b,map (\x -> map P [x]) phs)
          | otherwise = let (a, b) = genValues (reverse phs) gen2 in (reverse a, b, map (\x -> map P [x]) phs)
        ph4 = getGPh gp
        gpNums = [G gp | gp /= 0]
        finPh = intercalate ph4 ph1
        finJustNums = intercalate gpNums nList
    in (finPh ++ ph4, newgen, finJustNums ++ gpNums)

-- | To generate an Uttarardha which has a fixed structure
getUttar :: Int-> ([Syllable], [JustNums])
getUttar len =
    let factor = head (filter (\y->mod (len - 2 * y) 3==0 && (len - 2*y) <= 27) (0:[2,3..8]))
        gapPhrase = if factor == 0  then [] else Dhin:replicate (factor - 1) Gdot
        lenphrase = div (len - 2*factor) 3
        mainPhrase = head (phrase4len lenphrase)
    in (mainPhrase ++ gapPhrase ++ mainPhrase ++ gapPhrase ++ mainPhrase, [P lenphrase, G factor, P lenphrase, G factor, P lenphrase])

-- | To generate an Uttarardha which has a varying structure
getUttarVarying :: Int -> ([Syllable], [JustNums])
getUttarVarying len =
    let factor = head (filter (\y->mod (len - 2 * y) 6==0 && ((len - 2*y) <= 54 || mod (len - 2*y) 9 ==0) && (len - 2*y) >= 30) [1,2,3,4,5,6,7,8])
        gapPhrase = if factor == 0  then [] else Dhin:replicate (factor - 1) Gdot
        lenphrase = if (len -2*factor) <= 54 then div (len - 2*factor) 6 else div (len - 2*factor) 9
        mainPhrase = head (phrase4len lenphrase)
    in if len - 2*factor <=54 then (mainPhrase ++ gapPhrase ++ mainPhrase ++ mainPhrase ++ gapPhrase ++ mainPhrase ++ mainPhrase ++ mainPhrase,
                                        [P lenphrase, G factor, P lenphrase, P lenphrase, G factor, P lenphrase, P lenphrase, P lenphrase ])
        else (mainPhrase ++ mainPhrase ++ mainPhrase ++ gapPhrase ++ mainPhrase ++ mainPhrase ++ mainPhrase ++ gapPhrase ++mainPhrase ++ mainPhrase ++ mainPhrase,
            [P lenphrase, P lenphrase, P lenphrase, G factor, P lenphrase, P lenphrase, P lenphrase, G factor, P lenphrase, P lenphrase, P lenphrase ])

-- | Core function to generate a desired Korvai
genKorvai::JatiGati -> Thala -> JatiGati -> StdGen -> ([Syllable], [JustNums])
genKorvai jati thala gati gen=
    let sp = getMohraSpeed gati -1
        avarta = calculateCount jati thala*getCountPerBeat gati sp
        counts = 4*avarta
        overallCount = 2* counts
        (totPur', gen1) = randomR (div (overallCount - 50) 2, div (overallCount - 15) 2) gen :: (Int, StdGen)
        totPur = totPur' * 2
        totUtt = overallCount -  totPur
        (purva, _, nList) =getPurvardha totPur gen1
        (uttara, nList2) = if even totUtt && totUtt >=32 then getUttarVarying totUtt else getUttar totUtt
        in (purva ++ uttara ,  nList ++ nList2)

-- | New datatype for users to input compositions as numbers
data UIComp = Ph [(Int, Int)] | Gp [(Int, Int)] | Tc JatiGati

-- | Method to convert compositions into a format which can be used by getRepresentation
concatPhGp :: [Comp] -> [Comp]
concatPhGp ((C a):(C b):t) = concatPhGp (C (a ++ b) : t)
concatPhGp (x: t) = x: concatPhGp t
concatPhGp [] = []

-- | Method to generate phrases for a composition
genComp::[UIComp] -> JatiGati -> Thala ->StdGen -> [Comp]
genComp ((Tc x):y ) jati thala gen =K x:genComp y jati thala gen
genComp ((Ph t:y)) jati thala gen =
    let b = foldl (\(acc, gen1) (p,q) -> ((fst (genPhrase4Me p gen1),q):acc, snd(genPhrase4Me p gen1))) ([], gen) t
        a = fst b
        in C a:genComp y jati thala (snd b)
genComp ((Gp t: y)) jati thala gen =
    let a  = map(\(p,q) -> (Dhi:replicate (p - 1) Gdot,q)) t
        in C a: genComp y jati thala gen
genComp [] _ _ _  = []

-- | Helper function to check for substring
findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

-- | Core function which validates a string and prints if valid
compValidator :: [UIComp] -> JatiGati -> Thala -> StdGen -> String
compValidator comp jati thala gen =
    let actcomp = concatPhGp $ genComp comp jati thala gen
        stringedcomp = getRepresentation actcomp jati thala 0
        valComp = if findString "Error" stringedcomp == Nothing then stringedcomp else "Error"
    in valComp

-- | Datatype for users to enter values for Korvais
data JustNums = P Int | G Int| B deriving(Show)

-- | Simplify Korvais to the maximum extent
groupPhs::[JustNums] -> [JustNums]
groupPhs [] = []
groupPhs (P a:P b:xs) = groupPhs (P (a+b): xs)
groupPhs (G a:G b: xs) = groupPhs ( G (a + b) : xs)
groupPhs (G a:B : xs) = groupPhs(G a:xs)
groupPhs (x:xs) = x:groupPhs xs

-- | To convert a simplified Korvai to its numerical equivalent
getNums::[JustNums]->[Int]
getNums [] = []
getNums ((P a):xs) = a:getNums xs
getNums ((G a) : xs) = a:getNums xs
getNums (B:xs) = 0:getNums xs

-- | Validate the second half of the Korvai
validateUttar :: [Int]  -> Bool
validateUttar arr =
    let a = head arr
        b = arr !! 2
        c = arr !! 4
    in (arr!! 1 == arr !!3) && ((c - b) == (b -  a)) && (a<=10 || mod a 3 ==0) && (b<=10 || mod b 3 == 0) && (c <= 10 || mod c 3 ==0)

-- | Validate the overall Korvai
validateKorvai :: [JustNums] -> JatiGati -> Thala -> JatiGati-> Bool
validateKorvai arr jati thala gati =
    let narr = getNums $ groupPhs arr
        totsum = sum narr
        nrev = reverse narr
        bool1 = validateUttar nrev
        npur = reverse $ drop 5 nrev
        pursum = sum npur
        vals1 = [([x, x + d..(x + (n -1)*d)],g) | x <- [1,2..(div pursum 3)], d <- [2..(div pursum 4)], g<- [2,3,4,5], n <-[3,4, 5], n*x + n*g + (div (n*(n-1)) 2) *d == pursum]
        vals2 = map (\(a,b) -> (reverse a,b)) vals1
        vals3 = [([x,x,x], g) | x<- [1,2..(div pursum 3)], g<-[0,2,3,4,5], 3*x + 3*g == pursum]
        vals4 = [(map (\k -> x*d^k) [0,1..(n-1)], g) | x <- [1,2..(div pursum 3)] , d <- [2,3,4,5], g <- [0,2,3,4,5], n <- [3,4,5], x*(div (d^n - 1) (d -1)) + n*g == pursum ]
        vals = map (\(l, g) -> intersperse g l ++ [g]) (vals1 ++ vals2 ++ vals3 ++ vals4)
        bool2 = elem npur vals
        sp = if gati ==Tisra then getMohraSpeed gati -1 else getMohraSpeed gati
        avarta = calculateCount jati thala*getCountPerBeat gati sp
        bool3 = mod totsum avarta == 0
    in bool1 && bool2 && bool3

main :: IO()
main = do
    value1 <- getLine
    value2 <- getLine
    value3 <- getLine
    value4 <- getLine
    gen <- getStdGen
    let
        x = (read value1::JatiGati)
        y = getThala value2
        z = (read value3:: JatiGati)
        ch = (read value4 :: Int)
        w =  if ch==1 then getRepresentation [K z, C [(fst (genKorvai x y z gen), getMohraSpeed z - 1)]] x y 0
        else getRepresentation [K z, C [(genMohra x y z gen, getMohraSpeed z)]] x y 0
    putStrLn w


-- | Representing Compositions with changing speeds
getRT:: [Comp] -> JatiGati ->Thala ->Int->String
getRT ((K x):y:xs) jati thala pos  =
     let (a,b) = getSCT y jati thala (K x) pos
     in a ++ getRT xs jati thala b
getRT _ _ _ _ =""

-- | Driver function for getting a virtual representation of a composition, after validation
getSCT :: Comp->JatiGati->Thala->Comp->Int-> (String, Int)
getSCT (C k) jati (T thala) (K gati) pos =
    let maxS = maximum $ map snd k
        countPerBeat = getCountPerBeat gati maxS
        b = calculateCount jati (T thala)
        countPerAvarta = countPerBeat * b
        a = convToList k countPerBeat gati
        d =
         --if mod (length a) countPerBeat == 0 then
            let c = replicate (calculateCount jati (T thala)) countPerBeat
                e = getThalaSplitPoints jati (T thala)
                in (finalDT a (T thala) c pos countPerBeat e, pos + div (mod (length a) countPerAvarta) countPerBeat )
         --else ("Error", 0)
    in d
getSCT _ _ _ _ _ = ("",0)

-- | Final display of a thala in lines with proper subdivisions
finalDT :: [Syllable] ->Thala -> [Int] -> Int ->Int->[String]-> String
finalDT s (T thala) arr n cPB e =
    if null s then ""
    else let pos = mod n (length arr)
        in "[" ++ showT (take cPB s) ++"] " ++ finalDT (drop (arr !! pos) s) (T thala) arr (n+1) cPB e

-- | Function which returns compositions in mini-notation
showT::[Syllable] -> String
showT [] = ""
showT (Gdot:xs) = "~ " ++ showT xs
showT (Tha:xs) = "tha " ++ showT xs
showT (Ki:xs) = "ki " ++ showT xs
showT (Ta:xs) = "ta " ++ showT xs
showT (Di:xs) = "dhi " ++ showT xs
showT (Dhi:xs) = "dhi " ++ showT xs
showT (Gi:xs) = "ki " ++ showT xs
showT (Jho:xs) = "thom " ++ showT xs
showT (Na:xs) = "na " ++ showT xs
showT (Thom:xs) = "thom " ++ showT xs
showT (Lan:xs) = "c " ++ showT xs
showT (Gu:xs) = "gumki " ++ showT xs
showT (Dhin:xs) = "dhin " ++ showT xs
showT (Ku:xs) = "ac " ++ showT xs
showT (Ri:xs) = "c " ++ showT xs
showT (Ka:xs) = "ka " ++ showT xs
showT (Tham:xs) = "ac " ++ showT xs
showT (Thak:xs) = "c " ++ showT xs
showT (Dhim:xs) = "dhin " ++ showT xs
showT (Nam:xs) = "nam " ++ showT xs
showT (Mi:xs) = "c " ++ showT xs
showT (Nu:xs) = "ac " ++ showT xs

-- | Function to return a desired Korvai in mini-notation
tidalK :: JatiGati -> Thala -> JatiGati -> StdGen ->ControlPattern
tidalK jati thala gati gen =
    let x = getRT [K gati, C[(fst(genKorvai jati thala gati gen), getMohraSpeed gati - 1)]] jati thala 0
        y = (length.filter (=='[') ) x
    in (slow (fromInteger $ toInteger y) $ sound (fromString x))

-- | Function to return a desired Mohra in mini-notation
tidalM :: JatiGati -> Thala -> JatiGati -> StdGen -> ControlPattern
tidalM jati thala gati gen =
    let x = getRT [K gati, C [(genMohra jati thala gati gen, getMohraSpeed gati - 1)]] jati thala 0
        y = (length.filter (=='[') ) x
    in (slow (fromInteger $ toInteger y) $ sound (fromString x))

-- Method to convert a composition into a pattern Sequence
-- Converting getRT would be pretty easy I guess in this regard

-- | Representing Compositions with changing speeds as lists of lists of syllables
getSeqRep:: [Comp] -> JatiGati ->Thala ->Int->[[Syllable]]
getSeqRep ((K x):y:xs) jati thala pos  =
     let (a,b) = getSCTSeq y jati thala (K x) pos
     in a ++ getSeqRep xs jati thala b
getSeqRep _ _ _ _ =[]

-- | Driver function for getting a list representation of a composition, after validation
getSCTSeq :: Comp->JatiGati->Thala->Comp->Int-> ([[Syllable]], Int)
getSCTSeq (C k) jati (T thala) (K gati) pos =
    let maxS = maximum $ map snd k
        countPerBeat = getCountPerBeat gati maxS
        b = calculateCount jati (T thala)
        countPerAvarta = countPerBeat * b
        a = convToList k countPerBeat gati
        d =
         --if mod (length a) countPerBeat == 0 then
            let c = replicate (calculateCount jati (T thala)) countPerBeat
                in (finalDTSeq a (T thala) c pos countPerBeat, pos + div (mod (length a) countPerAvarta) countPerBeat )
         --else ("Error", 0)
    in d
getSCTSeq _ _ _ _ _ = ([[]],0)

-- | Final display of a composition as a list of lists of syllables with proper subdivisions
finalDTSeq :: [Syllable] ->Thala -> [Int] -> Int ->Int-> [[Syllable]]
finalDTSeq s (T thala) arr n cPB=
    if null s then [[]]
    else let pos = mod n (length arr)
        in  take cPB s : finalDTSeq (drop (arr !! pos) s) (T thala) arr (n+1) cPB

-- | Function which takes a list of list of syllables and converts each list to a Sequence 
-- getSeqfromKon :: [[Syllable]] -> Rational  -> [Sequence Syllable]
-- getSeqfromKon [] _  = []
-- getSeqfromKon (x:xs) durat =
--     let y = durat / realToFrac (length x)
--         a = map (\t -> if t == Gdot then Gap y else Atom y t) x
--     in Sequence a : getSeqfromKon xs durat

-- -- | Function to return a desired Korvai in Sequence notation
-- sequenceK :: JatiGati -> Thala -> JatiGati -> StdGen ->Sequence Syllable
-- sequenceK jati thala gati gen =
--     let x = getSeqRep [K gati, C[(fst(genKorvai jati thala gati gen), getMohraSpeed gati - 1)]] jati thala 0
--         y = getSeqfromKon x 1
--     in Sequence.unwrap $ Sequence y

-- -- | Function to return a desired Mohra in Sequence notation
-- sequenceM :: JatiGati -> Thala -> JatiGati -> StdGen -> Sequence Syllable
-- sequenceM jati thala gati gen =
--     let x = getSeqRep [K gati, C [(genMohra jati thala gati gen, getMohraSpeed gati - 1)]] jati thala 0
--         y = getSeqfromKon x 1
--     in Sequence.unwrap $ Sequence y


-- Mridangam samples (c) Arthur Carabott, distributed under a CC-BY-SA license https://creativecommons.org/licenses/by-sa/4.0/

-- | Representing Compositions with changing speeds
getRTNum:: [Comp] -> JatiGati ->Thala ->Int->String
getRTNum ((K x):y:xs) jati thala pos  =
     let (a,b) = getSCTNum y jati thala (K x) pos
     in a ++ getRTNum xs jati thala b
getRTNum _ _ _ _ =""

-- | Driver function for getting a virtual representation of a composition, after validation
getSCTNum :: Comp->JatiGati->Thala->Comp->Int-> (String, Int)
getSCTNum (C k) jati (T thala) (K gati) pos =
    let maxS = maximum $ map snd k
        countPerBeat = getCountPerBeat gati maxS
        b = calculateCount jati (T thala)
        countPerAvarta = countPerBeat * b
        a = convToList k countPerBeat gati
        d =
         --if mod (length a) countPerBeat == 0 then
            let c = replicate (calculateCount jati (T thala)) countPerBeat
                e = getThalaSplitPoints jati (T thala)
                in (finalDTNum a (T thala) c pos countPerBeat e, pos + div (mod (length a) countPerAvarta) countPerBeat )
         --else ("Error", 0)
    in d
getSCTNum _ _ _ _ _ = ("",0)



-- | Final display of a thala in lines with proper subdivisions
finalDTNum :: [Syllable] ->Thala -> [Int] -> Int ->Int->[String]-> String
finalDTNum s (T thala) arr n cPB e =
    if null s then ""
    else let pos = mod n (length arr)
        in "[" ++ showNum (take cPB s) ++"] " ++ finalDTNum (drop (arr !! pos) s) (T thala) arr (n+1) cPB e

showNum :: [Syllable] -> String
showNum [] = ""
showNum (x:xs) = show (fromEnum x) ++ " " ++ showNum xs

-- | Function to return a desired Korvai in mini-notation
numK :: JatiGati -> Thala -> JatiGati -> StdGen ->Pattern String ->ControlPattern
numK jati thala gati gen sc=
    let x = getRTNum [K gati, C[(fst(genKorvai jati thala gati gen), getMohraSpeed gati - 1)]] jati thala 0
        y = (length.filter (=='[') ) x
    in note (scale sc $ slow (fromInteger $ toInteger y) ( fromString x))

-- | Function to return a desired Mohra in mini-notation
numM :: JatiGati -> Thala -> JatiGati -> StdGen ->Pattern String -> ControlPattern
numM jati thala gati gen sc =
    let x = getRTNum [K gati, C [(genMohra jati thala gati gen, getMohraSpeed gati - 1)]] jati thala 0
        y = (length.filter (=='[') ) x
    in note (scale sc $ slow (fromInteger $ toInteger y) ( fromString x))


