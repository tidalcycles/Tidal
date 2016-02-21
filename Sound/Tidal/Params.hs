module Sound.Tidal.Params where

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import qualified Data.Map as Map
import Sound.Tidal.Utils
import Control.Applicative

make' :: (a -> Value) -> Param -> Pattern a -> ParamPattern
make' toValue par p = fmap (\x -> Map.singleton par (defaultV x)) p
  where defaultV a = Just $ toValue a

grp :: [Param] -> Pattern String -> ParamPattern
grp [] _ = silence
grp params p = (fmap lookupPattern p)
  where lookupPattern :: String -> ParamMap
        lookupPattern s = Map.fromList $ map (\(param,s') -> toPV param s') $ zip params $ (split s)
        split s = wordsBy (==':') s
        toPV :: Param -> String -> (Param, Maybe Value)
        toPV param@(S _ _) s = (param, (Just $ VS s))
        toPV param@(F _ _) s = (param, (Just $ VF $ read s))
        toPV param@(I _ _) s = (param, (Just $ VI $ read s))

sound :: Pattern String -> ParamPattern
sound = grp [s_p, n_p]

pF name defaultV = (make' VF param, param)
  where param = F name defaultV
pI name defaultV = (make' VI param, param)
  where param = I name defaultV
pS name defaultV = (make' VS param, param)
  where param = S name defaultV

(accelerate, accelerate_p)       = pF "accelerate" (Just 0)
(bandf, bandf_p)                 = pF "bandf" (Just 0)
(bandq, bandq_p)                 = pF "bandq" (Just 0)
(begin, begin_p)                 = pF "begin" (Just 0)
(channel, channel_p)             = pI "channel" Nothing
(clhatdecay, clhatdecay_p)       = pF "clhatdecay" (Just 0)
(coarse, coarse_p)               = pI "coarse" (Just 0)
(crush, crush_p)                 = pF "crush" (Just 0)
(cut, cut_p)                     = pI "cut" (Just 0)
(cutoff, cutoff_p)               = pF "cutoff" (Just 0)
(cutoffegint, cutoffegint_p)     = pF "cutoffegint" (Just 0)
(delay, delay_p)                 = pF "delay" (Just 0)
(delayfeedback, delayfeedback_p) = pF "delayfeedback" (Just 0)
(delaytime, delaytime_p)         = pF "delaytime" (Just 0)
(detune, detune_p)               = pF "detune" (Just 0)
(dry, dry_p)                     = pF "dry" (Just 0)
(end, end_p)                     = pF "end" (Just 0)
(gain, gain_p)                   = pF "gain" (Just 0)
(gate, gate_p)                   = pF "gate" (Just 0)
(hatgrain, hatgrain_p)           = pF "hatgrain" (Just 0)
(hcutoff, hcutoff_p)             = pF "hcutoff" (Just 0)
(hresonance, hresonance_p)       = pF "hresonance" (Just 0)
(kriole, kriole_p)               = pI "kriole" (Just 0)
(lagogo, lagogo_p)               = pF "lagogo" (Just 0)
(lclap, lclap_p)                 = pF "lclap" (Just 0)
(lclaves, lclaves_p)             = pF "lclaves" (Just 0)
(lclhat, lclhat_p)               = pF "lclhat" (Just 0)
(lcrash, lcrash_p)               = pF "lcrash" (Just 0)
(lfocutoffint, lfocutoffint_p)   = pF "lfocutoffint" (Just 0)
(lfoint, lfoint_p)               = pF "lfoint" (Just 0)
(lfopitchint, lfopitchint_p)     = pF "lfopitchint" (Just 0)
(lhitom, lhitom_p)               = pF "lhitom" (Just 0)
(lkick, lkick_p)                 = pF "lkick" (Just 0)
(llotom, llotom_p)               = pF "llotom" (Just 0)
(loop, loop_p)                   = pI "loop" (Just 1)
(lophat, lophat_p)               = pF "lophat" (Just 0)
(lsnare, lsnare_p)               = pF "lsnare" (Just 0)
(n, n_p)                         = pI "n" (Just 0)
(nudge, nudge_p)                 = pF "nudge" (Just 0)
(offset, offset_p)               = pF "offset" (Just 0)
(ophatdecay, ophatdecay_p)       = pF "ophatdecay" (Just 0)
(orbit, orbit_p)                 = pI "orbit" (Just 0)
(pan, pan_p)                     = pF "pan" (Just 0)
(pitch1, pitch1_p)               = pF "pitch1" (Just 0)
(pitch2, pitch2_p)               = pF "pitch2" (Just 0)
(pitch3, pitch3_p)               = pF "pitch3" (Just 0)
(portamento, portamento_p)       = pF "portamento" (Just 0)
(resonance, resonance_p)         = pF "resonance" (Just 0)
(room, room_p)                   = pF "room" Nothing
(sagogo, sagogo_p)               = pF "sagogo" (Just 0)
(sclap, sclap_p)                 = pF "sclap" (Just 0)
(sclaves, sclaves_p)             = pF "sclaves" (Just 0)
(scrash, scrash_p)               = pF "scrash" (Just 0)
(shape, shape_p)                 = pF "shape" (Just 0)
(size, size_p)                   = pF "size" Nothing
(slide, slide_p)                 = pF "slide" (Just 0)
(speed, speed_p)                 = pF "speed" (Just 0)
(s, s_p)                         = pF "s" (Just 0)
(stutterdepth, stutterdepth_p)   = pF "stutterdepth" (Just 0)
(stuttertime, stuttertime_p)     = pF "stuttertime" (Just 0)
(sustain, sustain_p)             = pF "sustain" (Just 0)
(tomdecay, tomdecay_p)           = pF "tomdecay" (Just 0)
(unit, unit_p)                   = pF "unit" (Just 0)
(velocity, velocity_p)           = pF "velocity" (Just 0)
(vcfegint, vcfegint_p)           = pF "vcfegint" (Just 0)
(vcoegint, vcoegint_p)           = pF "vcoegint" (Just 0)
(voice, voice_p)                 = pF "voice" (Just 0)
(vowel, vowel_p)                 = pS "vowel" (Just "")


-- short names
chdecay = clhatdecay
ctf  = cutoff
ctfg = cutoffegint
delayfb = delayfeedback
delayt  = delaytime
det  = detune
gat = gate_p
hg = hatgrain
lag = lagogo
lbd = lkick
lch = lclhat
lcl = lclaves
lcp = lclap
lcr = lcrash
lfoc = lfocutoffint
lfoi = lfoint
lfop = lfopitchint
lht = lhitom
llt = llotom
loh = lophat
lsn = lsnare
ohdecay = ophatdecay
pit1 = pitch1
pit2 = pitch2
pit3 = pitch3
por = portamento
sag = sagogo
scl = sclaves
scp = sclap
scr = scrash
sld = slide
std = stutterdepth
stt = stuttertime
sus  = sustain
tdecay = tomdecay
vcf  = vcfegint
vco  = vcoegint
voi  = voice


drum = n . (drumN <$>)

drumN :: String -> Int
drumN "bd"  = 36
drumN "sn"  = 38
drumN "lt"  = 43
drumN "ht"  = 50
drumN "ch"  = 42
drumN "oh"  = 46
drumN "cp"  = 39
drumN "cl"  = 75
drumN "ag"  = 67
drumN "cr"  = 49
drumN _ = 0
