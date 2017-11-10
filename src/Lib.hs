module Lib (adrenalina) where

import Euterpea

toPitches :: [AbsPitch] -> [Pitch]
toPitches as = map pitch as

toNotes :: Dur -> [AbsPitch] -> Music Pitch
toNotes d ns = line $ map (note d) (toPitches ns)

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns  =  let f n = n d
                in line $ map f ns

repeated :: Int -> [Int] -> [Int]
repeated n xs = concatMap (replicate n) xs

chordRhythms :: [Dur] -> [Dur -> Music Pitch] -> Music Pitch
chordRhythms [] ns = rest 0
chordRhythms (d:ds) ns = let f n = n d
                         in (chord (map f ns)) :+: chordRhythms ds ns

addInterval :: Int -> Music Pitch -> Music Pitch
addInterval x ns = ns :=: transpose x ns

-- MELODY
m0 = note 2 (A,3) :=: addDur (1/2) [e 4, f 4, fs 4, f 4]
m1 = (toNotes sn $ repeated 4 [45..68]) :+: note 2 (A,5)

m21 = [e 4 qn, d 4 en, c 4 en, c 4 en, c 4 sn, d 4 sn,
     e 4 en, g 3 en, c 4 qn, bf 3 en, a 3 en, hnr]
m22 = [e 5 en, a 4 en, e 5 en, g 4 en,
     c 5 qn, bf 4 en, a 4 en, c 5 qn, bf 4 en, a 4 en, hnr]
m23 = [c 5 en, c 5 sn, d 5 sn, e 5 en, g 4 en, c 5 qn, bf 4 en, a 4 en,
     c 5 qn, bf 4 en, a 4 en, enr, g 4 sn, a 4 sn, d 5 en, a 4 en,
     bf 4 en, bf 4 en, bf 4 en, a 4 en, enr, a 4 en, c 5 en, a 4 en,
     bf 4 en, a 4 en, a 4 en, g 4 en, bf 4 en, a 4 sn, g 4 sn, a 4 qn]
m2a = line $ m21 ++ m22 ++ m23
m2b = (transpose 12 (line m21)) :+: (line $ m22 ++ m23)
m2 = addInterval 12 $ m2a :+: m2b

m3 = line [enr, d 4 en, fs 4 en, g 4 en, a 4 en, a 4 sn, b 4 sn, c 5 en, g 4 en,
      c 5 en, g 4 en, d 5 en, e 5 en, e 5 en, c 5 en, qnr,
      c 5 qn, d 5 qn, d 5 en, c 5 en, enr,
      a 4 en, bf 4 en, a 4 en, a 4 en, g 4 en, bf 4 en, a 4 sn, g 4 sn, a 4 qn]

m4a = line [e 5 qn, d 5 en, c 5 en, e 5 en, d 5 sn, c 5 sn, d 5 en, a 4 en,
       d 5 qn, c 5 en, b 4 en, d 5 en, c 5 sn, b 4 sn, c 5 en, g 4 en,
       c 5 qn, bf 4 en, a 4 en, c 5 en, bf 4 qn, a 4 en,
       bf 4 qn, a 4 en, g 4 en, bf 4 en, a 4 sn, g 4 sn, a 4 qn]
m4b = line [qnr, a 4 qn, qnr, a 4 qn,
       qnr, a 4 qn, a 3 (1/12), c 4 (1/12), e 4 (1/12), a 4 qn]
m4 = addInterval 12 $ m4a :+: m4b

melodyLine = times 4 m0 :+: m1 :+: m2 :+: times 4 m3 :+: times 2 m4
melody = addInterval (-12) $ instrument Accordion melodyLine

-- HARMONY
aMinor = [a 3, c 4, e 4, a 4]
aMajor = [a 3, cs 4, e 4, a 4]
aOpen = [a 3, e 4, a 4]
dMajor = [d 3, fs 3, a 3, d 4]
dOpen = [d 3, a 3, d 4]
h1 = chordRhythms [dqn, dqn, qn] aOpen
h2 = chordRhythms [dqn, dqn, qn] dOpen
harmonyLine = times 8 h1 :+: rest 8 :+: times 16 h1 :+: times 4 (times 3 h2 :+: h1)
harmony = instrument RhodesPiano harmonyLine

-- RHYTHM
hit1 = perc LowTom en
hit2 = perc ClosedHiHat en :=: perc LowTom en
hit2a = perc ClosedHiHat (1/12) :=: perc LowTom (1/12)
hit3 = perc BassDrum1 qn
rbass = hit3 :+: qnr :+: hit3 :+: qnr
r0 = times 4 hit3 :=: hit1 :+: qnr :+: hit1 :+: qnr :+: hit1 :+: hit1 :+:
     enr :+: hit1 :+: enr :+: hit1 :+: qnr :+: hit1 :+: hit1
r1 = offset sn (perc LowTom 6) :+: hit2 :+: rest (7/8)
     :+: offset sn (perc ElectricSnare (9/16)) :+: rest (7/16)
r2 = hit2 :+: qnr :+: hit2 :+: hit1 :+: enr :+: hit1 :+: enr
r3a = hit2 :+: qnr :+: hit2 :+: qnr :+: hit2 :+: enr
r3b = qnr :+: hit2 :+: dqnr :+: hit2 :+: dqnr :+:
      hit2 :+: enr :+: hit2a :+: hit2a :+: hit2a :+: hit2 :+: enr
r3 = times 6 rbass :=: (times 4 r3a :+: r3b)

rhythmLine = times 4 r0 :+: r1 :+: times 32 r2 :+: times 2 r3
rhythm = instrument Percussion $ rhythmLine

t = 1.4 -- tempo scaling
adrenalina :: Music Pitch
adrenalina = tempo t $ times 2 $ melody :=: rhythm :=: harmony
