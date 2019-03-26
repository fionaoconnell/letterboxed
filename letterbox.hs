module Letterbox where

import Data.Char
import Data.List
import Data.Ord

type Side = (Char, Char, Char)

type Adjlist = [(Char, [Char])]

type Box = (Side, Side, Side, Side)

boxable :: String -> String -> Maybe String
boxable word1 word2
  | word1 == [] || word2 == [] = Nothing
  | length letters /= 12 = Nothing
  | length (group word1) /= length word1 = Nothing
  | length (group word2) /= length word2 = Nothing
  | head word1 == last word2 = Just (word2 ++ tail word1)
  | head word2 == last word1 = Just (word1 ++ tail word2)
  | otherwise = Nothing
      where letters = nub $ word1 ++ word2

getadj :: (Eq a) => [a] -> a -> (a, [a])
getadj letters v =  (v, nub $ map (letters !!) $ predind ++ succind)
  where vinds = elemIndices v letters
        predind = filter (0<=) $ map (subtract 1) vinds
        succind = filter (< length letters) $ map (+ 1) vinds

makeadjlist :: String -> Adjlist
makeadjlist answer = map (getadj answer) (nub answer)


sameside :: (Eq a) => (a, [a]) -> (a, [a]) -> Bool
sameside (v1, adj1) (v2, adj2) = not $ elem v2 adj1 || v1 == v2

-- this feels like a C solution, not a Haskell one. (0,0,0) is hacky and inelegant
-- Unfilled spots on a side are represented by '0'
getside :: (Side, Adjlist) -> (Side, Adjlist)
getside ((a, b, c), adjlist)
  | a == '0' = getside ((fst first, '0', '0'), sideoptions)
  | a /= '0' && b == '0' = getside ((a, fst first, '0'), sideoptions)
  | a /= '0' && b /= '0' && c == '0' = ((a, b, fst first), sideoptions)
  | otherwise = ((a, b, c), adjlist)
      where first = sidelast adjlist
            sideoptions = filter (sameside first) adjlist

sidelast :: Adjlist -> (Char, [Char])
sidelast [] = ('*', "") -- another C ish hacky solution, there must be a better way
sidelast adjlist = last $ sortBy (comparing (length.snd)) adjlist

startside :: Adjlist -> (Char, [Char]) -> Side
startside adjlist starter = fst $ getside ((fst starter, '0', '0'), filter (sameside starter) adjlist)

newside :: Adjlist -> Side
newside adjlist = fst $ getside (('0', '0', '0'), adjlist)

subside :: Side -> Adjlist -> Adjlist
subside (a, b, c) adjlist = deleteadj c $ deleteadj b $ deleteadj a adjlist

deleteadj :: Char -> Adjlist -> Adjlist
deleteadj c [] = []
deleteadj c (adj:adjs)
  | fst adj == c = deleteadj c adjs
  | otherwise = [adj] ++ deleteadj c adjs

tobox :: Adjlist -> (Char, [Char]) -> Box
tobox adjlist starter = (top, right, bottom, left)
  where top = startside adjlist starter
        right = newside $ subside top adjlist
        bottom = newside $ subside right $ subside top adjlist
        left = newside $ subside bottom $ subside right $ subside top adjlist

isbox :: Box -> Bool
isbox (_, _, _, (_,_,'*')) = False
isbox _ = True

boxes :: Adjlist -> [Box]
boxes adjlist = filter isbox $ map (tobox adjlist) adjlist

makeboxes :: String -> String -> Maybe [Box]
makeboxes word1 word2 = boxes <$> adjlist
  where answer = boxable word1 word2
        adjlist = makeadjlist <$> answer
