module Main where

import           Control.Applicative ((<|>))
import           Data.Char           (isDigit)
import qualified Data.Map.Strict     as SM
import           Data.Maybe          (fromJust)


type Item = Char
type Menu = [Item]

data Change = Remove Int
            | Insert Int Item
            | Replace Int Item
            deriving Show

type Experiment = (Menu, Change)
type ExperimentResult = Int


weightOfChange :: ((Item, Item), (Item, Item)) -> Int
weightOfChange (p1, p2)
  | p1 == p2  = 0
  | otherwise = fromJust $ try (p1, p2) <|> try (p2, p1) <|> pure 100
  where
    try = (`SM.lookup` rules)
    rules = foldr insertGroup SM.empty weights
      where
        insertGroup (w, pairs) m = foldr ((`SM.insert` w) . asKey) m pairs
        asKey (l1:r1:_:l2:r2:_) = ((l1, r1), (l2, r2))
        asKey _                 = undefined  -- for hlint specially

    weights = [(0, ["КЛ-КХ"
                   ,"ЛК-ЛП"
                   ,"ЛЛ-ЛХ"
                   ,"ПК-ПП"])
              ,(5, ["ХК-ХЛ"
                   ,"ПК-КП"
                   ,"ПП-КП"
                   ,"КК-ЛЛ"
                   ,"КК-ЛХ"])
              ,(25,["ПЛ-ПХ"
                   ,"ПЛ-КЛ"
                   ,"ПЛ-КХ"
                   ,"ПХ-КЛ"
                   ,"ПХ-КХ"
                   ,"ХП-ПХ"
                   ,"ХП-ПЛ"
                   ,"ХП-КЛ"
                   ,"ХП-КХ"])]


main :: IO ()
main = interact (unlines
                 . map (showResult . calculate . readExperiment)
                 . lines)


readExperiment :: String -> Experiment
readExperiment s = (menu, change)
  where
    change = parseChange rawChange

    (menu, rawChange) =
      let (xs, ys) = break     (== ':')      s
          y        = dropWhile (`elem` ": ") ys
      in  (xs, y)

    parseChange raw =
      case action of
        "удл" -> Remove  idx
        "вст" -> Insert  idx itm
        "изм" -> Replace idx itm
        _     -> error   "Unknown action!"
      where
        idx            = read idx'
        itm            = head itm'
        (action, itm') = splitAt 3 raw'
        (idx', raw')   = span isDigit raw


calculate :: Experiment -> ExperimentResult
calculate (menu, act) =
  sum
  $ zipWith (curry weightOfChange) (prepare menu) (prepare changedMenu)
  where
    prepare = fillAfter stopPair . pairwise

    pairwise []         = []
    pairwise [x]        = pairwise [x, 'Х']
    pairwise (x:y:rest) = (x, y) : pairwise rest

    changedMenu =
      case act of
        Remove  i   -> modifyAt i (const [])
        Insert  i x -> modifyAt i (\y -> [y, x])
        Replace i x -> modifyAt i (const [x])
      where
        modifyAt i f = let pos = i - 1
                           (prefix, x:suffix) = splitAt pos menu
                       in  prefix ++ f x ++ suffix

    fillAfter x = go False
      where
        go _ []         = []
        go f (y:ys)
          | f || y == x = x : go True  ys
          | otherwise   = y : go False ys

    stopPair = ('Х', 'Х')


showResult :: ExperimentResult -> String
showResult = show
