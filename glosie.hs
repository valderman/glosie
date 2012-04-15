{-# OPTIONS_GHC -F -pgmF she #-}
module Main where
import Control.Applicative
import Haste
import Haste.JSON
import Haste.Reactive
import FRP.Fursuit.Async (async)

mkDict :: String -> [(String, String)]
mkDict = map (\str -> let (v,k) = span (/=':') str in (drop 1 k, v)) . lines

mkOpts :: JSON -> String
mkOpts (Arr opts) =
  title ++ concat (map toOpt opts)
  where
    title = "<option disabled>Choose a dictionary</option>"
    toOpt (Str o) =
      "<option value=\"" ++ toStr o ++ ".lst\">" ++ toStr o ++ "</option>"

main = do
  (pstart, start) <- pipe ()
  (pnewQ, newQ) <- pipe ()
  dictList <- jsonSig (pure "api.cgi") ([] <$ start)
  dictName <- valueOf "dictList"
  answer <- valueOf "answer"
  let dictpath = ("dicts/"++) <$> (dictName `union` ("hiragana.lst" <$ start))
  dict <- fmap mkDict <$> ajaxSig dictpath (pure [])

  qaIndex <- async
               $   (\d p -> randomIO (0, length d-1) >>= write p)
               <$> dict
               <*  newQ
  let qAndA = (| dict !! qaIndex |)
      isGuess = (False <$ qAndA) `union` (True <$ answer)
      correct = fmap fst
              $ filterS snd
              $ zipS (| (snd <$> qAndA) == answer |) isGuess

      resetTries = (| correct || (not <$> isGuess) |)
      triesLeft = filterS (>= 0) $ accumS 3 (updateTries <$> resetTries)
      updateTries True = const (3 :: Int)
      updateTries _    = subtract 1

      problems = accumS [] (| addProblem qAndA triesLeft isGuess answer |)
      addProblem _ 3 _ _ =
        id
      addProblem (q,a) 2 True (_:_) = \xs ->
        (q,a,"correctAfterFirstGuess") : xs
      addProblem (q,a) n True ans = \xs ->
        case xs of
          (pq,_,_):xs' | pq == q -> (q,a,"wrong") : xs'
          _                      -> (q,a,"wrong") : xs
      showNewProblem = (True <$ qAndA) `union` (False <$ answer)
      visibleProbs = fmap fst $ filterS snd $ zipS problems showNewProblem

  domObj "dictList.innerHTML" << mkOpts <$> dictList
  domObj "question.innerHTML" << fst <$> qAndA
  domObj "hint.innerHTML" << formatTries <$> triesLeft
  domObj "answer.value" << "" <$ answer
  domObj "problems.innerHTML" << showList <$> visibleProbs
  pnewQ << () <$ filterS id correct
  write pstart ()
  where
    formatTries n = if n == 3 then "" else show n ++ " tries left"
    showList = concat
             . map
                (\(k,v,c) -> "<div class=\""++c++"\">"++k++" is "++v++"</div>")
             . reverse
