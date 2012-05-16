{-# OPTIONS_GHC -F -pgmF she #-}
module Main where
import Control.Applicative
import Haste
import Haste.JSON
import Haste.Reactive
import System.IO.Unsafe

mkDict :: String -> [(String, String)]
mkDict = map (\str -> let (v,k) = span (/=':') str in (drop 1 k, v)) . lines

mkOpts :: JSON -> String
mkOpts (Arr opts) =
  title ++ concat (map toOpt opts)
  where
    title = "<option disabled>Choose a dictionary</option>"
    toOpt (Str o) =
      "<option value=\"" ++ toStr o ++ ".lst\">" ++ toStr o ++ "</option>"

data State = State {
    totalTries   :: Double,
    correctTries :: Double,
    triesLeft    :: Int,
    curQuestion  :: String,
    curAnswer    :: String,
    dict         :: [(String, String)],
    seed         :: Seed,
    problems     :: [(String, String, String)]
  }

initState = State {
    totalTries = 0,
    correctTries = 0,
    triesLeft = 3,
    curQuestion = "",
    curAnswer = "",
    dict = [],
    seed = unsafePerformIO $ newSeed,
    problems = []
  }

newQ q = do
  setProp "question" "innerHTML" q
  setProp "hint" "innerHTML" ""

reportSuccess probs q good tot = do
  newQ q
  setProp "stats" "innerHTML" $ show_ (round_ $ (good/tot)*100) ++ " % correct"
  setProp "problems" "innerHTML" (showList probs)
  where
    showList = concat
             . map
                (\(k,v,c) -> "<div class=\""++c++"\">"++k++" is "++v++"</div>")
             . reverse

onNewDict d st =
  (st {dict = d, curQuestion = q, curAnswer = a, seed = seed', triesLeft = 3},
   newQ q)
  where
    (newIx, seed') = randomR (0, length d) (seed st)
    (q,a) = d !! newIx

onGuess guess st
  | guess == curAnswer st = goodGuess
  | otherwise             = badGuess
  where
    goodGuess =
      (st {correctTries = corr,
           totalTries = tot,
           curQuestion = q,
           curAnswer = a,
           seed = seed',
           triesLeft = 3},
       reportSuccess (problems st) q corr tot)

    badGuess =
      (st {triesLeft = tries,
           totalTries = newTotal,
           problems = probs},
       setProp "hint" "innerHTML" hint)

    (corr, tot) | triesLeft st == 3 = (correctTries st+1,totalTries st+1)
                | otherwise         = (correctTries st, totalTries st)
    (newIx, seed') = randomR (0, length $ dict st) (seed st)
    (q,a) = dict st !! newIx
      
    tries = max 0 (triesLeft st - 1)
    hint | tries > 0 = show tries ++ " tries left"
         | otherwise = "The answer is " ++ curAnswer st
    newTotal | tries == 2 = totalTries st+1
             | otherwise  = totalTries st
    probs
      | tries == 2 =
        (curQuestion st, curAnswer st, "correctAfterFirstGuess") : problems st
      | otherwise =
        case problems st of
          ((k,v,_):ps) -> (k,v,"wrong"):ps
          _            -> []

main = do
  (pstart, start) <- pipe ()
  dictList <- jsonSig (pure "api.cgi") ([] <$ start)
  dictName <- valueOf "dictList"
  answer <- valueOf "answer"

  let initialDict = ("hiragana.lst" <$ start)
      dictpath = (| pure "dicts/" ++ (dictName `union` initialDict) |)
  dict <- fmap mkDict <$> ajaxSig dictpath (pure [])
  
  let evts = unions [onGuess <$> answer, onNewDict <$> dict]
      acts = stateful (initState, return ()) evts

  domObj "dictList.innerHTML" << mkOpts <$> dictList
  domObj "answer.value" << ("" <$ answer)
  sink id acts
  write pstart ()
