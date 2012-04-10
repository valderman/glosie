module Main where
import Control.Applicative
import Haste
import Haste.JSON
import Haste.Reactive
import Haste.Reactive.Ajax

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
  dictName <- valueOf "dictList"
  answer <- valueOf "answer"
  (qaPipe, qAndA) <- pipe ("","")
  (tryPipe, tries) <- pipe (3 :: Int)
  (failPipe, failures) <- pipe []
  (kickoffPipe, kickoff) <- pipe ()
  let dict = mkDict <$> ajaxSig (("dicts/"++) <$> dname) (pure [])
      dname = kickoff `triggers` initially "default" dictName
  
      newQA dict True _ = getQA dict
      newQA dict _   qa = return qa
  
      getQA d = do
        wordIndex <- randomIO (0, length d-1)
        return (d !! wordIndex)
        
      correct =
        initially False $ buffered $ (==) <$> buffered (snd <$> qAndA) <*> answer

      answerColors =
        (\ok ts -> if ok || ts == 3 then "" else "wrong") <$> correct <*> tries
      
      triesLeft _   3             = ""
      triesLeft qa x | x <= 0    = snd qa
                     | otherwise = show_ x ++ " tries left"
      
      moreFail = addFail <$> initially [] (buffered failures)
                         <*> initially ("","") (buffered qAndA)
                         <*> tries
        where
          addFail fs cur 0 = cur:fs
          addFail fs _   _ = fs
  
  qaPipe << answer `triggers`
              (perform (newQA <$> dict <*> correct <*> buffered qAndA))
  qaPipe << perform (getQA <$> dict)
  tryPipe << answer `triggers` ((\x -> x - 1) <$> buffered tries)
  tryPipe << lazy qAndA `triggers` pure 3
  failPipe << moreFail
  
  domObj "dictList.innerHTML" <<
    mkOpts <$> jsonSig (pure "api.cgi") (kickoff `triggers` pure [])  
  domObj "question.innerHTML" << fst <$> qAndA
  domObj "question.className" << answerColors
  domObj "question.className" << qAndA `triggers` pure ""
  domObj "hint.innerHTML" << triesLeft <$> qAndA <*> tries
  domObj "answer.value" << answer `triggers` pure ""
  domObj "problems.innerHTML" << showList <$> failures
  push () kickoffPipe
  where
    showList =
      concat .
      map (\(k,v) -> "<div class=\"wrong\">" ++ k ++ " is " ++ v ++ "</div>") .
      reverse
