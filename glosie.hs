import Haste.App
import Haste.App.Concurrent
import Haste.Serialize
import System.Directory
import Control.Monad
import Control.Applicative

data Event = AnswerOK Double | NewDict [(String, String)]
data AnswerEvent = Answer String | Die

main = runApp (defaultConfig "ws://localhost:24601" 24601) $ do
  listDicts <- export . liftIO $ do
    files <- getDirectoryContents "dicts"
    return [takeWhile (/= '.') d | d <- files, head d /= '.', d /= "default"]

  getDict <- export $ liftIO . readFile . ("dicts/" ++) . filter (/= '/')

  let ids = ["hint", "question", "stats", "problems", "dictList", "answer"]
  runClient $ withElems ids  $ \[hint,q,stats,problems,dictList,ans] -> do
    ds <- onServer listDicts
    d <- parse <$> onServer (getDict <.> "hiragana.lst")
    s <- newSeed
    clearChildren dictList
    forM_ ds $ \d -> do
      e <- newElem "option"
      setProp e "innerText" d
      setProp e "value" $ d ++ ".lst"
      addChild e dictList

    ansvar <- newEmptyMVar
    ans `onEvent` OnKeyPress $ \13 -> do
      getProp ans "value" >>= putMVar ansvar . Answer
      setProp ans "value" ""

    evt <- newEmptyMVar
    dictList `onEvent` OnChange $ do
      newdict <- getProp dictList "value" >>= \d -> onServer (getDict <.> d)
      putMVar evt $ NewDict $ parse newdict

    let ask d s ok wrong = do
          let (ix, s') = randomR (0, length d) s
              p@(question, answer) = d !! ix
              st = show (convert $ ok*100/(ok+wrong) :: Int) ++ " % correct"
          when (ok > 0) $ setProp stats "innerText" st
          setProp q "innerText" question
          fork $ answerLoop 0 answer
          event <- takeMVar evt
          case event of
            AnswerOK wrongTries -> do
              let c = if wrongTries==1 then "correctAfterFirstGuess" else "wrong"
              when (wrongTries > 0) $ do
                e <- newElem "div"
                setProp e "innerText" $ question ++ " is " ++ answer
                setProp e "className" c
                addChild e problems
              ask d s' (ok+1) (wrong+wrongTries)
            NewDict d' -> do
              putMVar ansvar Die
              ask d' s' ok wrong

        answerLoop tries answer = do
          a <- takeMVar ansvar
          case a of
            Answer answer'
              | answer' == answer -> do
                setStyle q "color" "black"
                setProp hint "innerText" ""
                putMVar evt $ AnswerOK tries
              | otherwise -> do
                setStyle q "color" "red"
                when (tries > 1) $ setProp hint "innerText" answer
                answerLoop (tries+1) answer
            Die -> return ()
    ask d s 0 0

parse :: String -> [(String, String)]
parse = map go . lines
  where go s = case span (/= ':') s of
                 (a, _:q) -> (q, a)
