module Main where
import System.Directory

main = do
  dicts <- getDirectoryContents "dicts"
  print [takeWhile (/= '.') d | d <- dicts, head d /= '.', d /= "default"]
