module Main where

import Control.Monad ((=<<))
import System.Environment (getArgs)

import Data.BERT
import Network.BERT.Client
import Network.BERT.Transport
import qualified Data.ByteString.Lazy.UTF8 as UTF8

main :: IO ()
main = executeCommand =<< getArgs

executeCommand :: [String] -> IO ()
executeCommand [] = putStrLn "usage"
executeCommand ("find":filePath:line:column:[]) = executeFind filePath (read line) (read column)
executeCommand _ = putStrLn "err usage"

executeFind :: FilePath -> Int -> Int -> IO ()
executeFind filePath line column =
  do t <- fromURI "bert://localhost:39014"
     -- não funciona com o primeiro argumento... arg!!
     r <- call t "ariadne" "find" ([BinaryTerm (UTF8.fromString filePath), IntTerm line, IntTerm column]::[Term])
     case r of
       Right res -> print (res :: Int)
       Left _    -> putStrLn "error"
