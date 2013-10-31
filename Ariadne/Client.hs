module Main where

import Control.Monad ((=<<))
import System.Environment (getArgs)

import Data.BERT
import Network.BERT.Client
import Network.BERT.Transport
import qualified Data.ByteString.Lazy.UTF8 as UTF8

{-

Getting this message sometimes:

ariadne-client: recv: invalid argument (Bad file descriptor)

-}

main :: IO ()
main = executeCommand =<< getArgs

executeCommand :: [String] -> IO ()
executeCommand [] = putStrLn "usage"
executeCommand ("find":filePath:line:column:[]) = executeFind filePath (read line) (read column)
executeCommand _ = putStrLn "err usage"

executeFind :: FilePath -> Int -> Int -> IO ()
executeFind filePath line column =
  do t <- fromURI "bert://localhost:39014"
     r <- call t "ariadne" "find" ([BinaryTerm (UTF8.fromString filePath), IntTerm line, IntTerm column]::[Term])
     case r of
       Right res -> display res
       Left _ -> putStrLn "error"

display :: Term -> IO ()
display (TupleTerm [AtomTerm "no_name"]) = putStrLn "no name"
display (TupleTerm [AtomTerm "loc_known", BinaryTerm file, IntTerm line, IntTerm column]) = putStrLn $ "find at " ++ (UTF8.toString file) ++ " on " ++ show line ++ "," ++ show column
display (TupleTerm [AtomTerm "loc_unknown", BinaryTerm mod]) = putStrLn $ "unknown at " ++ (UTF8.toString mod) ++ " module"
display (TupleTerm [AtomTerm "error", BinaryTerm msg]) = putStrLn $ "server error: " ++ (UTF8.toString msg)
display (TupleTerm (AtomTerm x:_)) = putStrLn $ "unknown return " ++ x
display _ = putStrLn "unknown answer"
