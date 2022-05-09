module Main where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import System.IO          ( hPutStrLn, stderr )
import Control.Monad      ( when )

import AbsHawat   ()
import LexHawat   ( Token )
import ParHawat   ( pProgram, myLexer )
import SkelHawat  ()

type Err        = Either String
type ParseFun a = [Token] -> Err a


runFile :: (Show a) => ParseFun a -> FilePath -> IO ()
runFile p f = readFile f >>= run p

pareseErrorStr = "Parse Failed\n"

run :: (Show a) => ParseFun a -> String -> IO ()
run p s =
  case p ts of
    Left err -> do
      hPutStrLn stderr pareseErrorStr
      hPutStrLn stderr err
      exitFailure
    Right tree -> do
      putStrLn "Parse Successful!" -- TODO Remove, add typecheck and interpreter
  where
  ts = myLexer s

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "Hawat Programming Language Interpreter"
    , "Usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse and interpret from stdin."
    , "  (files)         Parse and interpret content of files."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run pProgram
    fs         -> mapM_ (runFile pProgram) fs