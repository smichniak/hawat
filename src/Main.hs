module Main where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import System.IO          ( hPutStrLn, stderr )
import Control.Monad      ( when )

import AbsHawat   ( Program )
import LexHawat   ( Token )
import ParHawat   ( pProgram, myLexer )
import SkelHawat  ()

import Typechecker 

type Err        = Either String
type ParseFun = [Token] -> Err Program


runFile :: ParseFun -> FilePath -> IO ()
runFile p f = readFile f >>= run p

pareseErrorStr = "Parse Failed\n"

run :: ParseFun -> String -> IO ()
run p s =
    case p ts of
        Left err -> do
            hPutStrLn stderr pareseErrorStr
            hPutStrLn stderr err
            exitFailure
        Right programTree -> do -- add prints
            case typecheckProgram programTree of
                Just err ->
                    do putStrLn (show err)
                Nothing -> do 
                    putStrLn "Interpreter"
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