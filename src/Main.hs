module Main where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import System.IO          ( hPutStrLn, stderr )

import AbsHawat   ( Program )
import LexHawat   ( Token )
import ParHawat   ( pProgram, myLexer )
import SkelHawat  ()

import Typechecker
import Interpreter
import InterpreterEnv

type Err      = Either String
type ParseFun = [Token] -> Err Program


runFile :: ParseFun -> FilePath -> IO ()
runFile p f = readFile f >>= run p

pareseErrorStr :: String
pareseErrorStr = "Parse Failed\n"

run :: ParseFun -> String -> IO ()
run p s =
    case p $ myLexer s of
        Left err -> do
            hPutStrLn stderr pareseErrorStr
            hPutStrLn stderr err
            exitFailure
        Right programTree ->
            case typecheckProgram programTree of -- add prints
                Just err -> print err
                Nothing -> case interpretProgram programTree of
                    Left interpreterErr -> print interpreterErr
                    Right (ProgramAns st) -> print st

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


-- TODO Move to utils?
fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x