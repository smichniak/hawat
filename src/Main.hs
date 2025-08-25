module Main where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure, ExitCode (ExitSuccess, ExitFailure), exitWith )
import System.IO          ( hPutStrLn, stderr )

import AbsHawat   ( Program )
import LexHawat   ( Token )
import ParHawat   ( pProgram, myLexer )
import SkelHawat  ()

import Typechecker
import Interpreter
import InterpreterEnv
import Utils


type Err      = Either String
type ParseFun = [Token] -> Err Program

runFile :: ParseFun -> FilePath -> IO ()
runFile p f = readFile f >>= run p

pareseErrorStr :: String
pareseErrorStr = "Parse Failed\n"

exitType :: Int -> ExitCode
exitType 0 = ExitSuccess
exitType n = ExitFailure n

run :: ParseFun -> String -> IO ()
run p s =
    case p $ myLexer s of
        Left err -> do
            hPutStrLn stderr pareseErrorStr
            hPutStrLn stderr err
            exitFailure
        Right programTree ->
            let builtinsProgramTree = addBuiltins programTree in
            case typecheckProgram builtinsProgramTree of
                Just err -> print err
                Nothing -> do
                    programResult <- interpretProgram builtinsProgramTree
                    case programResult of
                        Left interpreterErr -> print interpreterErr
                        Right (FunctionAns (IntS mainReturn, _Store)) ->
                            exitWith $ exitType $ fromInteger mainReturn

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
