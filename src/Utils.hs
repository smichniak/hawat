module Utils where

import AbsHawat


printIdentStrings :: [String]
printIdentStrings = ["printStr", "printInt", "printBool"]

printIdents :: [Ident]
printIdents = map Ident printIdentStrings

printTypeConstructors :: [a -> Type' a]
printTypeConstructors = [Str, Int, Bool]

printArgs :: [Arg]
printArgs = map (\x -> ArgL BNFC'NoPosition (x BNFC'NoPosition) (Ident "b")) printTypeConstructors

emptyBlock :: Block
emptyBlock = BlockL BNFC'NoPosition []

printFunctions :: [TopDef]
printFunctions = zipWith (\ident arg -> FnDef BNFC'NoPosition ident [arg] (Void BNFC'NoPosition) emptyBlock) printIdents printArgs

addBuiltins :: Program -> Program
addBuiltins (ProgramL position topDefList) = ProgramL position (printFunctions ++ topDefList)

naturals :: [Int]
naturals = iterate (+1) 0
getPlaceholderArgs :: [Type] -> [Arg]
getPlaceholderArgs = zipWith (\ident t -> ArgL (hasPosition t) t (Ident $ show ident)) naturals