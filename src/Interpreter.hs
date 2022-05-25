module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map

import AbsHawat
import InterpreterEnv
import InterpreterError
import Control.Monad.Cont (cont)

interpretProgram :: Program -> Either InterpreterError Ans
interpretProgram (ProgramL _ topDefList) = evalState ((runReaderT $ runExceptT $ interpretTopDefProgram topDefList) initIEnv) initStore
--interpretProgram (ProgramL _ topDefList) = (runCont (runReaderT (runExceptT (interpretTopDefs topDefList)) initIEnv)) id


-- Empty identifier is not taken, we can use it
callMain = Global BNFC'NoPosition (DeclL BNFC'NoPosition (Int BNFC'NoPosition) [Init BNFC'NoPosition (Ident "") (EApp BNFC'NoPosition (EVar BNFC'NoPosition (Ident "main")) [])])

interpretTopDefProgram :: [TopDef] -> IM Ans
interpretTopDefProgram defs = do
  --  finalCont <- evalTopDefs (defs) emptyCont -- TODO Restore: evalTopDefs (defs ++ [callMain]) emptyCont
    finalCont <- evalTopDefs (defs ++ [callMain]) emptyProgramCont
    gets finalCont
    --return store
 --   return $ finalCont store
 --   throwError $ IE Nothing IEDivZero


evalTopDefs :: [TopDef] -> Cont -> IM Cont
evalTopDefs [] cont = return cont
  --  do
  --  store <- get
  --  return $ cont store

evalTopDefs (def : restDefs) cont = do
    let envCont = \newEnv -> local (const newEnv) (evalTopDefs restDefs cont)
    evalTopDef def envCont

evalTopDef :: TopDef -> ContEnv -> IM Cont
evalTopDef (Global _ decl) envCont = evalDecl decl envCont
evalTopDef (FnDef pos ident argList returnType block) envCont = evalFuntionDef ident argList returnType block envCont

evalFuntionDef :: Ident -> [Arg] -> Type -> Block -> ContEnv -> IM Cont
evalFuntionDef ident argList returnType block envCont = do
    env <- ask
    store <- get
    let (newEnv, newStore) = declare ident (FunS (newEnv, argList, returnType, block)) env store -- TODO Incredible, that this works (recursive newEnv)
    put newStore
    envCont newEnv

evalDecl :: Decl -> ContEnv -> IM Cont
evalDecl (DeclL _ _ []) envCont = do
    env <- ask
    envCont env
evalDecl (DeclL declPos typ ((NoInit initPos ident) : xs)) envCont = evalDecl (DeclL declPos typ (Init initPos ident (typeDefaultExpr typ) : xs)) envCont
evalDecl (DeclL declPos typ ((Init initPos ident expr) : xs)) envCont = do
    env <- ask
    store <- get

    let exprCont = \val -> let (newEnv, newStore) = declare ident val env store in do
            put newStore
            local (const newEnv) (evalDecl (DeclL declPos typ xs) envCont)
    evalExpr expr exprCont

evalExpr :: Expr -> ContExpr -> IM Cont
evalExpr (EVar _ ident) contExpr = do
    env <- ask
    store <- get
    contExpr (getVal ident env store)

evalExpr (ELitInt _ int) contExpr = contExpr (IntS int)
evalExpr (ELitTrue _) contExpr = contExpr (BoolS True)
evalExpr (ELitFalse _) contExpr = contExpr (BoolS False)
evalExpr (EVoid _) contExpr = contExpr VoidS
evalExpr (EApp _ funExpr argExprList) contExpr = evalExpr funExpr (functionContExpr argExprList contExpr)
evalExpr (EGet _ arrExpr indexExpr) contExpr = undefined -- TODO
evalExpr (EArray _ exprList) contExpr = undefined -- TODO
evalExpr (EString _ str) contExpr = contExpr (StringS str)
evalExpr (ELambda _ argList returnType instructionBlock) contExpr = do
    env <- ask
    contExpr (FunS (env, argList, returnType, instructionBlock))

evalExpr (Neg _ expr) contExpr = evalExpr expr (\(IntS x) -> contExpr (IntS (-x)))
evalExpr (Not _ expr) contExpr = evalExpr expr (\(BoolS b) -> contExpr (BoolS (not b)))
evalExpr (EMul _ expr1 mulOp expr2) contExpr =
    evalExpr expr1 (\x1 -> evalExpr expr2 (\x2 -> do
        mulResult <- mulData x1 x2 mulOp
        contExpr mulResult))

evalExpr (EAdd _ expr1 addOp expr2) contExpr =
    evalExpr expr1 (\(IntS x1) -> evalExpr expr2 (\(IntS x2) -> contExpr (addInt x1 x2 (getAddOperation addOp))))

evalExpr (ERel _ expr1 relOp expr2) contExpr =
    evalExpr expr1 (\(IntS x1) -> evalExpr expr2 (\(IntS x2) -> contExpr (relInt x1 x2 (getRelOperation relOp))))

evalExpr (EAnd _ expr1 expr2) contExpr =
    evalExpr expr1 (\(BoolS b1) -> evalExpr expr2 (\(BoolS b2) -> contExpr (BoolS (b1 && b2))))

evalExpr (EOr _ expr1 expr2) contExpr =
    evalExpr expr1 (\(BoolS b1) -> evalExpr expr2 (\(BoolS b2) -> contExpr (BoolS (b1 || b2))))



evalStmt :: Stmt -> Cont -> IM Cont
evalStmt (Empty _) cont = return cont
evalStmt (Ret _ expr) cont = evalExpr expr functionReturn
evalStmt (VRet _) cont = evalExpr (typeDefaultExpr $ Void BNFC'NoPosition) functionReturn
evalStmt (SExp _ expr) cont = evalExpr expr (\_ -> return cont)
evalStmt (SCond _ cond) cont = evalCond cond cont


evalCond :: Cond -> Cont -> IM Cont
evalCond (If _ condExpr block) cont = evalExpr condExpr (\(BoolS b) -> if b then evalBlock block cont else return cont)
evalCond (IfElse _ condExpr ifBlock elseBlock) cont = evalExpr condExpr (\(BoolS b) -> if b then evalBlock ifBlock cont else evalBlock elseBlock cont)
evalCond (ElseIf _ condExpr block elseCond) cont = evalExpr condExpr (\(BoolS b) -> if b then evalBlock block cont else evalCond elseCond cont)


functionReturn :: ContExpr
functionReturn d = do return (\_ -> FunctionAns d)


functionBlockReturn :: Block -> Type -> IM StoreData
functionBlockReturn block t = do
    defaultCont <- evalExpr (typeDefaultExpr t) functionReturn
    functionCont <- evalBlock block defaultCont
    store <- get
    let (FunctionAns ans) = functionCont store
    return ans


evalBlock :: Block -> Cont -> IM Cont
evalBlock (BlockL _ []) cont = return cont
evalBlock (BlockL p (s1 : restS)) cont = do
    restCont <- evalBlock (BlockL p restS) cont
    evalStmt s1 restCont




functionContExpr :: [Expr] -> ContExpr -> ContExpr -- Pattern match only for FunS, typechecked before
functionContExpr [] contExpr (FunS (interpreterEnv, argList, returnType, block)) = do
    functionReturn <- functionBlockReturn block returnType
    contExpr functionReturn

functionContExpr (firstExpr : restExpr) contExpr (FunS (interpreterEnv, (ArgL _ _ argIdent) : restArgs, returnType, block)) = do
    store <- get
    let newExprCont = \exprVal -> let (newEnv, newStore) = declare argIdent exprVal interpreterEnv store in do
            put newStore
            local (const newEnv) (functionContExpr restExpr contExpr (FunS (interpreterEnv, restArgs, returnType, block)))

    evalExpr firstExpr newExprCont

-- TODO Move to utils

typeDefaultExpr :: Type -> Expr
typeDefaultExpr (Int pos) = ELitInt pos 0
typeDefaultExpr (Bool pos) = ELitFalse pos
typeDefaultExpr (Str pos) = EString pos ""
typeDefaultExpr (Void pos) = EVoid pos
typeDefaultExpr (Arr pos arrType) = EArray pos []
typeDefaultExpr (Fun pos typeList returnType) = ELambda pos (getPlaceholderArgs typeList) returnType (returnDefault returnType)

naturals :: [Int]
naturals = iterate (+1) 0
getPlaceholderArgs :: [Type] -> [Arg]
getPlaceholderArgs = zipWith (\ident t -> ArgL (hasPosition t) t (Ident $ show ident)) naturals

returnDefault :: Type -> Block
returnDefault returnType = BlockL BNFC'NoPosition [Ret BNFC'NoPosition (typeDefaultExpr returnType)]

type RelOperation =  (Integer -> Integer -> Bool)
type ArithemticOperation =  (Integer -> Integer -> Integer)

relInt :: Integer -> Integer -> RelOperation -> StoreData
relInt x1 x2 operation = BoolS (operation x1 x2)

addInt :: Integer -> Integer -> ArithemticOperation -> StoreData
addInt x1 x2 operation = IntS (operation x1 x2)

mulInt :: Integer -> Integer -> MulOp -> IM StoreData
mulInt x1 x2 (Times _) = return (IntS (x1 * x2))
mulInt x1 x2 (Div p) = do
    when (x2 == 0) (throwError $ IE p IEDivZero)
    return (IntS $ div x1 x2)
mulInt x1 x2 (Mod p) = do
    when (x2 == 0) (throwError $ IE p IEModZero)
    return (IntS $ mod x1 x2)

mulData :: StoreData -> StoreData -> MulOp -> IM StoreData
mulData (IntS x1) (IntS x2) mulOp = mulInt x1 x2 mulOp
mulData (ArrayS map) (IntS x) op = mulData (IntS x) (ArrayS map) op
mulData (IntS x) (ArrayS map) (Times _) = do
    let elements = IntMap.elems map
    let replicated_elements = concat $ replicate (fromInteger x) elements
    let new_map = IntMap.fromDistinctAscList (zip naturals replicated_elements)
    return $ ArrayS new_map



getRelOperation :: RelOp -> RelOperation
getRelOperation (LTH _) = (<)
getRelOperation (LE _) = (<=)
getRelOperation (GTH _) = (>)
getRelOperation (GE _) = (>=)
getRelOperation (EQU _) = (==)
getRelOperation (NE _) = (/=)

getAddOperation :: AddOp -> ArithemticOperation
getAddOperation (Plus _) = (+)
getAddOperation (Minus _) = (-)

getMulOperation :: MulOp -> ArithemticOperation
getMulOperation (Times _) = (*)
getMulOperation (Div _) = div
getMulOperation (Mod _) = mod
