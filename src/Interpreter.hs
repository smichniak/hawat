module Interpreter where

import Control.Monad.Except (when, runExceptT, MonadIO(liftIO), MonadError(throwError))
import Control.Monad.Reader (MonadReader(local, ask), ReaderT(runReaderT))
import Control.Monad.State ()
import System.IO ()

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map

import AbsHawat
import InterpreterEnv
import InterpreterError (InterpreterError(IE), InterpreterErrors(IEModZero, IEDivZero))
import Utils (printIdents, naturals, getPlaceholderArgs)


interpretProgram :: Program -> IO (Either InterpreterError Ans)
interpretProgram (ProgramL _ topDefList) =  (runReaderT $ runExceptT $ interpretTopDefProgram topDefList) initIEnv

functionReturn :: ContExpr
functionReturn val store = return $ FunctionAns (val, store)

mainExpr :: Expr
mainExpr = EApp BNFC'NoPosition (EVar BNFC'NoPosition (Ident "main")) []

mainCont :: Cont
mainCont = evalExpr mainExpr functionReturn

interpretTopDefProgram :: [TopDef] -> IM Ans
interpretTopDefProgram defs = evalTopDefs defs mainCont initStore

evalTopDefs :: [TopDef] -> Cont -> Store -> IM Ans
evalTopDefs [] cont s = cont s
evalTopDefs (def : restDefs) cont s = do
    let envCont = \newEnv newStore -> local (const newEnv) (evalTopDefs restDefs cont newStore)
    evalTopDef def envCont s

evalTopDef :: TopDef -> ContEnv -> Store -> IM Ans
evalTopDef (Global _ decl) envCont s = evalDecl decl envCont s
evalTopDef (FnDef pos ident argList returnType block) envCont s = evalFuntionDef ident argList returnType block envCont s

evalFuntionDef :: Ident -> [Arg] -> Type -> Block -> ContEnv -> Store -> IM Ans
evalFuntionDef ident argList returnType block envCont s = do
    env <- ask
    let functionType = if elem ident printIdents then PrintFunS else FunS
    let (newEnv, newStore) = declare ident (functionType (newEnv, argList, returnType, block)) env s
    envCont newEnv newStore

evalDecl :: Decl -> ContEnv -> Store -> IM Ans
evalDecl (DeclL _ _ []) envCont s = do
    env <- ask
    envCont env s
evalDecl (DeclL declPos typ ((NoInit initPos ident) : xs)) envCont s = evalDecl (DeclL declPos typ (Init initPos ident (typeDefaultExpr typ) : xs)) envCont s
evalDecl (DeclL declPos typ ((Init initPos ident expr) : xs)) envCont s = do
    env <- ask
    let exprCont = \val store -> let (newEnv, newStore) = declare ident val env store in do
            local (const newEnv) (evalDecl (DeclL declPos typ xs) envCont newStore)
    evalExpr expr exprCont s

evalExpr :: Expr -> ContExpr -> Store -> IM Ans
evalExpr (EVar _ ident) contExpr s = do
    env <- ask
    contExpr (getVal ident env s) s

evalExpr (ELitInt _ int) contExpr s = contExpr (IntS int) s
evalExpr (ELitTrue _) contExpr s = contExpr (BoolS True) s
evalExpr (ELitFalse _) contExpr s = contExpr (BoolS False) s
evalExpr (EVoid _) contExpr s = contExpr VoidS s
evalExpr (EApp _ funExpr argExprList) contExpr s = evalExpr funExpr (functionContExpr argExprList contExpr) s
evalExpr (EGet _ arrExpr indexExpr) contExpr s = undefined -- TODO
evalExpr (EArray _ exprList) contExpr s = undefined -- TODO
evalExpr (EString _ str) contExpr s = contExpr (StringS str) s
evalExpr (ELambda _ argList returnType instructionBlock) contExpr s = do
    env <- ask
    contExpr (FunS (env, argList, returnType, instructionBlock)) s

evalExpr (Neg _ expr) contExpr s = evalExpr expr (\(IntS x) -> contExpr (IntS (-x))) s
evalExpr (Not _ expr) contExpr s = evalExpr expr (\(BoolS b) -> contExpr (BoolS (not b))) s
evalExpr (EMul _ expr1 mulOp expr2) contExpr s =
    evalExpr expr1 (\x1 store1 -> evalExpr expr2 (\x2 store2 -> do
        mulResult <- mulData x1 x2 mulOp
        contExpr mulResult store2) store1) s

evalExpr (EAdd _ expr1 addOp expr2) contExpr s =
    evalExpr expr1 (\(IntS x1) -> evalExpr expr2 (\(IntS x2) -> contExpr (addInt x1 x2 (getAddOperation addOp)))) s

evalExpr (ERel _ expr1 relOp expr2) contExpr s =
    evalExpr expr1 (\(IntS x1) -> evalExpr expr2 (\(IntS x2) -> contExpr (relInt x1 x2 (getRelOperation relOp)))) s

evalExpr (EAnd _ expr1 expr2) contExpr s =
    evalExpr expr1 (\(BoolS b1) -> evalExpr expr2 (\(BoolS b2) -> contExpr (BoolS (b1 && b2)))) s

evalExpr (EOr _ expr1 expr2) contExpr s =
    evalExpr expr1 (\(BoolS b1) -> evalExpr expr2 (\(BoolS b2) -> contExpr (BoolS (b1 || b2)))) s


evalStmt :: Stmt -> Cont -> Store -> IM Ans
evalStmt (Empty _) cont s = cont s
evalStmt (BStmt _ block) cont s = do
    env <- ask
    evalBlock block cont env s
evalStmt (SDecl _ decl) cont s = evalDecl decl (\env store -> local (const env) (cont store)) s
evalStmt (Ass _ (EVar _ ident) expr) cont s = evalExpr expr (\newVal store -> do
    newStore <- updateVar ident newVal store
    cont newStore) s
evalStmt (Ass _ _ expr) cont s = undefined -- Never happens, typchecked before
evalStmt (Incr p expr) cont s = evalStmt (Ass p expr (EAdd p expr (Plus p) (ELitInt p 1))) cont s
evalStmt (Decr p expr) cont s = evalStmt (Ass p expr (EAdd p expr (Minus p) (ELitInt p 1))) cont s
evalStmt (Ret _ expr) cont s = evalExpr expr functionReturn s
evalStmt (VRet _) cont s = evalExpr (typeDefaultExpr $ Void BNFC'NoPosition) functionReturn s
evalStmt (SCond _ cond) cont s = evalCond cond cont s
evalStmt (While p condExpr block) cont s = evalExpr condExpr (\(BoolS b) store ->
    if b then do
        env <- ask
        ans <- evalBlock block (evalStmt (While p condExpr block) cont) env store
        case ans of
            BreakAns newStore -> cont newStore
            ContinueAns newStore -> evalStmt (While p condExpr block) cont newStore
            _ -> return ans
    else
        cont store) s

evalStmt (For p iteratorIdent startExpr endExpr block) cont s =
    evalStmt (SDecl BNFC'NoPosition (forDecl iteratorIdent startExpr))
    (evalExpr endExpr (\(IntS end) newStore -> evalStmt (While p (forCondExpr iteratorIdent end) (addToBlock block $ incrementIterator iteratorIdent)) cont newStore)) s

evalStmt (LoopJmp _ loopJmp) cont s = return $ getLoopAns loopJmp s
evalStmt (SExp _ expr) cont s = evalExpr expr (const cont) s


forDecl :: Ident -> Expr -> Decl
forDecl ident expr = DeclL BNFC'NoPosition (Int BNFC'NoPosition) [Init BNFC'NoPosition ident (EAdd BNFC'NoPosition expr (Minus BNFC'NoPosition) (ELitInt BNFC'NoPosition 1))]

forCondExpr :: Ident -> Integer -> Expr
forCondExpr iterator end = ERel BNFC'NoPosition (EVar BNFC'NoPosition iterator) (LTH BNFC'NoPosition) (ELitInt BNFC'NoPosition end)

incrementIterator :: Ident -> Stmt
incrementIterator ident = Incr BNFC'NoPosition (EVar BNFC'NoPosition ident)

addToBlock :: Block -> Stmt -> Block
addToBlock (BlockL p stmts) stmt = BlockL p [stmt, BStmt p (BlockL p stmts)]

getLoopAns :: LoopS -> Store -> Ans
getLoopAns (Break _) = BreakAns
getLoopAns (Cont _) = ContinueAns

evalCond :: Cond -> Cont -> Store -> IM Ans
evalCond (If _ condExpr block) cont s = evalExpr condExpr (\(BoolS b) store -> if b then do
    env <- ask
    evalBlock block cont env store
    else cont store) s

evalCond (IfElse _ condExpr ifBlock elseBlock) cont s = evalExpr condExpr (\(BoolS b) store -> do
    env <- ask
    if b then do
    evalBlock ifBlock cont env store else evalBlock elseBlock cont env store) s

evalCond (ElseIf _ condExpr block elseCond) cont s = evalExpr condExpr (\(BoolS b) store -> do
    env <- ask
    if b then evalBlock block cont env store else evalCond elseCond cont store) s

functionBlockReturn :: Block -> Type -> Store -> IM Ans
functionBlockReturn block t s = do
    env <- ask
    evalBlock block (evalExpr (typeDefaultExpr t) functionReturn) env s

evalBlock :: Block -> Cont -> ContEnv
evalBlock (BlockL _ []) cont env s = local (const env) (cont s)
evalBlock (BlockL p (stmt : restStmt)) cont env s = do
    localEnv <- ask
    local (const localEnv) (evalStmt stmt (evalBlock (BlockL p restStmt) cont env) s)

functionContExpr :: [Expr] -> ContExpr -> ContExpr -- Pattern match only for FunS, typechecked before
functionContExpr [] contExpr (FunS (interpreterEnv, argList, returnType, block)) s = do
    FunctionAns (returnValue, newStore) <- local (const interpreterEnv) (functionBlockReturn block returnType s)
    contExpr returnValue newStore

functionContExpr (firstExpr : restExpr) contExpr (FunS (interpreterEnv, (ArgL _ _ argIdent) : restArgs, returnType, block)) s = do
    let newExprCont = \exprVal store -> let (newEnv, newStore) = declare argIdent exprVal interpreterEnv store in do
            functionContExpr restExpr contExpr (FunS (newEnv, restArgs, returnType, block)) newStore
    evalExpr firstExpr newExprCont s

functionContExpr [printExpr] contExpr (PrintFunS (interpreterEnv, [ArgL _ _ argIdent], returnType, block)) s = do
    let newExprCont = \exprVal store -> do
        liftIO $ putStr $ show exprVal
        functionContExpr [] contExpr (FunS (interpreterEnv, [], returnType, block)) store
    evalExpr printExpr newExprCont s

functionContExpr _ _ _ _ = undefined -- Never happens, typchecked before

typeDefaultExpr :: Type -> Expr
typeDefaultExpr (Int pos) = ELitInt pos 0
typeDefaultExpr (Bool pos) = ELitFalse pos
typeDefaultExpr (Str pos) = EString pos ""
typeDefaultExpr (Void pos) = EVoid pos
typeDefaultExpr (Arr pos arrType) = EArray pos []
typeDefaultExpr (Fun pos typeList returnType) = ELambda pos (getPlaceholderArgs typeList) returnType (returnDefault returnType)

returnDefault :: Type -> Block
returnDefault returnType = BlockL BNFC'NoPosition [Ret BNFC'NoPosition (typeDefaultExpr returnType)]

type RelOperation = (Integer -> Integer -> Bool)
type ArithemticOperation = (Integer -> Integer -> Integer)

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
