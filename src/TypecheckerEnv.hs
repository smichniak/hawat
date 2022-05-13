module TypecheckerEnv where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.Reader

--import AbsHawat ( Type, Ident, Type'(Void), LoopS'(Break, Cont), LoopS, HasPosition ) 
import AbsHawat
import TypecheckerError

type DeclLevel = Integer

data TypecheckEnv = TCEnv {tceTypeEnv      :: Map.Map Ident (Type, DeclLevel),
                           tceReadOnly     :: Set.Set Ident,
                           tceLevel        :: DeclLevel,
                           tceInLoop       :: Bool,
                           tceReturnType   :: Type
                           }

initTCEnv = TCEnv {tceTypeEnv = Map.empty,
                   tceReadOnly = Set.empty,
                   tceLevel = 0,
                   tceInLoop = False,
                   tceReturnType = Void Nothing -- Just a placeholer, always changed before use
                   }

type TCM a = ExceptT TypeError (Reader TypecheckEnv) a

getType :: BNFC'Position -> Ident -> TCM Type
getType pos ident = do
    (TCEnv typeEnv readEnv l loop ret) <- ask
    case Map.lookup ident typeEnv of
        Nothing -> throwError $ TE pos (TEUndeclared ident) -- TODO add correct position, maybe add another argument, expression the varaible uses
        Just (t, _) -> return t

declare :: Ident -> Type -> TCM TypecheckEnv
declare ident t = if ident == Ident "" then ask else do -- Igonore empty ident, declared Lambda
    (TCEnv typeEnv readEnv l loop ret) <- ask
    case Map.lookup ident typeEnv of -- TODO Ugly
        Just (_, level) -> if l == level then throwError $ makeError t (TERedeclaration ident) else return (TCEnv (Map.insert ident (t, l) typeEnv) (Set.delete ident readEnv) l loop ret)
        Nothing -> return (TCEnv (Map.insert ident (t, l) typeEnv) (Set.delete ident readEnv) l loop ret)
        

declareArgs :: [Arg] -> TCM TypecheckEnv
declareArgs [] = ask
declareArgs (ArgL argPos typ ident : xs) = do
    newEnv <- declare ident typ
    local (const newEnv) (declareArgs xs)
        
getLoopError :: LoopS -> TypeErrors
getLoopError (Break _) = TEBreak
getLoopError (Cont _) = TEContinue

checkInLoop :: LoopS -> TCM TypecheckEnv
checkInLoop s = do
    env <- ask
    unless (tceInLoop env) (throwError $ makeError s (getLoopError s))
    return env

checkReadOnly :: BNFC'Position -> Ident -> TCM TypecheckEnv
checkReadOnly pos ident = do
    env <- ask
    when (Set.member ident (tceReadOnly env)) (throwError $ TE pos (TEReadOnly ident))
    return env
   

addReadOnly :: Ident -> TypecheckEnv -> TypecheckEnv
addReadOnly ident (TCEnv t r l loop ret) = TCEnv t (Set.insert ident r) l loop ret

setReturnType :: Type -> TypecheckEnv -> TypecheckEnv
setReturnType typ (TCEnv t r l loop _) = TCEnv t r l loop typ

goInsideLoop :: TypecheckEnv -> TypecheckEnv
goInsideLoop (TCEnv t r l _ ret) = TCEnv t r l True ret

addLevel :: TypecheckEnv -> TypecheckEnv
addLevel (TCEnv t r l loop ret) = TCEnv t r (l + 1) loop ret