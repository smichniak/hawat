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
                           tceReturnType   :: Maybe Type
                           }

initTCEnv = TCEnv {tceTypeEnv = Map.empty,
                   tceReadOnly = Set.empty,
                   tceLevel = 0,
                   tceInLoop = False,
                   tceReturnType = Nothing
                   }

type TCM a = ExceptT TypeError (Reader TypecheckEnv) a

getType :: BNFC'Position -> Ident -> TCM Type
getType pos ident = do
    (TCEnv typeEnv readEnv l loop ret) <- ask
    case Map.lookup ident typeEnv of
        Nothing -> throwError $ TE pos (TEUndeclared ident) -- TODO add correct position, maybe add another argument, expression the varaible uses
        Just (t, _) -> return t -- Type hidden in tceReturnType


--getType env ident = 
--    let typeEnv = tceTypeEnv env in
 --       case Map.lookup ident typeEnv of 
 --           Nothing -> Nothing
 --           Just (t, _) -> Just t

--isDeclaredHere :: TypecheckEnv -> Ident -> Bool
--isDeclaredHere env ident = 
--    let typeEnv = tceTypeEnv env in
--        case Map.lookup ident typeEnv of 
--            Nothing -> False
--            Just (_, level) -> level == tceLevel env 

declare :: Ident -> Type -> TCM TypecheckEnv
declare ident t = do
    (TCEnv typeEnv readEnv l loop ret) <- ask
    case Map.lookup ident typeEnv of
        Just (_, level) -> if l == level then throwError $ makeError t (TERedeclaration ident) else return (TCEnv typeEnv readEnv l loop ret)
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

--isReadOnly :: TypecheckEnv -> Ident -> Bool
--isReadOnly env ident = Set.member ident (tceReadOnly env)

addReadOnly :: Ident -> TypecheckEnv -> TypecheckEnv
addReadOnly ident (TCEnv t r l loop ret) = TCEnv t (Set.insert ident r) l loop ret

setReturnType :: Type -> TypecheckEnv -> TypecheckEnv
setReturnType typ (TCEnv t r l loop _) = TCEnv t r l loop (Just typ)

goInsideLoop :: TypecheckEnv -> TypecheckEnv
goInsideLoop (TCEnv t r l _ ret) = TCEnv t r l True ret

addLevel :: TypecheckEnv -> TypecheckEnv
addLevel (TCEnv t r l loop ret) = TCEnv t r (l + 1) loop ret