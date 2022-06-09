module TypecheckerEnv where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except (when, unless, MonadError(throwError), ExceptT)
import Control.Monad.Reader (asks, MonadReader(local, ask), Reader)

import AbsHawat
import TypecheckerError


type DeclLevel = Integer
type TypeMap = Map.Map Ident (Type, DeclLevel)

data TypecheckEnv = TCEnv {tceTypeEnv      :: TypeMap,
                           tceReadOnly     :: Set.Set Ident,
                           tceLevel        :: DeclLevel,
                           tceInLoop       :: Bool,
                           tceReturnType   :: Type
                           }

initTCEnv :: TypecheckEnv
initTCEnv = TCEnv {tceTypeEnv = Map.empty,
                   tceReadOnly = Set.empty,
                   tceLevel = 0,
                   tceInLoop = False,
                   tceReturnType = Void BNFC'NoPosition -- Just a placeholer, always changed before use
                   }

type TCM a = ExceptT TypeError (Reader TypecheckEnv) a

getType :: BNFC'Position -> Ident -> TCM Type
getType pos ident = do
    typeEnv <- asks tceTypeEnv
    case Map.lookup ident typeEnv of
        Nothing -> throwError $ TE pos (TEUndeclared ident)
        Just (t, _) -> return t


retInsertEnv :: Ident -> Type -> DeclLevel -> TypeMap -> Set.Set Ident -> Bool -> Type -> TypecheckEnv
retInsertEnv ident typ level typeEnv readEnv = TCEnv (Map.insert ident (typ, level) typeEnv) (Set.delete ident readEnv) level

declare :: Ident -> Type -> TCM TypecheckEnv
declare (Ident "") t = ask -- Igonore empty ident, declared Lambda
declare ident t = do
    (TCEnv typeEnv readEnv l loop ret) <- ask
    case Map.lookup ident typeEnv of
        Just (_, level) -> if l == level then
                throwError $ makeError t (TERedeclaration ident)
            else
                return $ retInsertEnv ident t l typeEnv readEnv loop ret
        Nothing -> return $ retInsertEnv ident t l typeEnv readEnv loop ret


declareArgs :: [Arg] -> TCM TypecheckEnv
declareArgs [] = ask
declareArgs (ArgL _ typ ident : xs) = do
    newEnv <- declare ident typ
    local (const newEnv) (declareArgs xs)

getLoopError :: LoopS -> TypeErrors
getLoopError (Break _) = TEBreak
getLoopError (Cont _) = TEContinue

checkInLoop :: LoopS -> TCM ()
checkInLoop s = do
    inLoop <- asks tceInLoop
    unless inLoop (throwError $ makeError s (getLoopError s))

checkReadOnly :: BNFC'Position -> Ident -> TCM ()
checkReadOnly pos ident = do
    readOnlySet <- asks tceReadOnly
    when (Set.member ident readOnlySet) (throwError $ TE pos (TEReadOnly ident))


addReadOnly :: Ident -> TypecheckEnv -> TypecheckEnv
addReadOnly ident (TCEnv t r l loop ret) = TCEnv t (Set.insert ident r) l loop ret

setReturnType :: Type -> TypecheckEnv -> TypecheckEnv
setReturnType typ (TCEnv t r l loop _) = TCEnv t r l loop typ

goInsideLoop :: TypecheckEnv -> TypecheckEnv
goInsideLoop (TCEnv t r l _ ret) = TCEnv t r l True ret

addLevel :: TypecheckEnv -> TypecheckEnv
addLevel (TCEnv t r l loop ret) = TCEnv t r (l + 1) loop ret