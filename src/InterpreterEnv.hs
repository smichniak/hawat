module InterpreterEnv where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader(ask), ReaderT)
import Control.Monad.State ()

import AbsHawat (Arg, Block, Ident(..), Type)
import InterpreterError (InterpreterError)


type Loc = Int
data Ans = FunctionAns (StoreData, Store) | BreakAns Store | ContinueAns Store

type Cont = Store -> IM Ans
type ContExpr = StoreData -> Store -> IM Ans
type ContEnv = InterpreterEnv -> Store -> IM Ans

data Store = IStore {storeMap :: IntMap.IntMap StoreData,
                     nextLoc :: Loc
                     }

data StoreData = IntS Integer | BoolS Bool | StringS String | VoidS | ArrayS (IntMap.IntMap StoreData)
  | FunS (InterpreterEnv, [Arg], Type, Block) | PrintFunS (InterpreterEnv, [Arg], Type, Block)

instance Show StoreData where
    show (IntS i) = show i
    show (BoolS b) = show b
    show (StringS s) = s
    show VoidS = "void"
    show (ArrayS a) = show a
    show (FunS (_, argList, returnType, _)) = show argList ++ " " ++ show returnType
    show (PrintFunS (_, argList, returnType, _)) = show argList ++ " " ++ show returnType

type InterpreterEnv = Map.Map Ident Loc

type IM a = ExceptT InterpreterError (ReaderT InterpreterEnv IO) a

initIEnv :: InterpreterEnv
initIEnv = Map.empty

initStore :: Store
initStore = IStore {storeMap = IntMap.empty,
                    nextLoc = 0}

declare :: Ident -> StoreData -> InterpreterEnv -> Store -> (InterpreterEnv, Store)
declare ident value env store =
    let loc = nextLoc store
        newEnv = Map.insert ident loc env
        newStore = IStore (IntMap.insert loc value (storeMap store)) (loc + 1) in
    (newEnv, newStore)

updateVar :: Ident -> StoreData -> Store -> IM Store
updateVar ident newVal store = do
    env <- ask
    let (Ident s) = ident
    let (Just loc) = Map.lookup ident env
    let newMap = IntMap.insert loc newVal (storeMap store)
    return $ IStore newMap (nextLoc store)

getVal :: Ident -> InterpreterEnv -> Store -> StoreData
getVal ident env store = storeMap store IntMap.! (env Map.! ident)
