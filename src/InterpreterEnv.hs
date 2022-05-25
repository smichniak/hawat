module InterpreterEnv where

import qualified Data.Map as Map
import qualified Data.IntMap.Strict as IntMap -- TODO Remove strict?
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import AbsHawat
import InterpreterError


type Loc = Int
data Ans = ProgramAns Store | FunctionAns StoreData

type Cont = Store -> Ans
type ContExpr = StoreData -> IM Cont
type ContEnv = InterpreterEnv -> IM Cont


emptyProgramCont :: Cont
emptyProgramCont = ProgramAns

data Store = IStore {storeMap :: IntMap.IntMap StoreData,
                     nextLoc :: Loc
                     } -- deriving (Eq, Ord, Show)

data StoreData = IntS Integer | BoolS Bool | StringS String | VoidS | ArrayS (IntMap.IntMap StoreData)
  | FunS (InterpreterEnv, [Arg], Type, Block) -- | StoreArray [StoreData] -- Think about this type, maybe (Map Ident StoreData) -> StoreData
  --  deriving (Eq, Ord, Show) -- TODO Kepp syntax of functions, not semantics


instance Show StoreData where
    show (IntS i) = show i
    show (BoolS b) = show b
    show (StringS s) = s
    show VoidS = "void"
    show (ArrayS a) = show a
    show (FunS (_, argList, returnType, _)) = show argList ++ " " ++ show returnType


instance Show Store where
    show (IStore s l) = show s ++ " " ++ show l

type InterpreterEnv = Map.Map Ident Loc

initIEnv :: InterpreterEnv
initIEnv = Map.empty

initStore :: Store
initStore = IStore {storeMap = IntMap.empty,
                    nextLoc = 0
                    }

type IM a = ExceptT InterpreterError (ReaderT InterpreterEnv (State Store)) a


declare :: Ident -> StoreData -> InterpreterEnv -> Store -> (InterpreterEnv, Store)
declare ident value env store =
    let loc = nextLoc store
        newEnv = Map.insert ident loc env
        newStore = IStore (IntMap.insert loc value (storeMap store)) (loc + 1) in
    (newEnv, newStore)


getVal :: Ident -> InterpreterEnv -> Store -> StoreData
getVal ident env store = storeMap store IntMap.! (env Map.! ident)
