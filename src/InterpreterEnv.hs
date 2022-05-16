module InterpreterEnv where

import qualified Data.Map as Map
import qualified Data.IntMap.Strict as IntMap -- TODO Remove strict?
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import AbsHawat
import InterpreterError


type Loc = Int
type Ans = Store

type Cont = Store -> Ans
type ContExpr = StoreData -> Store -> Ans
type ContEnv = InterpreterEnv -> Cont

emptyCont = id

data Store = IStore {storeMap :: IntMap.IntMap StoreData,
                     nextLoc :: Loc
                     } deriving (Eq, Ord, Show)

data StoreData = IntS Integer | BoolS Bool | StringS String | VoidS | ArrayS (IntMap.IntMap StoreData) -- | FunS (Cont -> Cont) TODO Function looks differenct thant PROC, propably takes sth of the reurn type
    deriving (Eq, Ord, Show) -- TODO Add void type? 


type InterpreterEnv = Map.Map Ident Loc

{-
initIEnv = IEnv {iEnv = Map.empty,
               --  funStore = IntMap.empty,
                 nextLoc = 0
                 }

-}

initIEnv = Map.empty

initStore = IStore {storeMap = IntMap.empty,
                    nextLoc = 0
                    }

--type IM a = ExceptT InterpreterError (ReaderT InterpreterEnv (Cont Ans)) a
--type IM a = ContT Ans (ExceptT InterpreterError (Reader InterpreterEnv)) a
--type IM a = ExceptT InterpreterError (ContT Ans (Reader InterpreterEnv)) a

type IM a = ExceptT InterpreterError (Reader InterpreterEnv) a



declare :: Ident -> StoreData -> InterpreterEnv -> Store -> (InterpreterEnv, Store)
declare ident value env store =
    let loc = nextLoc store
        newEnv = Map.insert ident loc env
        newStore = IStore (IntMap.insert loc value (storeMap store)) (loc + 1) in
    (newEnv, newStore)


getVal :: Ident -> InterpreterEnv -> Store -> StoreData
getVal ident env store = (storeMap store) IntMap.! (env Map.! ident)

--interpretTopDefs :: a -> IM a
--interpretTopDefs a = return a


{-
mainFun k s = s

interpretTopDefs :: [TopDef] -> IM Store
interpretTopDefs _ = do
  --  let s = IntMap.empty
  --  let st = IntMap.insert 1 (IntS 2) s
  --  return st
    env <- ask
    callCC mainFun
    throwError $ IE Nothing IEDivZero

-}

-- TODO Function to inc loc