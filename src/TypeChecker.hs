module TypeChecker where

import qualified Data.Map as Map

import AbsHawat ( Type, Ident, Program )

type DeclLevel = Integer

data TypeCheckEnv = TCEnv {tceTypeEnv      :: Map.Map Ident (Type, DeclLevel),
                           tceLevel        :: DeclLevel,
                           tceInLoop       :: Bool,
                           tceReturnType   :: Maybe Type
                           }

initTCEnv = TCEnv {tceTypeEnv = Map.empty,
                   tceLevel = 0,
                   tceInLoop = False,
                   tceReturnType = Nothing
                   }


typeCheckProgram :: Program -> Except String ()

