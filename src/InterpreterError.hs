module InterpreterError where

import Numeric ( showInt )

import AbsHawat ( BNFC'Position )
import TypecheckerError ( showPosition )


data InterpreterErrors = IEDivZero                       | -- Division by zero
                         IEModZero                       | -- Modulo by zero
                         IEIndexRange Int Int            | -- Array index out of range (receivedIndex, listLength)
                         IES String              -- For debugging TODO Remove


data InterpreterError = IE {position :: BNFC'Position, error :: InterpreterErrors}

instance Show InterpreterErrors where
    showsPrec _ IEDivZero                 = showString "Integer division by zero"
    showsPrec _ IEModZero                 = showString "Modulo by zero"
    showsPrec _ (IEIndexRange index len)  = showString "List index " . showInt index . showString " out of range " . showInt len
    showsPrec _ (IES s)                   = showString s

instance Show InterpreterError where
    showsPrec _ (IE pos err) = showString "Interpreter error at " . showPosition pos . shows err . showChar '.'