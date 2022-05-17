module TypecheckerError where

import AbsHawat ( Type, Ident, BNFC'Position, Type'(Int, Bool), Stmt, HasPosition, hasPosition )
import PrintHawat ( prt, render, concatS, Print )
import LexHawat ( printPosn, Posn( Pn ) )

data TypeErrors = TEBreak                       | -- Break not in a loop
                  TEContinue                    | -- Continue not in a loop
                  TEAssignExp                   | -- Cannot assign to expression that is not and Ident or array element
                  TEArrIndex Type               | -- Array indexed not by an int
                  TECondtion Type               | -- Condtion not a bool
                  TEForIter Type                | -- For iterator is not an int
                  TEUnaryOp Type                | -- Type not supported for unary operation
                  TENotFunction Type            | -- Application to a non-function ident
                  TENotArray Type               | -- Not an array indexed
                  TEReturn Type Type            | -- Wrong return type (returned, expected)
                  TEAssignment Type Type        | -- Wrong type assigneed (lvalue, rvalue)
                  TEApplication [Type] [Type]   | -- Wrong function application (applied, expected)
                  TEBinaryOp Type Type          | -- Types not supported for binary operation (left, right)
                  TEArrayType Type Type         | -- Two different types in array construction
                  TEMainSig Type Type           | -- Wrong main signature (declared, expexted)
                  TEUndeclared Ident            | -- Undeclared variable
                  TERedeclaration Ident         | -- Redeclartion of a variable
                  TEReadOnly Ident                -- Modification of read-only variable (usually in a for loop)

data TypeError = TE {position :: BNFC'Position, error :: TypeErrors}

showSPrintable :: Print a => a -> ShowS -- TODO Change name to include types and Idents, maybe add '' around name
showSPrintable = showString . render . (prt 0)

showSIntT = showSPrintable (Int Nothing)
showsBoolT = showSPrintable (Bool Nothing)

showsNot :: Print a => a -> ShowS
showsNot t = showString ", not " . showSPrintable t

instance Show TypeErrors where
    showsPrec _ TEBreak                 = showString "'break' outside loop"
    showsPrec _ TEContinue              = showString "'continue' outside loop"
    showsPrec _ TEAssignExp             = showString "Cannot assign to expression"
    showsPrec _ (TEArrIndex t)          = showString "Array index must be " . showSIntT . showsNot t
    showsPrec _ (TECondtion t)          = showString "Condition must be " . showsBoolT . showsNot t
    showsPrec _ (TEForIter t)           = showString "For loop iterator must be " . showSIntT . showsNot t
    showsPrec _ (TEUnaryOp t)           = showString "Unary opeartion not supported for " . showSPrintable t
    showsPrec _ (TEReturn t1 t2)        = showString "Wrong type returned, should be " . showSPrintable t2 . showsNot t1
    showsPrec _ (TEAssignment t1 t2)    = showString "Cannot assign expression of type " . showSPrintable t2 . showString " to " . showSPrintable t1
    showsPrec _ (TEApplication t1 t2)   = showString "Function expected types (" . showSPrintable t2 .  showString "), not (" . showSPrintable t1 . showChar ')'
    showsPrec _ (TEBinaryOp t1 t2)      = showString "Binary opeartion not supported for types " . showSPrintable t1 . showString " and " . showSPrintable t2
    showsPrec _ (TEArrayType t1 t2)     = showString "Cannot make array of types " . showSPrintable t1 . showString " and " . showSPrintable t2
    showsPrec _ (TEMainSig t1 t2)       = showString "Function 'main' should have signature " .showSPrintable t2 . showsNot t1
    showsPrec _ (TEUndeclared i)        = showString "Identifier " . showSPrintable i . showString " not declared in this scope"
    showsPrec _ (TERedeclaration i)     = showString "Redeclartion of identifier " . showSPrintable i
    showsPrec _ (TEReadOnly i)          = showString "Cannot modify read-only variable " . showSPrintable i
    showsPrec _ (TENotFunction i)       = showSPrintable i . showString " is not a function"
    showsPrec _ (TENotArray i)          = showString "Subscripted value " . showSPrintable i . showString " is not an array"

showPosition :: BNFC'Position -> ShowS
showPosition Nothing = showString "unknown position. "
showPosition (Just (line, column)) = showString (printPosn $ Pn 0 line column) . showString ". "

instance Show TypeError where
    showsPrec _ (TE pos err) = showString "Typechecker error at " . showPosition pos . showsPrec 0 err . showChar '.'

makeError :: HasPosition a => a -> TypeErrors -> TypeError
makeError s t = TE (hasPosition s) t