module TypecheckerError where 

import AbsHawat ( Type, Ident, BNFC'Position, Type'(Int, Bool), Stmt, HasPosition, hasPosition )
import PrintHawat ( prt, render, concatS, Print )
import LexHawat ( printPosn, Posn( Pn ) )


data TypeErrors = TEBreak                       | -- Break not in a loop
                  TEContinue                    | -- Continue not in a loop
                  TEArrIndex Type               | -- Array indexed not by an int
                  TECondtion Type               | -- Condtion not a bool
                  TEForIter Type                | -- For iterator is not an int
                  TEUnaryOp Type                | -- Type not supported for unary operation
                  TEReturn Type Type            | -- Wrong return type (returned, expected)
                  TEAssignment Type Type        | -- Wrong type assigneed (lvalue, rvalue)
                  TEApplication [Type] [Type]   | -- Wrong function application (applied, expected)
                  TEBinaryOp Type Type          | -- Types not supported for binary operation (left, right)
                  TEUndeclared Ident            | -- Undeclared variable
                  TERedeclaration Ident         | -- Redeclartion of a variable
                  TEReadOnly Ident                -- Modification of read-only variable (usually in a for loop)


data TypeError = TE {position :: BNFC'Position, error :: TypeErrors}


showType :: Print a => a -> ShowS -- TODO Change name to include types and Idents, maybe add '' around name
showType = showString . render . (prt 0)

showInt = showType (Int Nothing)
showBool = showType (Bool Nothing)

showNot :: Print a => a -> ShowS
showNot t = showString ", not " . showType t

instance Show TypeErrors where
    showsPrec _ TEBreak                 = showString "'break' outside loop"
    showsPrec _ TEContinue              = showString "'continue' outside loop"
    showsPrec _ (TEArrIndex t)          = showString "Array index must be " . showInt . showNot t
    showsPrec _ (TECondtion t)          = showString "Condition must be " . showBool . showNot t
    showsPrec _ (TEForIter t)           = showString "For loop iterator must be " . showInt . showNot t
    showsPrec _ (TEUnaryOp t)           = showString "Unary opeartion not supported for " . showType t
    showsPrec _ (TEReturn t1 t2)        = showString "Wrong type returned, should be " . showType t2 . showNot t1
    showsPrec _ (TEAssignment t1 t2)    = showString "Cannot assign expression of type " . showType t2 . showString " to " . showType t1
    showsPrec _ (TEApplication t1 t2)   = showString "Function expected types " . showType t2 . showNot t1
    showsPrec _ (TEBinaryOp t1 t2)      = showString "Binary opeartion not supported for types " . showType t1 . showString " and " . showType t2
    showsPrec _ (TEUndeclared i)        = showString "Variable " . showType i . showString " not declared in this scope"
    showsPrec _ (TERedeclaration i)     = showString "Redeclartion of identifier " . showType i
    showsPrec _ (TEReadOnly i)          = showString "Cannot modify read-only variable " . showType i

showPosition :: BNFC'Position -> ShowS
showPosition Nothing = showString "unknown position. "
showPosition (Just (line, column)) = showString (printPosn $ Pn 0 line column) . showString ". "

instance Show TypeError where
    showsPrec _ (TE pos err) = showString "Typechecker error at " . showPosition pos . showsPrec 0 err . showChar '.'

makeError :: HasPosition a => a -> TypeErrors -> TypeError
makeError s t = TE (hasPosition s) t 