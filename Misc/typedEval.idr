data Ty = INT | BOOL

interp : Ty -> Type
interp INT  = Int
interp BOOL = Bool

data Expr : Ty -> Type where
    CstI : Int -> Expr INT
    CstB : Bool -> Expr BOOL
    Add  : Expr INT -> Expr INT -> Expr INT
    LessThan : Expr INT -> Expr INT -> Expr Bool
    
eval : Expr t -> interp t
eval (CstI x)   = x
eval (CstB x)   = x
eval (Add x y)  = eval x + eval y
eval (LessThan x y) = eval x < eval y
