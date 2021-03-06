signature TYPE =
sig
    eqtype ty

    val Int : ty
    val Bool : ty
    val Var : Expr.ident -> ty
    val Fun : ty * ty -> ty

    val getType : Expr.expr -> ty

    val toString : ty -> string
end
