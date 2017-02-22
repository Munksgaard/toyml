signature TYPE =
sig
    eqtype ty

    val Int : ty
    val Bool : ty
    val Var : Misc.ident -> ty
    val Fun : ty * ty -> ty

    val getType : Expr.expr -> ty
end
