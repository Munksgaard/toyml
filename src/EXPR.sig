signature EXPR =
sig
    datatype expr = Int of int
                  | Bool of bool
                  | Ident of Misc.ident
                  | Let of string * expr * expr
                  | Apply of expr * expr
                  | Lambda of string * expr
end
