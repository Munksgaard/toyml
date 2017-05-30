signature EXPR =
sig
    type ident = string
    datatype expr = Int of int
                  | Bool of bool
                  | Ident of ident
                  | Let of string * expr * expr
                  | Apply of expr * expr
                  | Lambda of string * expr
end
