structure Parse :> PARSE =
struct

open Misc
infix contains

datatype token = Int of int
               | Bool of bool
               | Let
               | In
               | End
               | Ident of string
               | Equal
               | Fn
               | LParen
               | RParen
               | RArrow

fun parseInt s =
  case (List.all Char.isDigit (explode s), Int.fromString s) of
      (true, SOME n) => SOME (Int n)
    | _ => NONE

fun parseKeyword k t s =
  if k = s then SOME t
  else NONE

fun parseIdent s =
  if List.all Char.isAlphaNum (explode s) then
      SOME (Ident s)
  else NONE

fun tryParsers x [] = raise Fail ("Could not parse " ^ x)
  | tryParsers x (f :: fs)  =
    case f x of
        SOME res => res
      | NONE => tryParsers x fs

fun parseToken s = tryParsers s [parseInt,
                                 parseKeyword "true" (Bool true),
                                 parseKeyword "false" (Bool false),
                                 parseKeyword "let" Let,
                                 parseKeyword "in" In,
                                 parseKeyword "end" End,
                                 parseKeyword "=" Equal,
                                 parseKeyword "fn" Fn,
                                 parseKeyword "(" LParen,
                                 parseKeyword ")" RParen,
                                 parseKeyword "=>" RArrow,
                                 parseIdent]

fun tokens s =
  map parseToken (String.tokens (Char.isSpace) s)

fun tokenString (Bool b) = Bool.toString b
  | tokenString (Int n) = Int.toString n
  | tokenString Let = "Let"
  | tokenString In = "In"
  | tokenString End = "End"
  | tokenString Equal = "="
  | tokenString Fn = "fn"
  | tokenString LParen = "("
  | tokenString RParen = ")"
  | tokenString RArrow = "=>"
  | tokenString (Ident s) = s

fun term (Fn :: (Ident id) :: RArrow :: rest) =
  (case term rest of
       SOME (e, rest') => SOME (Expr.Lambda (id, e), rest')
     | _ => NONE)
  | term (Let :: (Ident id) :: Equal :: rest) =
    (case term rest of
         SOME (e1, (In :: rest')) =>
         (case term rest' of
              SOME (e2, (End :: rest'')) =>
              SOME (Expr.Let (id, e1, e2), rest'')
            | _ => NONE)
       | _ => NONE)
  | term toks =
    case atom toks of
        SOME (e1, rest) =>
        (case atom rest of
             SOME (e2, rest') => SOME (Expr.Apply (e1, e2), rest')
           | NONE => SOME (e1, rest))
      | NONE => NONE
and atom (Ident s :: rest) = SOME (Expr.Ident s, rest)
  | atom (Int n :: rest) = SOME (Expr.Int n, rest)
  | atom (Bool b :: rest) = SOME (Expr.Bool b, rest)
  | atom (LParen :: rest) =
    (case term rest of
         SOME (e, RParen :: rest') =>
         SOME (e, rest')
       | _ => NONE )
  | atom toks = NONE

fun parse s =
    let val ts = tokens s
    in case term ts of
           SOME (e, []) => e
         | SOME (e, rest) => raise Fail ("Unexpected tokens: " ^
                                         String.concatWith
                                             " " (map tokenString rest))
         | NONE => raise Fail "No parse"
    end

end
