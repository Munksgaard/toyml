(* Tests *)

local open Expr val getType = Type.getType in
val test_getType_Int_1 = getType (Int 4) = Type.Int;

val test_getType_Bool_1 = getType (Bool true) = Type.Bool;

val test_getType_Ident_1 =
    getType (Let ("x", Int 42, Ident "x")) = Type.Int;

val test_getType_Ident_2 =
    (getType (Ident "y"); false) handle _ => true;

val test_getType_Let_1 = getType (Let ("x", Int 4, Bool true)) = Type.Bool;

val test_getType_Let_2 =
    getType (Let ("x", Bool true, Ident "x")) = Type.Bool;

val test_getType_Apply_1 =
    getType (Let ("f", Lambda ("x", Bool true), Apply (Ident "f", Int 42))) =
    Type.Bool;

val test_getType_Apply_2 =
    (getType (Apply (Ident "f", Int 42)); false)
    handle _ => true;

val test_getType_Apply_3 =
    (getType (Apply (Ident "y", Int 42)); false)
    handle _ => true;

val testFromPaper0 =
    getType (Let ("id", (Lambda ("x", Ident "x")), Ident "id"));

val testFromPaper1 =
    getType (Let ("id", (Lambda ("x", Ident "x")), Apply (Ident "id", Ident "id")));

val testFromPaper2 =
    getType (Let ("id", (Lambda ("x", Let ("y", Ident "x", Ident "y"))),
                  Apply (Ident "id", Ident "id")));

val testFromPaper3 =
    getType (Let ("id", (Lambda ("x", Let ("y", Ident "x", Ident "y"))),
                  Apply (Apply (Ident "id", Ident "id"), Int 2)));

val testFromPaper4 =
    (getType (Let ("id", (Lambda ("x", Apply (Ident "x", Ident "x"))),
                   Ident "id")); false)
    handle _ => true;

val testFromPaper5 =
    getType (Lambda ("m", Let ("y", Ident "m",
                               Let ("x", Apply (Ident "y", Bool true),
                                    Ident "x"))));

(* From https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Free_type_variables *)
val testFromWikipedia =
    getType (Let ("bar",
                  Lambda ("x",
                          Let ("foo",
                               Lambda ("y", Ident "x"),
                               Ident "foo")),
                  Ident "bar"));

end
