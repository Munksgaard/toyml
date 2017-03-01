structure Type :> TYPE =
struct

open Misc
infix contains
infix withoutAll

datatype ty = Int
            | Bool
            | Var of ident
            | Fun of ty * ty

type substitution = (ident * ty)

type tyScheme = ident list * ty

type tyEnv = (ident * tyScheme) list

(* freevars : ty -> ident list
 * Lists the free variables in ty. *)
fun freevars Int = []
  | freevars Bool = []
  | freevars (Var id) = [id]
  | freevars (Fun (t1, t2)) = freevars t1 @ freevars t2

(* apply : substitution list -> ty -> ty
 * Applies the substitutions in subs to variables in the type ty *)
fun apply _ Int = Int
  | apply _ Bool  = Bool
  | apply subs (Fun (l, r)) =
    Fun (apply subs l, apply subs r)
  | apply subs (Var s) =
    case lookup subs s of
        SOME v => v
      | NONE => Var s

(* compose : substitution list -> substitution list -> substitution list
 * composes two substitutions into one. *)
fun compose s1 s2 =
  s1 @ (map (fn (s, v) => (s, apply s1 v)) s2)

(* varBind : ident -> ty -> substitution list option
 * Tries to bind ty to the type variable given by id.
 * If ty is a type variable, simply perform the binding, otherwise perform
 * the occurs check: If id is a free variable in ty, then we'll get a
 * recursive substitution, which we consider an error. *)
fun varBind id ty =
  case ty of
      Var id' => if id = id' then SOME []
                else SOME [(id, ty)]
    | _ => if (freevars ty) contains id then
             NONE
           else
             SOME [(id, ty)]

(* mgu : ty -> ty -> substitution list option
 * Find the most general substitution that unifies t1 and t2. *)
fun mgu (Fun (l, r)) (Fun (l', r')) =
  let val s1 = mgu l l'
      val s2 = Option.map (fn s => mgu (apply s r) (apply s r')) s1
  in case (s1, Option.join s2) of
         (SOME s1', SOME s2') => SOME (compose s1' s2')
       | _ => NONE
  end
  | mgu (Var u) t = varBind u t
  | mgu t (Var u) = varBind u t
  | mgu Int Int = SOME []
  | mgu Bool Bool = SOME []
  | mgu _ _ = NONE


(* freevarsScheme : tyScheme -> ident list
 * Find the free variables in the given type scheme. *)
fun freevarsScheme (ids, t) =
  freevars t withoutAll ids

local
  val counter = ref 0
in
(* getFresh : unit -> ty
 * Returns a fresh and unique type variable *)
fun getFresh () =
  let val i = !counter
      val () = counter := i + 1
  in Var ("tyVar_" ^ Int.toString i) end
end

(* instantiate : tyScheme -> ty
 * Create a fresh type from a given type scheme. *)
fun instantiate (ids, ty) =
  let val subs = map (fn id => (id, getFresh ())) ids
  in apply subs ty end

(* applyScheme : substitution list -> tyScheme -> tyScheme
 * Apply substitutions to the given type scheme. *)
fun applyScheme subs (tyvars, t) =
  (* Remove all substitutions from subs that are in tyvars *)
  let val subs' = List.filter (fn (id, _) => not (tyvars contains id)) subs
  in (tyvars, apply subs' t) end

(* freevarsEnv : tyEnv -> ident list
 * Find the free variables in the given type environment. *)
fun freevarsEnv env =
  List.concat (map (fn (_, ts) => freevarsScheme ts) env)

(* generalize : tyEnv -> ty -> tyScheme
 * Generalizes a type into a type scheme in the given type environment.
   For instance, the concrete type `'a -> 'a`, generalizes to
   the type scheme `forAll 'a : 'a -> 'a`, but only if if `'a` is not
   also free in the type environment.
   Generalizing is the opposite of instantiating. *)
fun generalize env ty =
  (freevars ty withoutAll freevarsEnv env, ty)

(* applyEnv : substitution list -> tyEnv -> tyEnv
 * Apply the given substitutions to a type environment. *)
fun applyEnv subs env =
  map (fn (id, ts) => (id, applyScheme subs ts)) env

(* infer : tyEnv -> expr -> substitution list * ty
 * Infer the type of an expression in the given type environment. *)
fun infer env (Expr.Lambda (x, e)) : (substitution list * ty) =
  let val tv = getFresh ()
      val env' = List.filter (fn (y, _) => x <> y) env
      val env'' = (x, ([], tv)) :: env'
      val (s1, t1) = infer env'' e
  in (s1, Fun (apply s1 tv, t1)) end
  | infer env (Expr.Apply (e1, e2)) =
    let val (s1, t1) = infer env e1
        val (s2, t2) = infer (applyEnv s1 env) e2
        val fresh = getFresh ()
        val s3 = case mgu (apply s2 t1) (Fun (t2, fresh)) of
                     SOME s => s
                   | NONE => raise Fail "No unifier found"
    in (compose s3 (compose s2 s1), apply s3 fresh)
    end
  | infer env (Expr.Let (x, e1, e2)) =
    let val (s1, t1) = infer env e1
        val env' = List.filter (fn (y, _) => x <> y) env
        val t' = generalize (applyEnv s1 env)
                            t1
        val env'' = (x, t') :: env'
        val (s2, t2) = infer (applyEnv s1 env'') e2
    in (compose s2 s1, t2) end
  | infer _ (Expr.Int _) = ([], Int)
  | infer _ (Expr.Bool _) = ([], Bool)
  | infer env (Expr.Ident x) =
    case lookup env x of
        SOME ts => ([], instantiate ts)
      | NONE => raise Fail "Unbound var"

(* getType : expr -> ty
 * Returns the concrete type of a given expression. *)
fun getType e =
  let val (s, t) = infer [] e
  in apply s t end

(* toString : ty -> string
 * Returns a string representation of the given type. *)
fun toString (Var id) = "'" ^ id
  | toString Bool = "bool"
  | toString Int = "int"
  | toString (Fun (t1, t2)) = toString t1 ^ " -> " ^ toString t2

end
