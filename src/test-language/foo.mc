include "map.mc"
include "string.mc"

lang FooLang
  syn Primitive =
  | PId ()
  | PCompose (Option, Option)
  | PConst Option
  | PAp (Option, Option)
  | PJoin Option
  | PFlip (Option, Option)
  | POn (Option, Option, Option)

  syn Term =
  | TmVar String
  | TmPrim Primitive
  | TmLet (String, Term, Term)
  | TmApp (Term, Term)

  sem isvalue =
  | TmVar _ -> true
  | TmPrim _ -> true
  | TmLet _ -> false
  | TmApp _ -> false

  sem applyPrimitive (tm : Term) =
  | PId () -> tm
  | PCompose (None (), None ()) -> TmPrim (PCompose (Some tm, None ()))
  | PCompose (Some t, None ()) -> TmPrim (PCompose (Some t, Some tm))
  | PCompose (Some t1, Some t2) -> TmApp (t1, TmApp (t2, tm))
  | PConst (None ()) -> TmPrim (PConst (Some tm))
  | PConst (Some t) -> t
  | PAp (None (), None ()) -> TmPrim (PAp (Some tm, None ()))
  | PAp (Some t, None ()) -> TmPrim (PAp (Some t, Some tm))
  | PAp (Some t1, Some t2) -> TmApp (TmApp (t1, tm), TmApp (t2, tm))
  | PJoin (None ()) -> TmPrim (PJoin (Some tm))
  | PJoin (Some t) -> TmApp (TmApp (t, tm), tm)
  | PFlip (None (), None ()) -> TmPrim (PFlip (Some tm, None ()))
  | PFlip (Some t, None ()) -> TmPrim (PFlip (Some t, Some tm))
  | PFlip (Some t1, Some t2) -> TmApp (TmApp (t1, tm), t2)
  | POn (None (), None (), None ()) -> TmPrim (POn (Some tm, None (), None ()))
  | POn (Some t, None (), None ()) -> TmPrim (POn (Some t, Some tm, None ()))
  | POn (Some t1, Some t2, None ()) -> TmPrim (POn (Some t1, Some t2, Some tm))
  | POn (Some t1, Some t2, Some t3) -> TmApp (TmApp (t1, TmApp (t2, t3)), TmApp (t2, tm))

  sem eval (env : [(String, Term)]) =
  | TmVar s -> mapLookupOpt eqstr s env
  | TmPrim p -> Some (TmPrim p)
  | TmApp (tm1, tm2) & t ->
    match tm1 with TmPrim p then
      eval env (applyPrimitive tm2 p)
    else
      optionBind (eval env tm1) (lam t.
      eval env (TmApp (t, tm2)))
  | TmLet (name, expr, body) -> eval (cons (name, expr) env) body

  sem formatPrim =
  | PId () -> "id"
  | PCompose (None (), None ()) -> "compose"
  | PCompose (Some t, None ()) -> join ["compose (", formatTm t, ")"]
  | PCompose (Some t1, Some t2) -> join ["compose (", formatTm t1, ") (", formatTm t2, ")"]
  | PConst (None ()) -> "const"
  | PConst (Some t) -> join ["const (", formatTm t, ")"]
  | PAp (None (), None ()) -> "ap"
  | PAp (Some t, None ()) -> join ["ap (", formatTm t, ")"]
  | PAp (Some t1, Some t2) -> join ["ap (", formatTm t1, ") (", formatTm t2, ")"]
  | PJoin (None ()) -> "join"
  | PJoin (Some t) -> join ["join (", formatTm t, ")"]
  | PFlip (None (), None ()) -> "flip"
  | PFlip (Some t, None ()) -> join ["flip (", formatTm t, ")"]
  | PFlip (Some t1, Some t2) -> join ["flip (", formatTm t1, ") (", formatTm t2, ")"]
  | POn (None (), None (), None ()) -> "on"
  | POn (Some t, None (), None ()) -> join ["on (", formatTm t, ")"]
  | POn (Some t1, Some t2, None ()) -> join ["on (", formatTm t1, ") (", formatTm t2, ")"]
  | POn (Some t1, Some t2, Some t3) -> join ["on (", formatTm t1, ") (", formatTm t2, ") (", formatTm t3, ")"]

  sem formatTm =
  | TmVar s -> s
  | TmPrim p -> formatPrim p
  | TmApp (tm1, tm2) ->
    match tm2 with TmApp _ then
      join [formatTm tm1, " (", formatTm tm2, ")"]
    else
      join [formatTm tm1, " ", formatTm tm2]
  | TmLet (n, expr, body) ->
    strJoin " " ["let", n, "=", formatTm expr, "in", formatTm body]
end

let builtins =
    use FooLang in
    [ ("id", TmPrim (PId ()))
    , ("compose", TmPrim (PCompose (None (), None ())))
    , ("const", TmPrim (PConst (None ())))
    , ("ap", TmPrim (PAp (None (), None ())))
    , ("join", TmPrim (PJoin (None ())))
    , ("flip", TmPrim (PFlip (None (), None ())))
    , ("on", TmPrim (POn (None (), None (), None ())))
    ]

mexpr

use FooLang in
let k = TmPrim (K (None ())) in
let s = TmPrim (S (None (), None ())) in
utest eval [] (TmApp (TmApp (TmApp (s, k), TmVar "hej"), TmVar "foo")) with TmVar "foo" in
()


-- I = id
-- K = const
-- S = ap               (S f g x = f x (g x))

-- x 	Variable 	A character or string representing a combinatory term.
-- P 	Primitive function 	One of the combinator symbols I, K, S.
-- (M N) 	Application 	Applying a function to an argument. M and N are combinatory terms.

-- let of some kind?
