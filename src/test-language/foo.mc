lang FooLang
  syn Primitive =
  | K Option
  | S (Option, Option)
  | Builtin String

  syn Term =
  | TmVar String
  | TmPrim Primitive
  | TmApp (Expr, Expr)

  sem isvalue =
  | TmVar _ -> true
  | TmPrim _ -> true
  | TmApp _ -> false

  sem applyPrimitive (tm : Term) =
  | K (None ()) -> TmPrim (K (Some tm))
  | K (Some t) -> t
  | S (None (), None ()) -> TmPrim (S (Some tm, None ()))
  | S (Some t, None ()) -> TmPrim (S (Some t, Some tm))
  | S (Some t1, Some t2) -> TmApp (TmApp (t1, tm), TmApp (t2, tm))
  | _ -> error "invalid primitive encountered"

  sem eval =
  | TmVar s -> TmVar s
  | TmPrim p -> TmPrim p
  | TmApp (tm1, tm2) & t ->
    match tm1 with TmPrim p then
      eval (applyPrimitive tm2 p)
    else match tm1 with TmApp _ then
      eval (TmApp (eval tm1, tm2))
    else
      t

  sem formatPrim =
  | K (None ()) -> "K()"
  | K (Some t) -> join ["K(", formatTm t, ")"]
  | S (None (), None ()) -> "S()"
  | S (Some t, None ()) -> join ["S(", formatTm t, ")"]
  | S (Some t1, Some t2) -> join ["S(", formatTm t1, ", ", formatTm t2, ")"]
  | _ -> error "invalid primitive encountered"

  sem formatTm =
  | TmVar s -> s
  | TmPrim p -> formatPrim p
  | TmApp (tm1, tm2) -> join ["(", formatTm tm1, " ", formatTm tm2, ")"]
end


mexpr

use FooLang in
let k = TmPrim (K (None ())) in
let s = TmPrim (S (None (), None ())) in
utest eval (TmApp (TmApp (TmApp (s, k), TmVar "hej"), TmVar "foo")) with TmVar "foo" in
()


-- I = id
-- K = const
-- S = ap               (S f g x = f x (g x))

-- x 	Variable 	A character or string representing a combinatory term.
-- P 	Primitive function 	One of the combinator symbols I, K, S.
-- (M N) 	Application 	Applying a function to an argument. M and N are combinatory terms.

-- let of some kind?
