include "map.mc"
include "string.mc"

lang FooLang
  syn Primitive =
  | PId ()
  | PCompose ()
  | PConst ()
  | PAp ()
  | PJoin ()
  | PFlip ()
  | POn ()

  syn Term =
  | TmVar String
  | TmPrim (Primitive, [Term])
  | TmLet (String, Term, Term)
  | TmApp (Term, Term)

  sem isvalue =
  | TmVar _ -> true
  | TmPrim _ -> true
  | TmLet _ -> false
  | TmApp _ -> false

  sem applyPrimitive (tm : Term) =
  | TmPrim (PId (), []) -> tm
  | TmPrim (PCompose (), [t1, t2]) -> TmApp (t1, TmApp (t2, tm))
  | TmPrim (PConst (), [t]) -> t
  | TmPrim (PAp (), [t1, t2]) -> TmApp (TmApp (t1, tm), TmApp (t2, tm))
  | TmPrim (PJoin (), [t]) -> TmApp (TmApp (t, tm), tm)
  | TmPrim (PFlip (), [t1, t2]) -> TmApp (TmApp (t1, tm), t2)
  | TmPrim (POn (), [t1, t2, t3]) -> TmApp (TmApp (t1, TmApp (t2, t3)), TmApp (t2, tm))
  | TmPrim (p, args) -> TmPrim (p, (snoc args tm))

  sem eval (env : [(String, Term)]) =
  | TmVar s ->
    optionBind (index (lam x. eqstr s x.0) env) (lam i.
    let old_env = (splitAt env i).1 in
    match old_env with [(_, tm)] ++ rest then
      eval rest tm
    else
      None ())
  | TmPrim _ & t -> Some t
  | TmApp (tm1, tm2) & t ->
    match tm1 with TmPrim _ then
      eval env (applyPrimitive tm2 tm1)
    else
      optionBind (eval env tm1) (lam t.
      eval env (TmApp (t, tm2)))
  | TmLet (name, expr, body) -> eval (cons (name, expr) env) body

  sem formatPrim =
  | PId () -> "id"
  | PCompose () -> "compose"
  | PConst () -> "const"
  | PAp () -> "ap"
  | PJoin () -> "join"
  | PFlip () -> "flip"
  | POn () -> "on"

  sem formatTmPrec (prec : Int) =
  | TmVar s -> s
  | TmPrim (p, args) -> strJoin " " (cons (formatPrim p) (map (formatTmPrec 1) args))
  | TmApp (tm1, tm2) ->
    let inner = join [formatTm tm1, " ", formatTmPrec 1 tm2] in
    if gti prec 0 then
      join ["(", inner, ")"]
    else
      inner
  | TmLet (n, expr, body) ->
    strJoin " " ["let", n, "=", formatTm expr, "in", formatTm body]

  sem formatTm =
  | tm -> formatTmPrec 0 tm
end

let builtins =
    use FooLang in
    map (lam x. (formatPrim x, TmPrim (x, [])))
      [ PId ()
      , PCompose ()
      , PConst ()
      , PAp ()
      , PJoin ()
      , PFlip ()
      , POn ()
      ]
