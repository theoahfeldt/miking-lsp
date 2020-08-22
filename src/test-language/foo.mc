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
  | PUnit ()
  | PInt Int
  | PAdd ()
  | PMul ()
  | PGt ()
  | PString String
  | PConcat ()
  | PPrint ()

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

  sem applyPrimitive (env : [(String, Term)]) (tm : Term) =
  | TmPrim (PUnit _, _)
  | TmPrim (PInt _, _)
  | TmPrim (PString _, _) -> None ()
  | TmPrim (PId (), []) -> Some tm
  | TmPrim (PCompose (), [t1, t2]) -> Some (TmApp (t1, TmApp (t2, tm)))
  | TmPrim (PConst (), [t]) -> Some t
  | TmPrim (PAp (), [t1, t2]) -> Some (TmApp (TmApp (t1, tm), TmApp (t2, tm)))
  | TmPrim (PJoin (), [t]) -> Some (TmApp (TmApp (t, tm), tm))
  | TmPrim (PFlip (), [t1, t2]) -> Some (TmApp (TmApp (t1, tm), t2))
  | TmPrim (POn (), [t1, t2, t3]) -> Some (TmApp (TmApp (t1, TmApp (t2, t3)), TmApp (t2, tm)))
  | TmPrim (PAdd (), [t]) ->
    optionBind (eval env t) (lam r1.
    optionBind (eval env tm) (lam r2.
    match (r1,r2) with (TmPrim (PInt i1, []), TmPrim (PInt i2, [])) then
      Some (TmPrim (PInt (addi i1 i2), []))
    else None ()
    ))
  | TmPrim (PMul (), [t]) ->
    optionBind (eval env t) (lam r1.
    optionBind (eval env tm) (lam r2.
    match (r1,r2) with (TmPrim (PInt i1, []), TmPrim (PInt i2, [])) then
      Some (TmPrim (PInt (muli i1 i2), []))
    else None ()
    ))
  | TmPrim (PGt (), [t]) ->
    optionBind (eval env t) (lam r1.
    optionBind (eval env tm) (lam r2.
    match (r1,r2) with (TmPrim (PInt i1, []), TmPrim (PInt i2, [])) then
      if gti i1 i2 then
        Some (TmPrim (PConst (), []))
      else
        Some (TmPrim (PFlip (), [TmPrim (PConst (), [])]))
    else None ()
    ))
  | TmPrim (PConcat (), [t]) ->
    optionBind (eval env t) (lam r1.
    optionBind (eval env tm) (lam r2.
    match (r1,r2) with (TmPrim (PString s1, []), TmPrim (PString s2, [])) then
      Some (TmPrim (PString (concat s1 s2), []))
    else None ()
    ))
  | TmPrim (PPrint (), []) ->
    optionBind (eval env tm) (lam r.
    match r with TmPrim (PString s, []) then
      let _ = printLn s in
      Some (TmPrim (PUnit (), []))
    else None ()
    )
  | TmPrim (p, args) -> Some (TmPrim (p, (snoc args tm)))

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
      optionBind (applyPrimitive env tm2 tm1) (eval env)
    else
      optionBind (eval env tm1) (lam t.
      eval env (TmApp (t, tm2)))
  | TmLet (name, expr, body) -> eval (cons (name, expr) env) body

  sem formatPrim =
  | PId () -> "id"
  | PCompose () -> "<."
  | PConst () -> "const"
  | PAp () -> "ap"
  | PJoin () -> "join"
  | PFlip () -> "~"
  | POn () -> "on"
  | PInt i -> int2string i
  | PAdd () -> "add"
  | PMul () -> "mul"
  | PGt () -> "gt"
  | PString s -> cons '"' (snoc s '"')
  | PConcat () -> "++"
  | PPrint () -> "print"
  | PUnit () -> "()"

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
      , PAdd ()
      , PMul ()
      , PGt ()
      , PConcat ()
      , PPrint ()
      ]
