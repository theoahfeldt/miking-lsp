include "parser.mc"
include "foo.mc"

-- Parser for FooLang, based on stdlib/mexpr/mcore_parser.mc

-- Tokens ----------------------------------

-- line_comment : Parser ()
--
-- Parse a line comment, ignoring its contents.
let line_comment =
  void (apr (apr (lex_string "--")
                 (many (satisfy (compose not (eqchar '\n')) "")))
            (alt (lex_string "\n") end_of_input))

-- ws : Parser ()
--
-- Parse whitespace or comments.
let ws = void (many (alt line_comment spaces1))

-- token : Parser a -> Parser a
--
-- `token p` parses `p` and any trailing whitespace or comments.
let token = lex_token ws

-- symbol : String -> Parser String
let symbol = lam s. token (lex_string s)

let is_valid_char = lam c.
  or (is_alphanum c) (eqchar c '_')

-- reserved : String -> Parser String
--
-- Parse a specific string and fail if it is followed by
-- additional valid identifier characters.
let reserved = lam s.
  void (token (apl (lex_string s) (not_followed_by (satisfy is_valid_char ""))))

-- number : Parser Int
let number = token (alt (apr (lex_char '-') (fmap negi lex_number)) lex_number)

-- parens : Parser a -> Parser a
let parens = wrapped_in (symbol "(") (symbol ")")

-- string_lit : Parser String
let string_lit = token lex_string_lit

-- List of reserved keywords
let keywords = ["let", "in"]

-- ident : Parser String
--
-- Parse an identifier, but require that it is not in the list
-- of reserved keywords.
let identifier =
  let valid_id =
    bind (satisfy (lam c. or (is_alpha c) (eqchar '_' c)) "valid identifier") (lam c.
    bind (token (many (satisfy is_valid_char ""))) (lam cs.
    pure (cons c cs)))
  in
  try (
    bind valid_id (lam x.
    if any (eqstr x) keywords
    then fail (concat (concat "keyword '" x) "'") "identifier"
    else pure x)
  )

-- FooLang parsers ----------------------------------------

recursive
-- atom : Parser Expr
--
-- Innermost expression parser.
  let atom = lam st.
    use FooLang in
    let var_access =
      let _ = debug "== Parsing var_access ==" in
      fmap (lam x. TmVar x) identifier
    in
    let paren_expr =
      let _ = debug "== Parsing parens ==" in
      parens (alt expr (pure (TmPrim (PUnit (), []))))
    in
    let num =
      let _ = debug "== Parsing num ==" in
      fmap (lam n. TmPrim (PInt n, [])) number
    in
    let str_lit =
      let _ = debug "== Parsing string ==" in
      fmap (lam s. TmPrim (PString s, [])) string_lit
    in
      label "atomic expression"
      (alt var_access
      (alt paren_expr
      (alt num
           str_lit)))
      st

  -- expr: Parser Expr
  --
  -- Main expression parser.
  let expr = lam st.
    use FooLang in
    let left =
      bind (many1 atom) (lam as.
      pure (foldl1 (curry (lam x. TmApp x)) as))
    in
    let let_ =
      let _ = debug "== Parsing let ==" in
      bind (reserved "let") (lam _.
      bind identifier (lam x.
      bind (symbol "=") (lam _.
      bind expr (lam e.
      bind (reserved "in") (lam _.
      bind expr (lam body.
      pure (TmLet(x, e, body))))))))
    in
    label "expression"
    (alt left
         let_) st
end

-- program : Parser Expr
let program = apl (apr ws expr) end_of_input
