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

-- string : String -> Parser String
let string = lam s. token (lex_string s)

-- symbol : String -> Parser String
let symbol = string

let is_valid_char = lam c.
  or (is_alphanum c) (eqchar c '_')

-- reserved : String -> Parser String
--
-- Parse a specific string and fail if it is followed by
-- additional valid identifier characters.
let reserved = lam s.
  void (token (apl (lex_string s) (not_followed_by (satisfy is_valid_char ""))))

-- number : Parser Int
let number = token lex_number

-- float : Parser Float
let float = token lex_float

-- parens : Parser a -> Parser a
let parens = wrapped_in (symbol "(") (symbol ")")

-- brackets : Parser a -> Parser a
let brackets = wrapped_in (symbol "[") (symbol "]")

-- char_lit : Parser Char
let char_lit = token lex_char_lit

-- string_lit : Parser String
let string_lit = token lex_string_lit

-- comma_sep : Parser a -> Parser [a]
let comma_sep = sep_by (symbol ",")

-- List of reserved keywords
let keywords = ["let", "in", "if", "true", "false"]

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
    -- let seq =
    --   let _ = debug "== Parsing seq ==" in
    --   fmap (lam x. TmSeq x) (brackets (comma_sep expr))
    -- in
    -- let tuple =
    --   let _ = debug "== Parsing tuple ==" in
    --   bind (parens (comma_sep expr)) (lam es.
    --   if null es
    --   then pure (TmConst (CUnit()))
    --   else if eqi (length es) 1
    --   then pure (head es)
    --   else pure (TmTuple es))
    -- in
    -- let num =
    --   let _ = debug "== Parsing num ==" in
    --   fmap (lam n. TmConst (CInt n)) number
    -- in
    -- let float =
    --   let _ = debug "== Parsing float ==" in
    --   fmap (lam f. TmConst (CFloat f)) float
    -- in
    -- let bool =
    --   let _ = debug "== Parsing bool ==" in
    --   alt (apr (reserved "true")  (pure (TmConst (CBool true))))
    --       (apr (reserved "false") (pure (TmConst (CBool false))))
    -- in
    -- let str_lit =
    --   let _ = debug "== Parsing string ==" in
    --   bind string_lit (lam s.
    --   pure (TmSeq (map (lam c. TmConst (CChar c)) s)))
    -- in
    -- let chr_lit =
    --   let _ = debug "== Parsing character ==" in
    --   fmap (lam c. TmConst (CChar c)) char_lit
    -- in
      label "atomic expression" var_access
      -- (alt seq
      -- (alt tuple
      -- (alt (try float)
      -- (alt num
      -- (alt bool
      -- (alt str_lit chr_lit)
      st

  -- expr: Parser Expr
  --
  -- Main expression parser.
  let expr = lam st.
    use FooLang in
    -- left : Parser Expr
    --
    -- Left recursive expressions, i.e. function application
    -- and tuple projection.
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
    -- let if_ =
    --   let _ = debug "== Parsing if ==" in
    --   bind (reserved "if") (lam _.
    --   bind expr (lam cnd.
    --   bind (reserved "then") (lam _.
    --   bind expr (lam thn.
    --   bind (reserved "else") (lam _.
    --   bind expr (lam els.
    --   pure (TmIf(cnd, thn, els))))))))
    -- in
    label "expression"
    (alt left
         let_) st
    -- (alt let_
    -- (alt if_
end
-- program : Parser Expr
let program = apl (apr ws expr) end_of_input
