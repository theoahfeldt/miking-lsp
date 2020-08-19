include "parser.mc"
include "json.mc"
include "json-rpc.mc"
include "utils.mc"

let token = lex_token spaces
let lexString = compose token lex_string
let lexChar = compose token lex_char
let lexAnything = void (many (satisfy (const true) ""))

con ContentLength: Int -> HeaderField
con ContentType: () -> HeaderField

let parseHeaderField = lam s.
  let len = apr (lexString "Length")
           (apr (lexChar ':')
                (fmap (lam x. ContentLength x) (token lex_int)))
  in
  let tpe = apr (lexString "Type")
           (apr (lexChar ':')
                (fmap (lam x. ContentType x) lexAnything))
  in
  let headerField = apr spaces
                   (apr (lexString "Content-")
                   (apl (alt len tpe)
                        end_of_input))
  in match (run_parser "" headerField s) with Success (hdr, pos) then
    Some hdr
  else
    None ()

let matchNewline = lam line.
  match line with "" then
    None ()
  else
    Some (line, ())

let readHeaderLines = unfoldr (compose matchNewline readLine)

let readRequests =
  optionBind (optionMapM parseHeaderField (readHeaderLines ())) (lam headers.
  let len = optionFoldMap (lam x. match x with ContentLength i then Some i else None ()) headers in
  optionBind len (compose parseJson readBytes))

let readRequests =
  recursive let helper =
    let line = readLine () in
    let new_acc = concat acc line in
    match parseJson new_acc with Some json then
       processBatch jsonToRequest json
    else
      helper new_acc
  in helper ""

let serverMain = lam _.
  let requests = readRequests () in
  let responses = processRequests requests in
  let _ = putResponse responses in
  serverMain ()

let zerverMain = serverMain, but spelled with a z
