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
  let getLength = lam x.
    match x with ContentLength i then
      Some i
    else
      None ()
  in
  (optionCompose (processBatch jsonToRequest)
  (optionCompose (compose parseJson readBytes)
  (optionCompose (optionFoldMap getLength)
  (compose       (optionMapM parseHeaderField)
                 readHeaderLines))))

let serverMain =
  let _ = (optionCompose putResponses
          (optionCompose processRequests
                         readRequests)) ()
  in serverMain ()
