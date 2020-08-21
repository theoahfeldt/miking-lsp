include "parser.mc"
include "json.mc"
include "json-rpc.mc"
include "utils.mc"

let token = lex_token spaces
let lexString = compose token lex_string
let lexChar = compose token lex_char
let lexAnything = void (many (satisfy (const true) ""))

type HeaderField
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

let readBody = lam toRead.
  recursive let f = lam len. lam acc.
    match readBytesAsString len with (readLen, str) then
      let new_acc = concat acc str in
      if lti readLen len then
        f (subi len readLen) new_acc
      else
        new_acc
    else
      error "Not possible"
  in
  f toRead ""

let readRequests =
  let getLength = lam x.
    match x with ContentLength i then
      Some i
    else
      None ()
  in
  (optionCompose (processBatch jsonToRequest)
  (optionCompose (compose parseJson readBody)
  (optionCompose (optionFoldMap getLength)
  (compose       (optionMapM parseHeaderField)
                 readHeaderLines))))

let processRequests = lam _. Some ()
let putResponses = lam _. printLn "Hello World!"

recursive
let serverMain = lam _.
  let _ = (compose       (optionMap putResponses)
          (optionCompose processRequests
                         readRequests)) ()
  in serverMain ()
end
