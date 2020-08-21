include "parser.mc"
include "json.mc"
include "json-rpc.mc"
include "utils.mc"

let lexAnything = void (many (satisfy (const true) ""))

type HeaderField
con ContentLength: Int -> HeaderField
con ContentType: () -> HeaderField

let parseHeaderField = lam s.
  let len = (apr (lex_string "Length: ")
                 (fmap (lam x. ContentLength x) lex_number))
  in
  let tpe = (apr (lex_string "Type: ")
                 (fmap (lam x. ContentType x) lexAnything))
  in
  let headerField = (apr (lex_string "Content-")
                    (apl (alt len tpe)
                         end_of_input))
  in match (run_parser "" headerField s) with Success (hdr, pos) then
    Some hdr
  else
    None ()

utest parseHeaderField "Content-Length: 50" with Some (ContentLength 50)
utest parseHeaderField "Content-Type:   sh13ETRNU-upsht!!rntdTEFrty" with Some (ContentType ())
utest parseHeaderField "Content-Length:  50" with None ()
utest parseHeaderField "Content-Length: 50hej" with None ()
utest parseHeaderField " Content-Type: sh13ETRNU-upsht!!rntdTEFrty" with None ()

let matchNewline = lam line.
  match line with "\n" then
    None ()
  else match line with str ++ "\n" then
    Some (str, ())
  else error "Unexpected end of input"

let readHeaderLines = unfoldr (compose matchNewline readLine)

let readBody = lam toRead.
  recursive let f = lam len. lam acc.
    let readResult = readBytesAsString len in
    let new_acc = concat acc readResult.0 in
    if lti readResult.1 len then
      f (subi len readResult.1) new_acc
    else
      new_acc
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

let processRequests = lam jsonRpcLst.
    Some (join (map (compose formatJson requestToJson) jsonRpcLst))
let putResponses = printLn

recursive
let serverMain = lam _.
  let _ = (compose       (optionMap putResponses)
          (optionCompose processRequests
                         readRequests)) ()
  in serverMain ()
end
