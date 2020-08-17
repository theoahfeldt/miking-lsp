include "foo-parser.mc"
include "foo.mc"
include "parser.mc"

let parse_foo = lam file.
  let contents = readFile file in
  run_parser file program contents

let usage = lam _. printLn "Usage: mi main.mc -- <file>"

let main = lam _.
  if lti (length argv) 2 then
    usage ()
  else
    let parsed = parse_foo (get argv 1) in
    match parsed with Success (ast, _) then
      use FooLang in
      let result = eval ast in
      printLn (formatTm result)
    else printLn (show_error parsed)

mexpr

main ()
