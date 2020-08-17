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
      let _ = print "Evaluating: " in
      let _ = printLn (formatTm ast) in
      let result = eval builtins ast in
      match result with Some t then
        printLn (formatTm t)
      else
        printLn ("Stuck term")
    else printLn (show_error parsed)

mexpr

main ()
