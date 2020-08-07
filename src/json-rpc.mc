include "json.mc"
include "map.mc"
include "utils.mc"

type Id
con StrId : String -> Id
con IntId : Int    -> Id

type Params
con ByPosition : [ JsonValue ]           -> Params
con ByName     : [ (String, JsonValue) ] -> Params

type RpcError = { code    : Int
                , message : String
                , data    : Option -- JsonValue
                }

type RpcResult
con Success : JsonValue -> RpcResult
con Failure : RpcError  -> RpcResult

type RpcRequest = { method : String
                  , params : Option -- Params
                  , id : Option -- Id
                  }
type RpcResponse = { result : RpcResult
                   , id : Id
                   }

let paramsToJson = lam x.
  match x with ByPosition arr then JsonArray arr
  else match x with ByName obj then JsonObject obj
  else error "paramsToJson: Arg is not of type Params"

let idToJson = lam x.
  match x with StrId s then JsonString s
  else match x with IntId i then JsonInt i
  else error "idToJson: Arg is not of type Id"

let jsonrpc = ("jsonrpc", JsonString "2.0")

let requestToJson = lam x.
  let members =
    optionSnoc
      (optionSnoc
        [ jsonrpc , ("method", JsonString x.method) ]
        (optionMap (lam x. ("params", paramsToJson x)) x.params))
      (optionMap (lam x. ("id", idToJson x)) x.id)
  in
  JsonObject members

let errorToJson = lam x.
  let members =
    optionSnoc [ ("code", JsonInt x.code)
               , ("message", JsonString x.message)
               ]
               (optionMap (lam x. ("data", x)) x.data)
  in
  JsonObject members

let responseToJson = lam x.
  let resultOrError =
    match x.result with Success v then
      ("result", v)
    else match x.result with Failure err then
      ("error", errorToJson err)
    else error "responseToJson: Invalid result"
  in
  let members = [ jsonrpc
                , resultOrError
                , ("id", idToJson x.id)
                ]
  in
  JsonObject members

let jsonToParams = lam x.
  match x with JsonObject o then
    Some (ByName o)
  else match x with JsonArray a then
    Some (ByPosition a)
  else
    None ()

let jsonToId = lam x.
  match x with JsonString s then
    Some (StrId s)
  else match x with JsonInt i then
    Some (IntId i)
  else
    None ()

let getJsonInt = lam x.
  match x with JsonInt i then Some i else None ()

let getJsonString = lam x.
  match x with JsonString s then Some s else None ()

let getObjectMapping = lam x.
  match x with JsonObject arr then
    Some (lam k. mapLookupOpt eqstr k arr)
  else
    None ()

let jsonToRequest = lam x.
  let extractRequest = lam lookup.
    optionBind (optionBind (lookup "method") getJsonString) (lam method.
    optionBind (optionInvertMap jsonToParams (lookup "params")) (lam params.
    optionBind (optionInvertMap jsonToId (lookup "id")) (lam id.
    Some { method = method
         , params = params
         , id = id
         })))
  in optionBind (getObjectMapping x) extractRequest

let jsonToResult = lam x. Some (Success x)

let jsonToError = lam x.
  let extractError = lam lookup.
    optionBind (optionBind (lookup "code") getJsonInt) (lam code.
    optionBind (optionBind (lookup "message") getJsonString) (lam msg.
    Some (Failure { code = code
                  , message = msg
                  , data = lookup "data"
                  })))
  in optionBind (getObjectMapping x) extractError

let jsonToResponse = lam x.
  let extractResponse = lam lookup.
    optionBind (optionOr (optionBind (lookup "result") jsonToResult)
                         (optionBind (lookup "error") jsonToError)) (lam res.
    optionBind (optionBind (lookup "id") jsonToId) (lam id.
    Some { result = res
         , id = id
         }))
  in optionBind (getObjectMapping x) extractResponse

let processBatch = lam jsonToRpc. lam x.
  match x with JsonObject _ then
    optionMapM jsonToRpc [x]
  else match x with JsonArray arr then
    optionMapM jsonToRpc arr
  else
    None ()

mexpr

let testRequest = {method="foo", params=None (), id=Some (IntId 42)} in
let testJsonRequest = JsonObject [ jsonrpc
                                 , ("method", JsonString "foo")
                                 , ("id", JsonInt 42)
                                 ]
in
let testResponse = {result=Failure {code=negi 32700, message="bar", data=None ()}, id=IntId 42} in
let testJsonResponse =
  JsonObject [ jsonrpc
             , ("error", JsonObject [ ("code", JsonInt (negi 32700))
                                    , ("message", JsonString "bar")
                                    ])
             , ("id", JsonInt 42)
             ]
in
utest requestToJson testRequest
with testJsonRequest in
utest responseToJson testResponse
with testJsonResponse in
utest jsonToRequest testJsonRequest with Some testRequest in
utest jsonToRequest (JsonObject [ jsonrpc ]) with None () in
utest jsonToResponse testJsonResponse with Some testResponse in
utest jsonToResponse (JsonObject [ jsonrpc ]) with None () in
()
