include "json.mc"
include "map.mc"

type Id
con StrId : String -> Id
con IntId : Int    -> Id

type Params
con ByPosition : [ JsonValue ]           -> Params
con ByName     : [ (String, JsonValue) ] -> Params

type RpcError = { code    : Int
                , message : String
                , data    : Option -- Option JsonValue
                }

type RpcResult
con Success : JsonValue -> RpcResult
con Failure : RpcError  -> RpcResult

type RpcRequest = { method : String
                  , params : Params
                  , id : Option
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
  let members = [ jsonrpc
                , ("method", JsonString x.method)
                , ("params", paramsToJson x.params)
                ]
  in
  let handleId = lam x. [("id", idToJson x)] in
  let membersWithId = concat members (optionMapOr [] handleId x.id) in
  JsonObject membersWithId

let errorToJson = lam x.
  let members = [ ("code", JsonInt x.code)
                , ("message", JsonString x.message)
                ]
  in
  let handleData = lam x. [("data", x)] in
  let membersWithData = concat members (optionMapOr [] handleData x.data) in
  JsonObject membersWithData

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

let jsonToRequest = lam x.
  let extractRequest = lam arr.
    let lookupArr = lam k. mapLookupOpt eqstr k arr in
    let extractJsonString = lam x.
      match x with JsonString s then Some s else None ()
    in
    optionBind (optionBind (lookupArr "method") extractJsonString) (lam method.
    optionBind (optionBind (lookupArr "params") jsonToParams) (lam params.
    optionBind (optionBind (lookupArr "id") jsonToId) (lam id.
    Some { method = method
         , params = params
         , id = id
         })))
  in
  match x with JsonObject arr then
    extractRequest arr
  else
    None ()

-- let jsonBatchToRequests
-- let jsonToResponse

mexpr

let testRequest = {method="foo", params=ByPosition [], id=Some (IntId 42)} in
let testResponse = {result=Failure {code=negi 32700, message="bar", data=None ()}, id=IntId 42} in

utest requestToJson testRequest
with JsonObject [ jsonrpc
                , ("method", JsonString "foo")
                , ("params", JsonArray [])
                , ("id", JsonInt 42)
                ] in
utest responseToJson testResponse
with JsonObject [ jsonrpc
                , ("error", JsonObject [ ("code", JsonInt (negi 32700))
                                       , ("message", JsonString "bar")
                                       ])
                , ("id", JsonInt 42)
                ] in
()
