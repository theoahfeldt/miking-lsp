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

let optionSnoc = lam x. lam y.
  match y with Some e then snoc x e else x

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

let optionInvert = lam opt.
  match opt with None () then
    Some (None ())
  else match opt with Some (None ()) then
    None ()
  else opt

let optionInvertMap = lam f. compose optionInvert (optionMap f)

let jsonToRequest = lam x.
  let extractRequest = lam arr.
    let lookupArr = lam k. mapLookupOpt eqstr k arr in
    let extractJsonString = lam x.
      match x with JsonString s then Some s else None ()
    in
    optionBind (optionBind (lookupArr "method") extractJsonString) (lam method.
    optionBind (optionInvertMap jsonToParams (lookupArr "params")) (lam params.
    optionBind (optionInvertMap jsonToId (lookupArr "id")) (lam id.
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
()
