open Authentication
open Cohttp_lwt_unix
open Definition
open Json
open Logger
open Lwt
open Parser

type http_method = 
  | GET 
  | POST
  | DELETE

let string_of_http_method http_method = 
  match http_method with 
  | GET -> "GET"
  | POST -> "POST"
  | DELETE -> "DELETE"

let api_endpoint = "https://api.pro.coinbase.com"

let accounts = "/accounts"
let account id = accounts ^ "/" ^ id 
let account_history id = (account id) ^ "/ledger"
let account_holds id = (account id) ^ "/holds"
let orders = "/orders"
let order id = orders ^ "/" ^ id 
let order_client client_oid = orders ^ "/client:" ^ client_oid
let cancel_order id = orders ^ "/" ^ id
let cancel_orders = orders
let fills = "/fills" 
let fills_by_order_id order_id = fills ^ "/?order_id=" ^ order_id
let fills_by_product_id product_id = fills ^ "/?product_id=" ^ (string_of_product product_id)
let payment_methods = "/payment-methods"
let coinbase_accounts = "/coinbase-accounts"
let fees = "/fees"
let report_status report_id = "/reports/:" ^ report_id
let profiles = "/profiles"
let profile profile_id = profiles ^ profile_id
let user_trailing_volume = "/users/self/trailing-volume"
let oracle = "/oracle"
let products = "/products"
let product_book product_id = products ^ "/" ^ product_id ^ "/book"
let product_ticker product_id = products ^ "/" ^ product_id ^ "/ticker"
let product_trades product_id = products ^ "/" ^ product_id ^ "/trades"
let product_candles product_id = products ^ "/" ^ product_id ^ "/candles" 
let product_stats product_id = products ^ "/" ^ product_id ^ "/stats"
let currencies = "/currencies"
let time = "/time"

let log_response uri code method_string json = 
  if code == 200 then 
    Logger.write_log (Printf.sprintf "%s %s [%d]" method_string (Uri.to_string uri) code)
  else
    Logger.write_log
      ~level:ERROR
      (Printf.sprintf "%s %s [%d, %s]" method_string (Uri.to_string uri) code (parse_error json));
  ()

let request ?json http_method path  =
  let http_method_string = string_of_http_method http_method in 
  let uri = (Uri.of_string (api_endpoint ^ path)) in
  let json_string = string_of_json json in
  let headers = get_headers ~body:json_string path http_method_string in
  let response =
    match http_method with
    | GET -> Client.get ~headers uri 
    | POST -> Client.post ~body:(Cohttp_lwt__Body.of_string json_string) ~headers uri
    | DELETE -> Client.delete ~headers uri in
  response >>= parse_response

let send_request ?json http_method path = 
  Lwt_main.run (request ?json http_method path)