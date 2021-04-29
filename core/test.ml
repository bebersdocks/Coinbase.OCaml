open Lwt_result
open Yojson.Basic
open Yojson.Basic.Util

open Json 
open Parser
open Logger
open Request
open Definition

let account_test () = 
  Logger.write_log DEBUG "BEGIN ACCOUNT TEST\n";
  let (_code, accounts_json) = send_request GET Request.accounts in
  let accounts = Parser.parse_accounts accounts_json in
  Logger.write_log DEBUG 

  "Printing accounts"; 
  List.iter 
    (fun (_, currency, balance, _, _, _, _) -> 
      Logger.write_log DEBUG ((string_of_currency currency) ^ ":" ^ string_of_float balance))
    accounts;
  Logger.write_log DEBUG "Retrieving first EUR account";

  let eur_account = 
    List.find (fun (id, currency, _, _, _, _, _) -> currency == EUR) accounts in
  let (id, _, _, _, _, _, _) = eur_account in 
  let (_code, specific_account) = send_request GET (account id) in 
  let (_, currency, balance, _, _, _, _) = Parser.parse_account specific_account in 
  Logger.write_log DEBUG ((string_of_currency currency) ^ ":" ^ string_of_float balance);

  let (_, account_history) = send_request GET (account_history id) in 
  let parsed_histories = Parser.parse_account_histories account_history in

  Logger.write_log DEBUG "Printing histories"; 
  List.iter 
    (fun (id, created_at, amount, balance, _, _) -> 
      Logger.write_log DEBUG 
        (id ^ ":" ^ created_at ^ ":" ^ (string_of_float amount) ^ ":" ^ (string_of_float balance)))
    parsed_histories;
  let (_, holds) = send_request GET (account_holds id) in
  let parsed_holds = Parser.parse_holds holds in

  Logger.write_log DEBUG "Printing holds";
  List.iter 
  (fun (id, created_at, amount, hold, _) -> 
    Logger.write_log DEBUG 
      (id ^ ":" ^ created_at ^ ":" ^ (string_of_float amount) ^ ":" ^ (string_of_hold hold)))
  parsed_holds;

  Logger.write_log DEBUG "END ACCOUNT TEST\n"
 
let print_orders orders = 
  Logger.write_log DEBUG "***************** Printing orders"; 
  List.iter 
  (fun (_, price, size, product_id, side, order, _, created_at, fill_fees, filled_size, status, settled) -> 
    Logger.write_log DEBUG 
      (Printf.sprintf 
        "%f %f %s %s %s %s %f %f %s %b" 
        price size (string_of_product product_id) (string_of_side side) 
        (string_of_order order) created_at fill_fees filled_size
        (string_of_status status) settled))
  orders

let order_test () = 
  Logger.write_log DEBUG "BEGIN ORDER TEST\n";
  print_orders (parse_orders (send_request GET orders |> fun (_, body) -> body));
  
  Logger.write_log DEBUG "Placing new order";
  let place_order_body = Json.place_new_order 1.0 0.2 Buy ZRX_EUR Limit in 
  let place_order_response = send_request ~json:place_order_body POST orders in
  place_order_response |> fun (_, place_order_response) ->
  let (order_id, price, size, product_id, side, order, _, created_at, _, _, status, _) = Parser.parse_order place_order_response in 
  
  Logger.write_log DEBUG 
    (Printf.sprintf 
      "%s %s order for %s of size %f at price %f is %s"
      (string_of_order order) (string_of_side side) 
      (string_of_product product_id) size price (string_of_status status));
  print_orders (parse_orders (send_request GET orders |> fun (_, body) -> body));
  
  Logger.write_log DEBUG "Canceling last order";
  let _delete_order = send_request DELETE (cancel_order order_id) in
  print_orders (parse_orders (send_request GET orders |> fun (_, body) -> body));
  let place_order_body = Json.place_new_order 1.0 0.2 Buy ZRX_EUR Limit in 
  let _ = send_request ~json:place_order_body POST orders in
  let _ = send_request ~json:place_order_body POST orders in

  print_orders (parse_orders (send_request GET orders |> fun (_, body) -> body));

  let (_, _) = send_request DELETE cancel_orders in

  print_orders (parse_orders (send_request GET orders |> fun (_, body) -> body));

  Logger.write_log DEBUG "END ORDER TEST\n"

let misc_test_one () =
  Logger.write_log DEBUG "BEGIN MISC TEST ONE\n";
  let _ = send_request POST ~json:(Json.place_new_order 1.0 0.306 Buy ZRX_EUR Market) orders in
  let _ = send_request POST ~json:(Json.place_new_order 1.0 0.306 Sell ZRX_EUR Market) orders in
  let (_, fills) = send_request GET (fills_by_product_id ZRX_EUR) in 
  let parsed_fills = parse_fills fills in 

  List.iter
    (fun (_, product, price, size, order_id, create_at, liquidity, fee, _, side) ->
    Logger.write_log 
      DEBUG 
      (Printf.sprintf 
      "%s %s trade of size %f filled at price %f with fee %f as %s" 
      (string_of_product product) (string_of_side side) 
      size price fee (string_of_liquidity liquidity)))
    parsed_fills;

  Logger.write_log DEBUG "END ORDER TEST\n"
