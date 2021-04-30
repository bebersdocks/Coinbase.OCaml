open Lwt_result
open Yojson.Basic
open Yojson.Basic.Util

open Json 
open Parser
open Logger
open Request
open Definition

let account () = 
  Logger.write_log DEBUG "BEGIN ACCOUNT TEST\n";

  let accounts = 
    send_request GET Request.accounts 
    |> (fun (_, response) -> Parser.parse_accounts response) in 
    
  Logger.write_log DEBUG "PRINTING ACCOUNTS";

  List.iter 
    (fun (account: Parser.account) -> 
      Logger.write_log DEBUG ((string_of_currency account.currency) ^ ":" ^ string_of_float account.balance))
    accounts;

  Logger.write_log DEBUG "RETRIEVING FIRST EUR ACCOUNT BY ID";

  let eur_account_id = 
    List.find (fun (account: Parser.account) -> account.currency == EUR) accounts
    |> (fun account -> account.id) in

  let eur_account = 
    send_request GET (eur_account_id)
    |> (fun (_, response) -> Parser.parse_account response) in 

  Logger.write_log DEBUG ((string_of_currency eur_account.currency) ^ ":" ^ string_of_float eur_account.balance);

  Logger.write_log DEBUG "RETRIEVING ACCOUNT HISTORIES";

  let histories = 
    send_request GET (account_history eur_account.id)
    |> (fun (_, response) -> Parser.parse_account_histories response) in 
    
  Logger.write_log DEBUG "PRINTING HISTORIES";

  List.iter 
    (fun (history: Parser.account_history) -> 
      Logger.write_log DEBUG (history.id ^ ":" ^ history.created_at ^ ":" ^ (string_of_float history.amount) ^ ":" ^ (string_of_float history.balance)))
    histories;

  Logger.write_log DEBUG "RETRIEVING HOLDS";

  let holds = 
    send_request GET (account_holds eur_account.id)
    |> (fun (_, response) -> Parser.parse_holds response) in 

  Logger.write_log DEBUG "PRINTING HOLDS";
  List.iter 
    (fun (hold: Parser.hold_record) -> 
      Logger.write_log DEBUG (hold.id ^ ":" ^ hold.created_at ^ ":" ^ (string_of_float hold.amount) ^ ":" ^ (string_of_hold hold.hold)))
    holds;

  Logger.write_log DEBUG "END ACCOUNT TEST\n"

let print_current_orders () = 
  let orders = 
    send_request GET orders 
    |> (fun (_, response) -> Parser.parse_orders response) in

  Logger.write_log DEBUG "\nPRINTING CURRENT ORDERS"; 

  List.iter 
    (fun (order: Parser.order) -> 
    Logger.write_log DEBUG 
      (Printf.sprintf 
        "%f %f %s %s %s %s %f %f %s %b" 
        order.price order.size (string_of_product order.product_id) (string_of_side order.side) 
        (string_of_order order.order_type) order.created_at order.fill_fees order.filled_size
        (string_of_status order.status) order.settled))
  orders

let order () = 
  Logger.write_log DEBUG "BEGIN ORDER TEST\n";

  print_current_orders ();

  Logger.write_log DEBUG "PLACING NEW ORDER";

  let buy_order = Json.place_new_order 1.0 0.2 Buy ZRX_EUR Limit in 

  let order = 
    send_request ~json:buy_order POST orders
    |> fun (_, response) -> Parser.parse_order response in

  Logger.write_log DEBUG 
    (Printf.sprintf 
      "%s %s order for %s of size %f at price %f is %s"
      (string_of_order order.order_type) (string_of_side order.side) 
      (string_of_product order.product_id) order.size order.price (string_of_status order.status));

  print_current_orders ();

  Logger.write_log DEBUG "CANCELLING LAST ORDER";

  send_request DELETE (cancel_order order.id) |> ignore;

  print_current_orders ();

  let sell_order = Json.place_new_order 1.0 0.2 Buy ZRX_EUR Limit in 

  send_request ~json:sell_order POST orders |> ignore;

  print_current_orders ();

  send_request DELETE cancel_orders |> ignore;

  print_current_orders ();

  Logger.write_log DEBUG "END ORDER TEST\n"

let market () =
  Logger.write_log DEBUG "BEGIN MARKET TEST\n";

  send_request POST ~json:(Json.place_new_order 1.0 0.306 Buy ZRX_EUR Market) orders |> ignore;

  send_request POST ~json:(Json.place_new_order 1.0 0.306 Sell ZRX_EUR Market) orders |> ignore;

  let fills = 
    send_request GET (fills_by_product_id ZRX_EUR)
    |> (fun (_, response) -> Parser.parse_fills response) in 

  List.iter
    (fun fill ->
      Logger.write_log DEBUG 
        (Printf.sprintf 
          "%s %s trade of size %f filled at price %f with fee %f as %s" 
          (string_of_product fill.product) (string_of_side fill.side) 
          fill.size fill.price fill.fee (string_of_liquidity fill.liquidity)))
    fills;

  Logger.write_log DEBUG "END ORDER TEST\n"