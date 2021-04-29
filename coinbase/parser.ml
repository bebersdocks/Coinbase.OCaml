open Yojson.Basic.Util

open Common
open Logger
open Definition

let parse_error error = 
  error |> member "message" |> to_string

let parse_account account = 
  let id = account |> member "id" |> to_string in 
  let currency = account |> member "currency" |> to_string |> currency_of_string in 
  let balance = account |> member "balance" |> to_string |> float_of_string in 
  let available = account |> member "available" |> to_string |> float_of_string in 
  let hold = account |> member "hold" |> to_string |> float_of_string in 
  let profile_id = account |> member "profile_id" |> to_string in 
  let trading_enabled = account |> member "trading_enabled" |> to_bool in
  (id, currency, balance, available, hold, profile_id, trading_enabled)

let parse_accounts accounts = 
  List.filter_map 
    (fun account -> 
      try 
        Some (parse_account account)
      with Unmatched_definition message -> 
        Logger.write_log ERROR message;
        None) 
    (to_list accounts)

let parse_history_detail detail = 
  let from_profile = detail |> member "from" |> to_string in 
  let to_profile = detail |> member "to" |> to_string in 
  let profile_transfer_id = detail |> member "profile_transfer_id" |> to_string in
  (from_profile, to_profile, profile_transfer_id)

let parse_account_history history = 
  let id = history |> member "id" |> to_string in 
  let created_at = history |> member "created_at" |> to_string in 
  let amount = history |> member "amount" |> to_string |> float_of_string in 
  let balance = history |> member "balance" |> to_string |> float_of_string in 
  let entry_type = history |> member "type" |> to_string |> entry_of_string in 
  let details = history |> member "details" |> parse_history_detail in 
  (id, created_at, amount, balance, entry_type, details)

let parse_account_histories histories = 
  List.map (fun history -> parse_account_history history) (to_list histories)

let parse_hold hold = 
  let id = hold |> member "id" |> to_string in 
  let created_at = hold |> member "created_at" |> to_string in 
  let amount = hold |> member "amount" |> to_string |> float_of_string in 
  let hold_type = hold |> member "type" |> to_string |> hold_of_string in 
  let hold_ref = hold |> member "ref" |> to_string in 
  (id, created_at, amount, hold_type, hold_ref)

let parse_holds holds = 
  List.map (fun hold -> parse_hold hold) (to_list holds)

let parse_order order =
  let id = order |> member "id" |> to_string in
  let price = order |> member "price" |> to_string |> float_of_string in
  let size = order |> member "size" |> to_string |> float_of_string in
  let product_id = order |> member "product_id" |> to_string |> product_of_string in
  let side = order |> member "side" |> to_string |> side_of_string in
  let order_type = order |> member "type" |> to_string |> order_of_string in 
  let post_only = order |> member "post_only" |> to_bool in
  let created_at = order |> member "created_at" |> to_string in
  let fill_fees = order |> member "fill_fees" |> to_string |> float_of_string in
  let filled_size = order |> member "filled_size" |> to_string |> float_of_string in
  let status = order |> member "status" |> to_string |> status_of_string in
  let settled = order |> member "settled" |> to_bool in
  (id, price, size, product_id, side, order_type, post_only, 
  created_at, fill_fees, filled_size, status, settled)
  
let parse_orders orders = 
  List.filter_map 
    (fun order -> 
      try 
        Some (parse_order order) 
      with Unmatched_definition message -> 
        Logger.write_log ERROR message;
        None)
    (to_list orders)

let parse_canceled_orders ids = 
  ids |> to_list

let parse_fill fill = 
  let trade_id = fill |> member "trade_id" |> to_int in
  let product = fill |> member "product_id" |> to_string |> product_of_string in
  let price = fill |> member "price" |> to_string |> float_of_string in 
  let size = fill |> member "size" |> to_string |> float_of_string in 
  let order_id = fill |> member "order_id" |> to_string in 
  let created_at = fill |> member "created_at" |> to_string in 
  let liquidity = fill |> member "liquidity" |> to_string |> liquidity_of_string in 
  let fee = fill |> member "fee" |> to_string |> float_of_string in 
  let settled = fill |> member "settled" |> to_bool in 
  let side = fill |> member "side" |> to_string |> side_of_string in 
  (trade_id, product, price, size, order_id, created_at, liquidity, fee, settled, side)

let parse_fills fills = 
  List.filter_map 
    (fun fill ->
      try 
        Some (parse_fill fill) 
      with Unmatched_definition message -> 
        Logger.write_log ERROR message;
        None)
    (to_list fills)

let parse_payment_deposit payment_deposit = 
  let id = payment_deposit |> member "id" |> to_string in 
  let amount = payment_deposit |> member "amount" |> to_string |> float_of_string in 
  let currency = payment_deposit |> member "currency" |> to_string |> currency_of_string in 
  let payout_at = payment_deposit |> member "payout_at" |> to_string in 
  (id, amount, currency, payout_at)

let parse_fee fee = 
  let maker_fee_rate = fee |> member "make_fee_rate" |> to_string |> float_of_string in 
  let taker_fee_rate = fee |> member "taker_fee_rate" |> to_string |> float_of_string in 
  let usd_volume = fee |> member "fee" |> to_string |> float_of_string in 
  (maker_fee_rate, taker_fee_rate, usd_volume)

let parse_fees fees = 
  List.map (fun fee -> parse_fee fee) (to_list fees)