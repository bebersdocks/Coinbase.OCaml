open Yojson.Basic.Util

open Common
open Logger
open Definition

let parse_error error = 
  error |> member "message" |> to_string

type account = {
  id : string;
  currency : currency;
  balance : float;
  available :  float;
  hold : float;
  profile_id : string;
  trading_enabled : bool; }

let parse_account account = {
  id = account |> member "id" |> to_string;
  currency = account |> member "currency" |> to_string |> currency_of_string;
  balance = account |> member "balance" |> to_string |> float_of_string;
  available = account |> member "available" |> to_string |> float_of_string;
  hold = account |> member "hold" |> to_string |> float_of_string;
  profile_id = account |> member "profile_id" |> to_string;
  trading_enabled = account |> member "trading_enabled" |> to_bool; }

let parse_accounts accounts = 
  List.filter_map 
    (fun account -> 
      try 
        Some (parse_account account)
      with Unmatched_definition message -> 
        Logger.write_log ~level:ERROR message;
        None) 
    (to_list accounts)

type history_detail = {
  from_profile : string;
  to_profile : string;
  profile_transfer_id : string; }

let parse_history_detail detail = {
  from_profile = detail |> member "from" |> to_string;
  to_profile = detail |> member "to" |> to_string;
  profile_transfer_id = detail |> member "profile_transfer_id" |> to_string; }

type account_history = {
  id : string;
  created_at : string;
  amount : float;
  balance : float;
  entry : entry;
  details : history_detail; }

let parse_account_history history = {
  id = history |> member "id" |> to_string;
  created_at = history |> member "created_at" |> to_string;
  amount = history |> member "amount" |> to_string |> float_of_string;
  balance = history |> member "balance" |> to_string |> float_of_string;
  entry = history |> member "type" |> to_string |> entry_of_string; 
  details = history |> member "details" |> parse_history_detail; }

let parse_account_histories histories = 
  List.map (fun history -> parse_account_history history) (to_list histories)

type hold_record = {
  id : string;
  created_at : string;
  amount : float;
  hold: hold;
  hold_ref : string; }

let parse_hold hold = {
  id = hold |> member "id" |> to_string;
  created_at = hold |> member "created_at" |> to_string;
  amount = hold |> member "amount" |> to_string |> float_of_string;
  hold = hold |> member "type" |> to_string |> hold_of_string;
  hold_ref = hold |> member "ref" |> to_string; }

let parse_holds holds = 
  List.map (fun hold -> parse_hold hold) (to_list holds)

type order = {
  id : string;
  price : float;
  size : float;
  product_id : product;
  side : side;
  order_type : Definition.order;
  post_only : bool;
  created_at : string;
  fill_fees : float;
  filled_size : float;
  status : status;
  settled : bool; }

let parse_order order = {
  id = order |> member "id" |> to_string;
  price = order |> member "price" |> to_string |> float_of_string;
  size = order |> member "size" |> to_string |> float_of_string;
  product_id = order |> member "product_id" |> to_string |> product_of_string;
  side = order |> member "side" |> to_string |> side_of_string;
  order_type = order |> member "type" |> to_string |> order_of_string;
  post_only = order |> member "post_only" |> to_bool;
  created_at = order |> member "created_at" |> to_string;
  fill_fees = order |> member "fill_fees" |> to_string |> float_of_string;
  filled_size = order |> member "filled_size" |> to_string |> float_of_string;
  status = order |> member "status" |> to_string |> status_of_string;
  settled = order |> member "settled" |> to_bool; }
  
let parse_orders orders = 
  List.filter_map 
    (fun order -> 
      try 
        Some (parse_order order) 
      with Unmatched_definition message -> 
        Logger.write_log ~level:(ERROR) message;
        None)
    (to_list orders)

let parse_canceled_orders ids = 
  ids |> to_list

type fill = {
  trade_id : int;
  product : product;
  price : float;
  size : float;
  order_id : string;
  created_at : string;
  liquidity : liquidity;
  fee : float;
  settled: bool;
  side : side; }

let parse_fill fill = {
  trade_id = fill |> member "trade_id" |> to_int;
  product = fill |> member "product_id" |> to_string |> product_of_string;
  price = fill |> member "price" |> to_string |> float_of_string;
  size = fill |> member "size" |> to_string |> float_of_string;
  order_id = fill |> member "order_id" |> to_string;
  created_at = fill |> member "created_at" |> to_string;
  liquidity = fill |> member "liquidity" |> to_string |> liquidity_of_string;
  fee = fill |> member "fee" |> to_string |> float_of_string; 
  settled = fill |> member "settled" |> to_bool;
  side = fill |> member "side" |> to_string |> side_of_string; }

let parse_fills fills = 
  List.filter_map 
    (fun fill ->
      try 
        Some (parse_fill fill) 
      with Unmatched_definition message -> 
        Logger.write_log ~level:ERROR message;
        None)
    (to_list fills)

type payment_deposit = {
  id : string;
  amount : float;
  currency : currency;
  payout_at : string; }

let parse_payment_deposit payment_deposit = {
  id = payment_deposit |> member "id" |> to_string;
  amount = payment_deposit |> member "amount" |> to_string |> float_of_string;
  currency = payment_deposit |> member "currency" |> to_string |> currency_of_string;
  payout_at = payment_deposit |> member "payout_at" |> to_string; }

type fee = {
  maker_fee_rate : float;
  taker_fee_rate : float;
  usd_volume : float; }

let parse_fee fee = {
  maker_fee_rate = fee |> member "make_fee_rate" |> to_string |> float_of_string;
  taker_fee_rate = fee |> member "taker_fee_rate" |> to_string |> float_of_string;
  usd_volume = fee |> member "fee" |> to_string |> float_of_string; }

let parse_fees fees = 
  List.map (fun fee -> parse_fee fee) (to_list fees)