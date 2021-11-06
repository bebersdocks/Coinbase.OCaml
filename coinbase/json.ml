open Definition
open Yojson.Basic

let string_of_json (json: t option) =
  match json with 
  | None -> ""
  | Some json -> to_string json

(* [TODO] Optional parameters *)
let place_new_order ?(stop=None) ?(stop_price=0.0) size price side product order_type =
  `Assoc
    [
      ("size", `Float size);
      ("price", `Float price);
      ("side", `String (string_of_side side));
      ("product_id", `String (string_of_product product));
      ("type", `String (string_of_order order_type));
    ]

let cancel_all_orders order_ids = 
  `List (List.map (fun order_id -> `String order_id) order_ids)

let payment_deposit amount currency payment_method_id = 
  `Assoc
    [
      ("amount", `Float amount);
      ("currency", `String (string_of_currency currency));
      ("payment_method_id", `String payment_method_id);
    ]
