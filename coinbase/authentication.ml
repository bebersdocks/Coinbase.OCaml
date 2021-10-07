open Common
open Cryptokit
open Settings
open Unix
    
let generate_signature timestamp http_method path body = 
  let message = timestamp ^ http_method ^ path ^ body in
  let hmac_key = transform_string (Base64.decode()) Settings.secret in 
  let signature = hash_string (MAC.hmac_sha256 hmac_key) message in 
  let signature_base64 = 
    transform_string (Base64.encode_multiline()) signature in
  String.sub signature_base64 0 ((String.length signature_base64) - 1)

let headers request http_method body = 
  let timestamp = string_of_int(int_of_float(Unix.time())) in 
  let signature = generate_signature timestamp http_method request body in
  [
    "CB-ACCESS-SIGN", signature;
    "CB-ACCESS-TIMESTAMP", timestamp;
    "CB-ACCESS-KEY", Settings.api_key;
    "CB-ACCESS-PASSPHRASE", Settings.passphrase;
    "Content-Type", "application/json";
  ]

let get_headers ?(body="") request http_method = 
  let header = Cohttp.Header.init() in 
  Cohttp.Header.add_list header (headers request http_method body)