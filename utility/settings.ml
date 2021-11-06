open Yojson.Basic
open Yojson.Basic.Util

type settings = 
  { api_key: string; 
    secret: string; 
    passphrase: string; }

let settings =
  let settings_json = from_file "settings.json" in
  { api_key = settings_json |> member "apiKey" |> to_string;
    secret = settings_json |> member "secret" |> to_string;
    passphrase = settings_json |> member "passphrase" |> to_string; }
