open Str

let char_list_of_string s = List.init (String.length s) (String.get s)

let rec string_of_char_list = function 
  | [] -> ""
  | hd :: tail -> (String.make 1 hd) ^ string_of_char_list tail

let trim_char str =
  let char_list = char_list_of_string str in 
  let rec trim_char_from_list = function
    | [] -> []
    | hd :: tail ->
      if hd = '\n' then
        trim_char_from_list tail 
      else 
        hd :: trim_char_from_list tail in
  let trimmed = trim_char_from_list char_list in 
  string_of_char_list trimmed

let contains s1 s2 =
  let re = Str.regexp_string s2
  in
    try
      ignore (Str.search_forward re s1 0); 
      true
    with Not_found -> false
