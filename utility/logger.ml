open Sys
open Unix

type log_level = 
  | DEBUG
  | WARNING 
  | ERROR 

let string_of_log_level log_level = 
  match log_level with 
  | DEBUG -> "DEBUG"
  | WARNING -> "WARNING"
  | ERROR -> "ERROR"

let write_log_to_file date log = 
  if not (Sys.file_exists "logs") then 
    Unix.mkdir "logs" 0o744;
  let log_file = Unix.openfile ("logs/GTM_" ^ date) [O_WRONLY; O_APPEND; O_CREAT] 0o644 in 
  let output_channel = Unix.out_channel_of_descr log_file in 
  output_string output_channel log;
  close_out output_channel

let write_log ?level:(level = DEBUG) message = 
  let unix_time = Unix.time() in 
  let localtime = Unix.localtime(unix_time) in 
  let time = 
    string_of_int localtime.tm_hour ^ ":" ^ 
    string_of_int localtime.tm_min ^ ":" ^ 
    string_of_int localtime.tm_sec in
  let date = 
    string_of_int localtime.tm_mday ^ "_" ^ 
    string_of_int (localtime.tm_mon + 1) ^ "_" ^ 
    string_of_int (localtime.tm_year + 1900) in
  let log = Printf.sprintf "%s [%s] %s\n" time (string_of_log_level level) message in 
  print_string log;
  write_log_to_file date log
