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

let time_string_of_time time = 
  string_of_int time.tm_hour ^ ":" ^ string_of_int time.tm_min ^ ":" ^ string_of_int time.tm_sec

let date_string_of_time time = 
  string_of_int time.tm_mday ^ "_" ^ string_of_int (time.tm_mon + 1) ^ "_" ^ string_of_int (time.tm_year + 1900)

let write_log ?level:(level = DEBUG) message = 
  let unix_time = Unix.time() in 
  let localtime = Unix.localtime(unix_time) in 
  let time = time_string_of_time localtime in
  let date = date_string_of_time localtime in
  let log = Printf.sprintf "%s [%s] %s\n" time (string_of_log_level level) message in 
  print_string log;
  write_log_to_file date log
