external process_and_close_fd : int -> string -> unit = "caml_process_fd"

let () =
  for i = 1 to (Array.length Sys.argv) -1
  do
    process_and_close_fd i Sys.argv.(i);
  done;
  Sys.remove "tmp.txt"
