external process_and_close_fd : int -> string -> unit = "caml_process_fd"

let () =
  for i = 2 to (Array.length Sys.argv) -1
  do
    process_and_close_fd (i - 1) Sys.argv.(i);
  done;
  (* For the execv version of the test, clean-up tmp.txt - for the
     Unix.create_process version, this is done by cloexec.ml *)
  if Sys.argv.(1) = "execv" then
    Sys.remove "tmp.txt"
