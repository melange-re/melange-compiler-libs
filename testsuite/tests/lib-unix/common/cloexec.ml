(* TEST
 include unix;
 readonly_files = "fdstatus_aux.c fdstatus_main.ml";
 (*
   This test is temporarily disabled on the MinGW and MSVC ports,
   because since fdstatus has been wrapped in an OCaml program,
   it does not work as well as before.
   Presumably this is because the OCaml runtime opens files, so that handles
   that have actually been closed at execution look open and make the
   test fail.

   One possible fix for this would be to make it possible for ocamltest to
   compile C-only programs, which will be a bit of work to handle the
   output of msvc and will also duplicate what the OCaml compiler itself
   already does.
 *)
 hasunix;

 {
   program = "${test_build_directory}/cloexec.byte";
   setup-ocamlc.byte-build-env;
   program = "${test_build_directory}/fdstatus.exe";
   all_modules = "fdstatus_aux.c fdstatus_main.ml";
   ocamlc.byte;
   program = "${test_build_directory}/cloexec.byte";
   all_modules = "fdstatus_aux.c cloexec.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
   check-program-output;
 }{
   program = "${test_build_directory}/cloexec.opt";
   setup-ocamlopt.byte-build-env;
   program = "${test_build_directory}/fdstatus.exe";
   all_modules = "fdstatus_aux.c fdstatus_main.ml";
   ocamlopt.byte;
   program = "${test_build_directory}/cloexec.opt";
   all_modules = "fdstatus_aux.c cloexec.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
   check-program-output;
 }
*)

external fd_of_file_descr : Unix.file_descr -> int = "caml_fd_of_filedescr"
let string_of_fd fd = Int.to_string (fd_of_file_descr fd)

let status_checker = "fdstatus.exe"

let _ =
  let f0 = Unix.(openfile "tmp.txt" [O_WRONLY; O_CREAT; O_TRUNC] 0o600) in
  let f1 = Unix.(openfile "tmp.txt" [O_RDONLY; O_KEEPEXEC] 0) in
  let f2 = Unix.(openfile "tmp.txt" [O_RDONLY; O_CLOEXEC] 0) in
  let d0 = Unix.dup f0 in
  let d1 = Unix.dup ~cloexec:false f1 in
  let d2 = Unix.dup ~cloexec:true f2 in
  let (p0, p0') = Unix.pipe () in
  let (p1, p1') = Unix.pipe ~cloexec:false () in
  let (p2, p2') = Unix.pipe ~cloexec:true () in
  let s0 = Unix.(socket PF_INET SOCK_STREAM 0) in
  let s1 = Unix.(socket ~cloexec:false PF_INET SOCK_STREAM 0) in
  let s2 = Unix.(socket ~cloexec:true PF_INET SOCK_STREAM 0) in
  let (x0, x0') = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let (x1, x1') = Unix.(socketpair ~cloexec:false PF_UNIX SOCK_STREAM 0) in
  let (x2, x2') = Unix.(socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0) in

  let fds = [| f0;f1;f2; d0;d1;d2;
               p0;p0';p1;p1';p2;p2';
               s0;s1;s2;
               x0;x0';x1;x1';x2;x2' |] in
  let string_fds = (Array.map string_of_fd fds) in
  (* NB On Windows, as documented, execv terminates immediately, which is
        usually a problem. However, ocamltest runs tests in a process group and
        the test step is not terminated until _all_ processes have completed, so
        we can use Unix.execv here, even on Windows. *)
  Unix.execv
    (Filename.concat Filename.current_dir_name status_checker)
    (Array.append [| status_checker |] string_fds)
