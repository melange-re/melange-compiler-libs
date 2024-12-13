(* TEST
 modules = "a.ml cocinelle.ml";
 {
   setup-ocamlc.byte-build-env;
   flags = "-for-pack Pack";
   module = "a.ml";
   ocamlc.byte;
   flags = "-for-pack Pack";
   module = "cocinelle.ml";
   ocamlc.byte;
   module = "";
   flags = "";
   program="./cocinelle.byte";
   all_modules = "a.cmo cocinelle.cmo";
   module = "";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
     flags = "-for-pack Pack";
     module = "a.ml";
     ocamlopt.byte;
     flags = "-for-pack Pack";
     module = "cocinelle.ml";
     ocamlopt.byte;
     output="cocinelle";
     all_modules = "a.cmx cocinelle.cmx";
     program="./cocinelle.exe";
     module = "";
     ocamlopt.byte;
     run;
     check-program-output;
 }
*)

let () = Format.printf "A.x=%d@." A.x
