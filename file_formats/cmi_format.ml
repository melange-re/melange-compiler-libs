(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

type pers_flags =
  | Rectypes
  | Alerts of alerts
  | Opaque
  | Unsafe_string

type error =
  | Not_an_interface of filepath
  | Wrong_version_interface of filepath * string
  | Corrupted_interface of filepath

exception Error of error

(* these type abbreviations are not exported;
   they are used to provide consistency across
   input_value and output_value usage. *)
type signature = Types.signature_item list
type flags = pers_flags list
type header = modname * signature

type cmi_infos = {
    cmi_name : modname;
    cmi_sign : signature;
    cmi_crcs : crcs;
    cmi_flags : flags;
}

let input_cmi ic =
  let (name, sign) = (input_value ic : header) in
  let crcs = (input_value ic : crcs) in
  let flags = (input_value ic : flags) in
  {
      cmi_name = name;
      cmi_sign = sign;
      cmi_crcs = crcs;
      cmi_flags = flags;
    }

let read_cmi filename =
  let ic = open_in_bin filename in
  try
    let buffer =
      really_input_string ic (String.length Config.cmi_magic_number)
    in
    if buffer <> Config.cmi_magic_number then begin
      close_in ic;
      let pre_len = String.length Config.cmi_magic_number - 3 in
      if String.sub buffer 0 pre_len
          = String.sub Config.cmi_magic_number 0 pre_len then
      begin
        let msg =
          if buffer < Config.cmi_magic_number then "an older" else "a newer" in
        raise (Error (Wrong_version_interface (filename, msg)))
      end else begin
        raise(Error(Not_an_interface filename))
      end
    end;
    let cmi = input_cmi ic in
    close_in ic;
    cmi
  with End_of_file | Failure _ ->
      close_in ic;
      raise(Error(Corrupted_interface(filename)))
    | Error e ->
      close_in ic;
      raise (Error e)

let output_cmi filename oc cmi =
(* beware: the provided signature must have been substituted for saving *)
  output_string oc Config.cmi_magic_number;
  output_value oc ((cmi.cmi_name, cmi.cmi_sign) : header);
  flush oc;
  let crc = Digest.file filename in
  let crcs = (cmi.cmi_name, Some crc) :: cmi.cmi_crcs in
  output_value oc (crcs : crcs);
  output_value oc (cmi.cmi_flags : flags);
  crc

#if true then
(* This function is also called by [save_cmt] as cmi_format is subset of 
       cmt_format, so dont close the channel yet
*)
let create_cmi ?check_exists filename (cmi : cmi_infos) =
  (* beware: the provided signature must have been substituted for saving *)
  let content = 
    Config.cmi_magic_number ^ Marshal.to_string  (cmi.cmi_name, cmi.cmi_sign) []
    (* checkout [output_value] in {!Pervasives} module *)
  in 
  let crc = Digest.string content in   
  let cmi_infos = 
    if check_exists <> None && Sys.file_exists filename then 
      Some (read_cmi filename)
    else None in   
  match cmi_infos with 
  | Some {cmi_name = _; cmi_sign = _; cmi_crcs = (old_name, Some old_crc)::rest ; cmi_flags} 
    (* TODO: design the cmi format so that we don't need read the whole cmi *)
    when 
      cmi.cmi_name = old_name &&
      crc = old_crc &&
      cmi.cmi_crcs = rest &&
      cmi_flags = cmi.cmi_flags -> 
      crc 
  | _ -> 
      let crcs = (cmi.cmi_name, Some crc) :: cmi.cmi_crcs in
      let oc = open_out_bin filename in 
      output_string oc content;
      output_value oc crcs;
      output_value oc cmi.cmi_flags;
      close_out oc; 
      crc


#end
  
(* Error report *)

open Format

let report_error ppf = function
  | Not_an_interface filename ->
      fprintf ppf "%a@ is not a compiled interface"
        Location.print_filename filename
  | Wrong_version_interface (filename, older_newer) ->
      fprintf ppf
        "%a@ is not a compiled interface for this version of OCaml.@.\
         It seems to be for %s version of OCaml."
        Location.print_filename filename older_newer
  | Corrupted_interface filename ->
      fprintf ppf "Corrupted compiled interface@ %a"
        Location.print_filename filename

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
