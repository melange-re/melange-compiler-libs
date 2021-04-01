(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2005 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type mli_status = Mli_na | Mli_exists | Mli_non_exists
val no_implicit_current_dir : bool ref
val assume_no_mli : mli_status ref
val record_event_when_debug : bool ref
val bs_vscode : bool
val dont_record_crc_unit : string option ref
val bs_gentype : string option ref
val no_assert_false : bool ref
