(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type t = {
  nid : string;
  nss : string;
}

module Uuid = struct
  type t = {
    
  }

  let compare t t' =
end

let empty = Uri.(with_scheme empty (Some "urn"))

let compare t t' =
  match String.compare t.nid t'.nid with
  | 0 -> String.compare t.nss t'.nss
  | c -> c

let of_uri_exn uri =
  match Uri.scheme uri with
  | Some "urn" ->
    begin match Stringext.split ~max:2 (Uri.path uri) ~on:':' with
      | [] -> { nid=""; nss="" }
      | [nid] -> { nid=String.lowercase nid; nss="" }
      | nid::nss::_ -> { nid=String.lowercase nid; nss }
    end
  | None | Some _ -> failwith "not a URI with urn scheme"

let to_uri { nid; nss } = Uri.with_path empty (nid^":"^nss)

let of_uri uri = try Some (of_uri_exn uri) with Failure _ -> None
