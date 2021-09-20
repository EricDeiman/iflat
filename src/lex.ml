(*
    Copyright 2018 Eric J. Deiman

    This file is part of the iflat project.

    iflat is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    iflat is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with iflat.  If not, see <https://www.gnu.org/licenses/>.
*)

let source_string_stream str =
  let source = Scanf.Scanning.from_string str in
  Stream.from (fun i ->
      try Scanf.bscanf source "%c" (fun x -> Some x) with
        End_of_file -> None
    )

let lexer input =
  let between ch a b = a <= ch && ch <= b in
  let is_digit ch = between ch '0' '9' in
  let is_letter ch = (between ch 'a' 'z') || (between ch 'A' 'Z') in
  let is_whitespace ch = ch = ' ' || ch = '\t' in
  let is_symbol ch =
    begin
      let symbols = "~!@#$%^&*()_-+=[]{}:;'<>?,./|" in
      try ignore (String.index symbols ch); true with
      | Not_found -> false
    end
  in
  let read_object classifier =
    (fun stream ->
       let rec obj s b =
         let ch = Stream.peek s
         in
         match ch with
         | Some c ->
           if classifier c
           then (Buffer.add_char b (Stream.next s); obj s b)
           else Buffer.contents b
         | None -> Buffer.contents b
       in obj stream (Buffer.create 16)
    )
  in
  let read_number = read_object is_digit in
  let read_word = read_object is_letter in
  let read_symbol = read_object is_symbol in
  let read_whitespace = read_object is_whitespace in
  Stream.from (fun (i) ->
      ignore (read_whitespace input);
      match Stream.peek input with
      | None -> None
      | Some '\n' -> None
      | Some c ->
        if is_digit c
        then Some (read_number input)
        else if is_letter c
        then Some (read_word input)
        else if is_symbol c
        then Some (read_symbol input)
        else None)
