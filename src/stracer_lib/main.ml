(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open EzCompat

let int_of_string s =
  try
    int_of_string s
  with _ ->
    Printf.kprintf failwith "int_of_string(%S) failed" s

type process = {
  pid : int ;
  mutable lines : string list ;
  mutable printed : bool ;
  mutable father : process option ;
  mutable children : process list ;
  mutable process : string option ;
}

let parse filename =

  let map = ref IntMap.empty in
  let first_process = ref None in

  let find_process pid =
    match IntMap.find pid !map with
    | exception _ ->
        let p = { pid ;
                  printed = false ;
                  father = None ;
                  children = [];
                  lines = [];
                  process = None ;} in
        map := IntMap.add pid p !map ;
        p
    | p -> p
  in

  let rec parse_lines ic =
    match input_line ic with
    | exception _ ->
        (* Printf.eprintf "End of file reached\n%!" *)
        ()
    | line ->
        (* Printf.eprintf "LINE %S\n%!" line; *)
        let len = String.length line in
        if len=0 then
          parse_lines ic
        else
          let pid, line = EzString.cut_at line ' ' in
          let p = find_process (int_of_string pid) in
          begin match !first_process with
            | None -> first_process := Some p
            | Some _ -> ()
          end;
          p.lines <- line :: p.lines ;
          parse_lines ic

  in
  let get_retcode line =
    match String.rindex line '=' with
    | exception _ -> None
    | pos ->
        let len = String.length line in
        if line.[pos-1] = ' ' && line.[pos+1] = ' ' then
          Some ( String.sub line (pos+2) (len-pos-2) )
        else
          None
  in

  let ic = open_in filename in
  parse_lines ic ;
  close_in ic;

  let extract_process line =
    let _, line = EzString.cut_at line '[' in
    let process, _ = EzString.cut_at line ']' in
    process
  in

  let rec find_execve lines =
    match lines with
    | [] -> raise Not_found
    | line :: "<... execve resumed>)            = 0" :: lines ->
        extract_process line, lines
    | _line :: line_end :: lines when
        EzString.starts_with line_end ~prefix:"<... execve resumed>" ->
        find_execve lines
    | line :: lines ->
        if EzString.starts_with line ~prefix:"execve(" then
          match get_retcode line with
          | None-> raise Not_found
          | Some "0" ->
              extract_process line, lines
          | _ -> find_execve lines
        else
          raise Not_found
  in

  let rec iter_process p =
    p.printed <- true ;
    p.lines <- List.rev p.lines ;
    let lines = match find_execve p.lines with
      | exception _ -> p.lines
      | process, lines -> p.process <- Some process;
          lines
    in
    List.iter (fun line ->
        if EzString.starts_with line ~prefix:"clone(" ||
           EzString.starts_with line ~prefix:"<... vfork resumed>" ||
           EzString.starts_with line ~prefix:"<... clone resumed>"
        then
          let pid = get_retcode line in
          match pid with
          | None ->
              Printf.eprintf "%d CAN NOT EXTRACT RETCODE %s\n%!" p.pid line
          | Some pid ->
              match int_of_string pid with
              | exception _ ->
                  Printf.eprintf "pid %d CAN NOT PARSE RETCODE %s\n%!" p.pid line
              | pid ->
                  (*                  Printf.printf "%sclone() = %d\n%!" indent pid; *)
                  let pp = try
                      IntMap.find pid !map
                    with _ ->
                      Printf.eprintf "Cannot find pid %d in map\n%!" pid;
                      find_process pid
                  in
                  iter_process pp;
                  match pp.process, pp.children with
                  | None, [] -> ()
                  | _ ->
                      p.children <- pp :: p.children ;
                      pp.father <- Some p;
      ) lines ;
    p.children <- List.rev p.children
  in

  let rec print_process indent p =
    Printf.printf "%s%d: %s\n%!" indent p.pid
      (match p.process with
       | Some process -> process
       | None -> "<unknown>") ;
    List.iter (print_process (indent ^ "|---")) p.children ;
  in
  begin
    match !first_process with
    | None -> assert false
    | Some p ->
        iter_process p ;

        print_process "" p ;
  end;
  IntMap.iter (fun _pid p ->
      if not p.printed then begin
        Printf.eprintf "ERROR: orphaned process !!!\n%!";
        print_process "" p
      end
    ) !map ;


  ()

let main () =

  let arg_list = Arg.align [

    ] in
  let arg_anon filename = parse filename in
  let arg_usage =
    Printf.sprintf "%s [OPTIONS] [FILES]: parse strace output" Sys.argv.(0)
  in

  Arg.parse arg_list arg_anon arg_usage ;
  ()
