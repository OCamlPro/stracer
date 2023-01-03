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

let parse ?output filename =

  let oc = match output with
    | None -> stdout
    | Some file -> open_out file
  in

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
        if
          EzString.starts_with line ~prefix:"clone(" ||
          EzString.starts_with line ~prefix:"clone3(" ||
          EzString.starts_with line ~prefix:"<... vfork resumed>" ||
          EzString.starts_with line ~prefix:"<... clone resumed>" ||
          EzString.starts_with line ~prefix:"<... clone3 resumed>" ||
          false
        then
          let pid = get_retcode line in
          match pid with
          | None ->
              if Filename.check_suffix line "<unfinished ...>" then
                ()
              else
                Printf.eprintf "%d CAN NOT EXTRACT RETCODE '%s'\n%!" p.pid line
          | Some pid ->
              match int_of_string pid with
              | exception _ ->
                  Printf.eprintf "pid %d CAN NOT PARSE RETCODE '%s'\n%!" p.pid line
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
    Printf.fprintf oc "%s%d: %s (%d lines)\n%!" indent p.pid
      (match p.process with
       | Some process -> process
       | None -> "<unknown>")
      ( List.length p.lines )
    ;
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
        print_process "" p;
        List.iteri (fun i line ->
            Printf.fprintf oc "   %d: %s\n%!" i line;
          ) (List.rev p.lines) ;
      end
    ) !map ;

  begin
    match output with
    | None -> ()
    | Some _ -> close_out oc
  end;
  ()






let main () =
  let something_done = ref false in
  let exec_args = ref [] in
  let strace_output = ref "strace.log" in
  let output = ref None in
  let exec_and_parse arg = exec_args := arg :: !exec_args in

  let arg_list = Arg.align [

      "-o", Arg.String (fun s -> output := Some s),
      "FILE where results should be stored";

      "-f", Arg.String (fun s -> strace_output := s),
      "FILE where strace log should be stored";

      "--strace", Arg.Rest exec_and_parse,
      "Run strace with all remaining arguments";

    ] in
  let arg_anon filename =
    something_done := true ;
    Printf.eprintf "Parsing %s\n%!" filename;
    parse ?output:!output filename
  in

  let arg_usage =
    String.concat "\n" [
      Printf.sprintf "%s [OPTIONS] [FILES]: parse strace output" Sys.argv.(0) ;
      "" ;
      "Example:";
      "```";
      "strace -o strace.log -f -e trace=process COMMAND";
      "```";
      "Use of '-o' and '-f' is mandatory. Use of '-s 1024' and '-v' can be useful.";
    ]
  in

  Arg.parse arg_list arg_anon arg_usage ;

  begin
    match !exec_args with
    | [] -> ()
    | args ->
        let args = List.rev args in
        let cmd = [ "strace" ;
                    "-o"; !strace_output ;
                    "-f" ;
                    "-e" ; "trace=process" ;
                    "-s" ; "1024" ;
                  ]
                  @ args
        in
        Printf.eprintf "Calling %s\n%!"
          (String.concat " " cmd );
        let pid =
          Unix.create_process
            "strace"
            ( Array.of_list cmd )
            Unix.stdin Unix.stdout Unix.stderr
        in
        Printf.eprintf "Waiting for pid %d\n%!" pid;
        let code, _status = Unix.waitpid [] pid in
        Printf.eprintf "Command exited with code %d\n%!" code;
        arg_anon !strace_output
  end;

  if not !something_done then
    Printf.eprintf "%s\n%!" arg_usage;
  ()
