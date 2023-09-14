let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

let rec read_stdin lines =
  match maybe_read_line () with
  | Some line -> read_stdin (line :: lines)
  | None -> lines

type commit_info = { revision : string; msg : string }
[@@deriving show]

let print_commits lst =
  List.map show_commit_info lst |> String.concat "\n" |> print_endline

let space_re = Str.regexp " "

let split_once s =
  match Str.bounded_split space_re s 2 with
  | [ rev; msg ] -> Some (rev, msg)
  | _ -> None

let rec records_of_lines_aux lst records =
  match lst with
  | [] -> records
  | hd :: tl ->
      let record =
        match split_once hd with
        | Some (rev, msg) -> { revision = rev; msg }
        | None -> raise (Failure ("failed to parse commit: " ^ hd))
      in
      records_of_lines_aux tl (record :: records)

let records_of_lines_tr lst = records_of_lines_aux lst []

let () =
  let t = Sys.time () in
  let input = read_stdin [] in

  (* let input = String.split_on_char '\n' test in *)
  (* TODO: try to do some paralellization of input processing *)
  (* TODO: or try to split input in shell and use GNU/parallel to
     execute multiple version of the program *)
  let commits = input |> records_of_lines_tr in
  let t1 = Sys.time () in
  print_commits commits;
  print_int @@ List.length commits;
  print_newline ();
  Printf.printf "Execution time: %fs\n" (t1 -. t)
