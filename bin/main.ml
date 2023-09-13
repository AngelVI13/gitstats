let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

let rec read_stdin lines =
  match maybe_read_line () with
  | Some line -> read_stdin (lines @ [ line ])
  | None -> lines

(*
let test =
  "704800d8d (HEAD -> develop, tag: Sprint_Releases/S54_July_21_2023, \
   origin/develop, origin/HEAD) Merged PR 3787: VL 52 fix for 4AP2-44364\n\
   4e06e981d updated docstrings\n\
   32d86fffa Apply suggestions from code review"
*)

(*
let print_lines lst =
    let out = List.mapi (
        fun i el -> 
            Printf.sprintf "%d, %s\n" i el 
    ) lst in 
    String.concat " " out |> print_endline
*)

type commit_info = { revision : string; msg : string }

let print_commits lst =
  let out =
    List.mapi
      (fun i el -> Printf.sprintf "%d: [%s] - %s\n" i el.revision el.msg)
      lst
  in
  String.concat " " out |> print_endline

let split_once s =
  let expr = Str.regexp " " in
  match Str.bounded_split expr s 2 with
  | [ rev; msg ] -> Some (rev, msg)
  | _ -> None

let rec records_of_lines lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      let record =
        match split_once hd with
        | Some (rev, msg) -> { revision = rev; msg }
        | None -> raise (Failure ("failed to parse commit: " ^ hd))
      in
      record :: records_of_lines tl

let () =
  let input = read_stdin [] in
  (* let input = String.split_on_char '\n' test in *)

  let t = Sys.time() in
  let commits = input |> records_of_lines in
  let t1 = Sys.time() in
  print_commits commits;
  Printf.printf "Execution time: %fs\n" (t1 -. t);
