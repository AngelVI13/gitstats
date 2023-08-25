let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

let rec read_stdin lines =
  match maybe_read_line () with
  (* this does not use tail recursion and its not very performant because of it *)
  | Some line ->
      read_stdin (lines @ [ line ]) (* for better perf: line :: lines *)
  | None -> lines

let rec find_commits lines commits =
  match lines with
  | [] -> commits
  | hd :: tl ->
      print_endline hd;
      let commits =
        match Scanf.sscanf_opt hd "commit %S" Fun.id with
        | None -> commits
        | Some c ->
            Printf.printf "%s" c;
            c :: commits
      in
      find_commits tl commits

let find_commits_new lines = find_commits lines []

let () =
  let commits = [] |> read_stdin |> find_commits_new in
  List.iter (Printf.printf "%s ") commits
