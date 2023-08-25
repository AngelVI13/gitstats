let maybe_read_line () =
    try Some(read_line ())
    with End_of_file -> None


let rec read_stdin lines =
    match maybe_read_line () with
    (* this does not use tail recursion and its not very performant because of it *)
    | Some(line) -> read_stdin (lines @ [line]) (* for better perf: line :: lines *)
    | None -> lines

let rec find_commits lines commit_idxs idx =
    match lines with 
    | [] -> commit_idxs
    | (hd :: tl) -> begin
        let commit_idxs = 
            if String.starts_with ~prefix:"commit " hd then 
                (idx :: commit_idxs) 
            else commit_idxs in
        find_commits tl commit_idxs (idx+1)
    end

let find_commit_idxs lines = 
    find_commits lines [] 0

let () = 
    let commit_idxs = [] |> read_stdin |> find_commit_idxs in
    List.iter (Printf.printf "%d ") commit_idxs;
    (* how best to do string processing ? *)
    print_endline ""
