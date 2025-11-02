let read_line_opt (inc: in_channel): string option =
  try
    let s = input_line inc in
    Some s
  with
  | End_of_file -> None
  | e -> raise e

let read_lines (file_path: string) : string Seq.t =
  let inc = open_in file_path in
  Seq.unfold
    (fun inc ->
      read_line_opt inc |> Option.map @@ fun s -> (s,inc))
    inc
  
