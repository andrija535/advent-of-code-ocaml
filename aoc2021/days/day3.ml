let get_bit_list line = 
  String.split_on_char ' ' line 
  |> List.map int_of_string

let get_columns (rows: 'a list list): 'a list list =
  match rows with
  | [] -> failwith "Empty input"
  | x::xs ->
      let x = List.(map singleton x) in
      List.fold_left (fun acc row ->
        List.to_seq acc
        |> Seq.zip (List.to_seq row)
        |> Seq.map (fun (a, b) -> a::b)
        |> List.of_seq
      ) x xs

let get_most_common_bit (bits: int list): int =
  let (count0, count1) = 
    List.fold_left (fun (count0, count1) bit ->
      match bit with
      | 0 -> (count0 + 1, count1)
      | 1 -> (count0, count1 + 1)
      | _ -> failwith "Invalid bit"
    ) (0, 0) bits 
  in
  if count1 >= count0 then 1 else 0

let bits_to_int (bits: int list): int =
  List.to_seq bits
  |> Seq.zip (Seq.iterate (( * ) 2) 0)
  |> Seq.fold_left (fun acc (bit, power) -> acc + (bit * power)) 0

let run (lines: string Seq.t): int =
  let gamma = List.(
    of_seq lines
    |> map get_bit_list
    |> get_columns
    |> map get_most_common_bit
    |> bits_to_int
  ) in
  let epilson = lnot gamma in
  gamma * epilson
