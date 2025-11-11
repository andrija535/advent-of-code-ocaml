(* Ignores order because it shouldn't matter for this problem *)
let get_columns (rows: 'a list list): 'a list list =
  match rows with
  | [] -> failwith "Empty input"
  | x::xs ->
      let open List in
      let x = map singleton x in
      fold_left (fun acc row ->
        to_seq acc
        |> Seq.zip (to_seq row)
        |> Seq.map (fun (a, b) -> a::b)
        |> of_seq
      ) x xs

let get_most_common_bit (bits: int list): int =
  let s = List.fold_left (fun s x -> if x = 1 then s + 1 else s - 1) 0 bits in
  if s >= 0 then 1 else 0

let bits_to_int (bits: int list): int =
  List.(to_seq @@ rev bits)
  |> Seq.zip (Seq.iterate ((+) 1) 0)
  |> Seq.fold_left (fun acc (power, bit) -> acc + bit * int_of_float (2. ** float_of_int power)) 0

let get_bit_list line = 
  let bit_to_int = function
    | '0' -> 0
    | '1' -> 1
    | c -> failwith @@ Printf.sprintf "Invalid bit character: %c" c
  in
  String.to_seq line
  |> Seq.map bit_to_int
  |> List.of_seq

let gamma_bits (lines: string list): int list =
  List.(map get_bit_list lines
  |> get_columns
  |> map get_most_common_bit)

let flip_bits (bits: int list): int list =
  List.map (function
    | 0 -> 1
    | 1 -> 0
    | b -> failwith @@ Printf.sprintf "Invalid bit to flip: %d" b
  ) bits

let day1 (lines: string Seq.t): int =
  let lines = List.of_seq lines in
  let gamma = gamma_bits lines in
  let epilson = flip_bits gamma in
  bits_to_int gamma * bits_to_int epilson

let day2 (_lines: string Seq.t): string = "Not implemented yet"
