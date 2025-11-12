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

let gamma_bits : string list -> int list = function
  | [] -> failwith "Empty input"
  | x::xs -> 
      let x = get_bit_list x in
      List.fold_left (fun acc line -> 
        let bits = get_bit_list line in
        List.map2 (fun x y -> if y = 1 then x+1 else x-1) acc bits
      ) x xs
  |> List.map (fun count -> if count > 0 then 1 else 0)


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
