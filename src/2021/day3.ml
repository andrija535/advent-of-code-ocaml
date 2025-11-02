let most_common_bits (rows: int list list) : int list =
  get_bits_of_columns rows
  |> List.(map sort)

let power_consumption ~(gamma:int) ~(epilson:int): int =
  gamma * epilson
