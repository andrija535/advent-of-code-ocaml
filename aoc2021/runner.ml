let day_mapping = function
  | 3 -> Days.Day3.run
  | day -> failwith (Printf.sprintf "Day %d not implemented" day)

let () =
  Aoc_helpers.Runners.run
    ~exec_name:"aoc2021"
    ~day_mapping
    ~printer:Format.pp_print_int

