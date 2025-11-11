open Aoc_helpers.Runners

let day_mapping : int -> (int,string) day_solution = function
  | 3 -> { day1 = Days.Day3.day1; day2 = Days.Day3.day2 }
  | day -> failwith (Printf.sprintf "Day %d not implemented" day)

let () =
  run
    ~exec_name:"aoc2021"
    ~day_mapping
    ~printer_day1:Format.pp_print_int
    ~printer_day2:Format.pp_print_string

