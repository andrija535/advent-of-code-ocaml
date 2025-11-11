type runner_configuration = {
  day: int;
  input_file: string
}

type ('a,'b) day_solution = {
  day1: string Seq.t -> 'a;
  day2: string Seq.t -> 'b
}

type ('a,'b) day_mapping = int -> ('a,'b) day_solution

let day = ref 1
let input_file = ref None

let arg_spec = [
  ("-day", Arg.Set_int day, "Specify the day to run");
  ("-input-file", Arg.String (fun s -> input_file := Some s), "Specify the input file")
]

let usage_message exec_name = Printf.sprintf "Usage: %s [-day N] [-input-file FILE]" exec_name

let parse_args (exec_name: string): unit =
  Arg.parse 
    arg_spec 
    (Fun.const ()) @@
    usage_message exec_name

let get_config (exec_name: string) : runner_configuration =
  let input_file =
    Option.map (fun input_file ->
      { day = !day; input_file }
    ) !input_file 
  in
  match input_file with
  | None -> 
      Printf.eprintf "%s\n" @@ usage_message exec_name;
      exit 1
  | Some config -> config

let run 
  ~(exec_name: string) 
  ~(day_mapping: ('a,'b) day_mapping)
  ~(printer_day1: Format.formatter -> 'a -> unit)
  ~(printer_day2: Format.formatter -> 'b -> unit) =
  parse_args exec_name;
  let runner_config = get_config exec_name in
  let lines = Io.read_lines runner_config.input_file in
  let day_solutions = day_mapping runner_config.day in
  let d1,d2 = day_solutions.day1 lines, day_solutions.day2 lines in
  Format.printf "Result of day %d:\nPart 1: %a\nPart 2: %a\n"
    runner_config.day
    printer_day1 d1
    printer_day2 d2
