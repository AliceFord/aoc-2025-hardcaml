open! Core
open! Hardcaml

module Day1 = Aoc_day1.Day1
module Day3 = Aoc_day3.Day3
module Day7 = Aoc_day7.Day7

let generate_day1_rtl () =
  let module C = Circuit.With_interface (Day1.I) (Day1.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"day1_top" (Day1.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let generate_day3_rtl () =
  let module C = Circuit.With_interface (Day3.I) (Day3.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"day1_top" (Day3.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let generate_day7_rtl () =
  let module C = Circuit.With_interface (Day7.I) (Day7.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"day1_top" (Day7.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let day1_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_day1_rtl ()]
;;

let day3_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_day3_rtl ()]
;;

let day7_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_day7_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ 
      "day1", day1_rtl_command;
      "day3", day3_rtl_command;
      "day7", day7_rtl_command
    ])
;;
