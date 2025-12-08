open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Day3 = Aoc_day3.Day3
module Harness = Cyclesim_harness.Make (Day3.I) (Day3.O)

let ( <--. ) = Bits.( <--. )

let string_to_char_list s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let parse_d3_line l =
  let clist = string_to_char_list l in
  List.map clist ~f:(fun c -> int_of_char c - int_of_char '0')

let d3_from_file filename =
  let chan = In_channel.create filename in
  let rec loop acc =
    match In_channel.input_line chan with
    | None ->
        In_channel.close chan;
        List.rev acc
    | Some line ->
        loop (parse_d3_line line :: acc)
  in
  loop []

(* let test_input = [[9;8;7;6;5;4;3;2;1;1;1;1;1;1;1]] *)

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one value *)
  let feed_input n =
    inputs.data_in <--. n;
    inputs.data_in_valid := Bits.vdd;
    cycle ();
    inputs.data_in_valid := Bits.gnd;
    (* cycle () *)
  in
  let feed_break () =
    inputs.data_sep := Bits.vdd;
    cycle ();
    inputs.data_sep := Bits.gnd;
    cycle ()
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  cycle ();
  (* Input some data *)
  List.iter (d3_from_file "/mnt/c/Users/olive/Documents/coding/aoc2025/hardcaml_template_project/d3_full.txt") ~f:(fun xs -> 
    List.iter xs ~f:(fun n -> feed_input n);
    feed_break ()
  );
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  while not (Bits.to_bool !(outputs.sum.valid)) do
    cycle ()
  done;
  let sum = Bits.to_unsigned_int !(outputs.sum.value) in
  print_s [%message "Result" (sum : int)];
  cycle ~n:2 ()
;;

let waves_config =
  Waves_config.to_directory "/mnt/c/Users/olive/Documents/coding/aoc2025/hardcaml_template_project/"
  |> Waves_config.as_wavefile_format ~format:Vcd
;;

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Harness.run_advanced ~waves_config ~create:Day3.hierarchical simple_testbench;
  [%expect {| (Result (num_zeros 1)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day3*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Day3.hierarchical
    ~trace:`Everything
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    simple_testbench;
  [%expect
    {|
    (Result (range 146))
    ┌
    |}]
;;
