open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Day7 = Aoc_day7.Day7
module Harness = Cyclesim_harness.Make (Day7.I) (Day7.O)

let ( <--. ) = Bits.( <--. )

let d7_from_file filename =
  let chan = In_channel.create filename in
  let rec loop acc =
    match In_channel.input_byte chan with
    | None ->
        In_channel.close chan;
        List.rev acc
    | Some c ->
        loop (c :: acc)
  in
  loop []

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one value *)
  let feed_input c =
    inputs.data_in <--. c;
    inputs.data_in_valid := Bits.vdd;
    cycle ();
    inputs.data_in_valid := Bits.gnd;
    (* cycle () *)
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
  (* Input some data *)
  List.iter (d7_from_file "/mnt/c/Users/olive/Documents/coding/aoc2025/hardcaml_template_project/d7_full.txt") ~f:(fun c -> feed_input c);
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  while not (Bits.to_bool !(outputs.out.valid)) do
    cycle ()
  done;
  let out = Bits.to_unsigned_int !(outputs.out.value) in
  print_s [%message "Result" (out : int)];
  cycle ~n:2 ()
;;

let waves_config =
  Waves_config.to_directory "/mnt/c/Users/olive/Documents/coding/aoc2025/hardcaml_template_project/"
  |> Waves_config.as_wavefile_format ~format:Vcd
;;

let%expect_test "Simple test with printing waveforms directly" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day7*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Day7.hierarchical
    ~trace:`Everything
    ~waves_config
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        waves)
    simple_testbench;
  [%expect
    {|
    (Result (range 146))
    ┌
    |}]
;;
