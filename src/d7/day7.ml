open! Core
open! Hardcaml
open! Signal

let num_bits = 16
let char_bits = 8
let max_beam_width = 150

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits char_bits]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    {
      out : 'a With_valid.t [@bits num_bits]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let or_reduce s =
  let n = width s in
  let rec loop i acc =
    if i >= n then acc
    else
      let bit = s.:(i) in
      let acc = match acc with
        | None -> Some bit
        | Some a -> Some (a |: bit)
      in
      loop (i+1) acc
  in
  match loop 0 None with
  | Some result -> result
  | None -> gnd  (* s has width 0, unlikely *)

let create scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm =
    State_machine.create (module States) spec
  in
  let%hw_var current_beams = Variable.reg spec ~width:max_beam_width in
  let%hw_var pointer = Variable.reg spec ~width:max_beam_width in

  (* let t = Variable.reg spec ~width:num_bits in *)

  let out = Variable.reg spec ~width:num_bits in
  let out_valid = Variable.reg spec ~width:1 in

  compile
    [ sm.switch
        [ ( Idle
          , [ when_ start [
                pointer <-- one max_beam_width;
                current_beams <-- zero max_beam_width;
                out <-- zero num_bits;
                out_valid <-- gnd;
                sm.set_next Accepting_inputs
              ]
            ] )
        ; ( Accepting_inputs
          , [ when_ data_in_valid [
                pointer <-- sll pointer.value ~by:1;
                switch data_in [
                  (of_char 'S', [
                    current_beams <-- (current_beams.value |: pointer.value)
                  ]);
                  (of_char '^', [
                    when_ (or_reduce (current_beams.value &: pointer.value)) [
                      out <-- out.value +:. 1;
                      current_beams <-- (current_beams.value |: (srl pointer.value ~by:1) &: (~: (pointer.value)) |: (sll pointer.value ~by:1)); (* set lower beam, unset current beam, set higher beam *)
                    ]
                  ]);
                  (of_char '\n', [
                    pointer <-- one max_beam_width;
                  ]);
                ];
              ];
              when_ finish [ sm.set_next Done ]
            ] )
        ; ( Done
          , [
              out_valid <-- vdd;
              when_ finish [ sm.set_next Idle ]
            ] )
        ]
    ];
  { 
    out = { value = out.value; valid = out_valid.value };
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day7" create
;;
