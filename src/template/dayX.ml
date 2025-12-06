open! Core
open! Hardcaml
open! Signal

let num_bits = 16

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; num_in : 'a [@bits num_bits]
    ; dir_in : 'a [@bits 1]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    {
      num_zeros : 'a With_valid.t [@bits num_bits];
      ready_for_input : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Mod100
    | Just_finished
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; num_in; dir_in; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm =
    State_machine.create (module States) spec
  in
  let%hw_var pos = Variable.reg spec ~width:num_bits in

  let num_zeros = Variable.reg spec ~width:num_bits in
  let num_zeros_valid = Variable.reg spec ~width:1 in
  let ready_for_input = Variable.reg spec ~width:1 in

  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ pos <--. 50;
                  num_zeros <-- zero num_bits;
                  num_zeros_valid <-- gnd;
                  ready_for_input <-- vdd;
                  sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_
                data_in_valid
                [ 
                  ready_for_input <-- gnd; (* no longer ready for input *)
                  if_ (dir_in ==:. 1) [
                    if_ (pos.value +: num_in >=+. 100) [
                      pos <-- pos.value +: num_in -:. 100;
                      sm.set_next Mod100;
                    ] [
                      pos <-- pos.value +: num_in;
                      ready_for_input <-- vdd; (* no modulo needed *)
                    ];
                  ] [
                    if_ (num_in >+ pos.value) [
                      pos <-- pos.value -: num_in +:. 100;
                      sm.set_next Mod100;
                    ] [
                      pos <-- pos.value -: num_in;
                      ready_for_input <-- vdd; (* no modulo needed *)
                    ];

                    (*when_ (pos.value -: num_in ==:. 0) [
                      num_zeros <-- (num_zeros.value +:. 1);
                    ] (* pt 2 *)*)
                  ];

                  when_ (pos.value ==:. 0) [
                    num_zeros <-- (num_zeros.value +:. 1);
                  ] (* pt 1 *)
                ]
            ; when_ finish [ sm.set_next Just_finished ]
            ] )
        ; ( Mod100
          , [ 
              if_ (pos.value >=+. 100) [
                pos <-- pos.value -:. 100;
              ] [
                if_ (pos.value <+. 0) [
                  pos <-- pos.value +:. 100;
                ] [
                  ready_for_input <-- vdd;
                  sm.set_next Accepting_inputs;
                ]
              ];
              (*num_zeros <-- (num_zeros.value +:. 1) (* pt 2  *)*)
            ] )
        ; ( Just_finished
          , [ 
              when_ (pos.value ==:. 0) [ (* final check *)
                num_zeros <-- (num_zeros.value +:. 1);
              ];
              num_zeros_valid <-- vdd;
              sm.set_next Done
            ] )
        ; ( Done
          , [ 
              when_ finish [ sm.set_next Accepting_inputs ]
            ] )
        ]
    ];
  { 
    num_zeros = { value = num_zeros.value; valid = num_zeros_valid.value };
    ready_for_input = ready_for_input.value
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day3" create
;;
