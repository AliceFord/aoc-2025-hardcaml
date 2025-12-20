open! Core
open! Hardcaml
open! Signal

let num_bits = 4
let num_bits_out = 64

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; part: 'a
    ; data_in : 'a [@bits num_bits]
    ; data_sep : 'a [@bits 1]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    {
      sum_value : 'a [@bits 64];
      sum_valid : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs1
    | Done1
    | Accepting_inputs2
    | Done2
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; part; data_in; data_sep; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm =
    State_machine.create (module States) spec
  in
  let%hw_var max_so_far = Variable.reg spec ~width:num_bits in
  let%hw_var snd_max_so_far = Variable.reg spec ~width:num_bits in

  let%hw_var m1 = Variable.reg spec ~width:num_bits in
  let%hw_var m2 = Variable.reg spec ~width:num_bits in
  let%hw_var m3 = Variable.reg spec ~width:num_bits in
  let%hw_var m4 = Variable.reg spec ~width:num_bits in
  let%hw_var m5 = Variable.reg spec ~width:num_bits in
  let%hw_var m6 = Variable.reg spec ~width:num_bits in
  let%hw_var m7 = Variable.reg spec ~width:num_bits in
  let%hw_var m8 = Variable.reg spec ~width:num_bits in
  let%hw_var m9 = Variable.reg spec ~width:num_bits in
  let%hw_var m10 = Variable.reg spec ~width:num_bits in
  let%hw_var m11 = Variable.reg spec ~width:num_bits in
  let%hw_var m12 = Variable.reg spec ~width:num_bits in

  let maxes = [m1;m2;m3;m4;m5;m6;m7;m8;m9;m10;m11;m12] in

  let shift_up n = (* starting at m<n>, shift all lower bits up *)
    List.map2_exn ~f:(fun ma mb -> ma <-- mb.value) (List.take (List.drop maxes (n-1)) (12-n)) (List.drop maxes n) @ [m12 <-- data_in]
  in
  let set_maxes_zero () : Always.t list = 
    List.map ~f:(fun ma -> ma <-- zero num_bits) maxes;
  in
  let get_sum () =
    let new_list = List.mapi ~f:(fun i ma -> ma.value *: (of_int_trunc (Int.pow 10 (11-i)) ~width:(num_bits_out - num_bits))) (List.take maxes 11) in
    List.fold_left new_list ~f:(fun acc x -> (x +: acc)) ~init:(uresize m12.value ~width:num_bits_out)
  in

  let sum = Variable.reg spec ~width:num_bits_out in
  let sum_valid = Variable.reg spec ~width:1 in

  (* logic: store max and second max seen thus far. update. sum. *)

  (* logic for second part: same. it's ridiculous, but fast. *)

  compile
    [ sm.switch
        [ ( Idle
          , [ when_ start [ 
                sum <-- zero num_bits_out;
                sum_valid <-- gnd;
                if_ (part) ([sm.set_next Accepting_inputs2] @ set_maxes_zero ()) [
                  max_so_far <-- zero num_bits;
                  snd_max_so_far <-- zero num_bits;
                  sm.set_next Accepting_inputs1;
                ]
              ]
            ] )
        ; ( Accepting_inputs1
          , [ when_ data_in_valid [
                if_ (snd_max_so_far.value >: max_so_far.value) [
                  max_so_far <-- snd_max_so_far.value;
                  snd_max_so_far <-- data_in;
                ] [
                  when_ (data_in >: snd_max_so_far.value) [
                    snd_max_so_far <-- data_in;
                  ]
                ]
              ];
              when_ data_sep [ 
                sum <-- sum.value +: (max_so_far.value *: (of_int_trunc 10 ~width:60) +: (uresize snd_max_so_far.value ~width:num_bits_out));
                max_so_far <-- zero num_bits;
                snd_max_so_far <-- zero num_bits;
              ];
              when_ finish [ sm.set_next Done1 ]
            ] )
        ; ( Done1
          , [ 
              when_ (max_so_far.value >:. 0) [
                sum <-- sum.value +: (max_so_far.value *: (of_int_trunc 10 ~width:60) +: (uresize snd_max_so_far.value ~width:num_bits_out));
                max_so_far <-- zero num_bits;
                snd_max_so_far <-- zero num_bits;
              ];

              sum_valid <-- vdd;
              sm.set_next Idle
            ] )
        ; ( Accepting_inputs2
          , [ when_ data_in_valid [
                if_ (m2.value >: m1.value) (shift_up 1) [
                if_ (m3.value >: m2.value) (shift_up 2) [
                if_ (m4.value >: m3.value) (shift_up 3) [
                if_ (m5.value >: m4.value) (shift_up 4) [
                if_ (m6.value >: m5.value) (shift_up 5) [
                if_ (m7.value >: m6.value) (shift_up 6) [
                if_ (m8.value >: m7.value) (shift_up 7) [
                if_ (m9.value >: m8.value) (shift_up 8) [
                if_ (m10.value >: m9.value) (shift_up 9) [
                if_ (m11.value >: m10.value) (shift_up 10) [
                if_ (m12.value >: m11.value) (shift_up 11) [
                if_ (data_in >: m12.value) (shift_up 12) [
                ]]]]]]]]]]]]
              ];
              when_ data_sep ([sum <-- sum.value +: get_sum ()] @ set_maxes_zero ());
              when_ finish [ sm.set_next Done2 ]
            ] )
        ; ( Done2
          , [ 
              when_ (max_so_far.value >:. 0) ([sum <-- sum.value +: get_sum ()] @ set_maxes_zero ());

              sum_valid <-- vdd;
              sm.set_next Idle
            ] )
        ]
    ];
  { 
    sum_value = sum.value;
    sum_valid = sum_valid.value;
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day3" create
;;
