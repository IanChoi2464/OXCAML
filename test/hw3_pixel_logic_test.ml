open! Core
open Pixel_logic_library
open Hw2_pixel_logic

let ok_exn result = Result.ok result |> Option.value_exn

let%test "Example of a unit test (returns bool)" =
  let state = Game_state.create ~winning_sequence_length:4 ~rows:8 ~columns:8 |> ok_exn in
  let expected_state : Game_state.t =
    { board = Cell_position.Map.empty
    ; rows = 8
    ; columns = 8
    ; winning_sequence_length = 4
    ; decision = In_progress { whose_turn = Red }
    ; last_move = None
    }
  in
  Game_state.equal state expected_state
;;

let create_and_print ~winning_sequence_length ~rows ~columns =
  let result = Game_state.create ~winning_sequence_length ~rows ~columns in
  print_s [%sexp (result : (Game_state.t, Game_state.Create_error.t list) Result.t)]
;;

let%expect_test "Example of an expect_test (returns unit)" =
  create_and_print ~winning_sequence_length:4 ~rows:8 ~columns:8;
  [%expect
    {|
    (Ok
     ((board ()) (rows 8) (columns 8) (winning_sequence_length 4)
      (decision (In_progress (whose_turn Red))) (last_move ())))
    |}]
;;

let%expect_test "Game_state.create fails on big (and small) sizes" =
  create_and_print ~winning_sequence_length:4 ~rows:8 ~columns:8;
  [%expect
    {|
    (Ok
     ((board ()) (rows 8) (columns 8) (winning_sequence_length 4)
      (decision (In_progress (whose_turn Red))) (last_move ())))
    |}];
  create_and_print ~winning_sequence_length:4 ~rows:8 ~columns:20;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~winning_sequence_length:5 ~rows:8 ~columns:8;
  [%expect {| (Error (Unwinnable_sequence_length)) |}];
  create_and_print ~winning_sequence_length:30 ~rows:21 ~columns:21;
  [%expect {| (Error (Board_too_big_or_small Unwinnable_sequence_length)) |}];
  create_and_print ~winning_sequence_length:1 ~rows:0 ~columns:1;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~winning_sequence_length:1 ~rows:1 ~columns:0;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~winning_sequence_length:1 ~rows:1 ~columns:(-10);
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~winning_sequence_length:1 ~rows:(-10) ~columns:1;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~winning_sequence_length:0 ~rows:1 ~columns:1;
  [%expect {| (Error (Unwinnable_sequence_length)) |}];
  create_and_print ~winning_sequence_length:(-10) ~rows:1 ~columns:1;
  [%expect {| (Error (Unwinnable_sequence_length)) |}]
;;

let%expect_test "Game_state.all_directions" =
  print_s [%sexp (Game_state.For_testing.all_directions : (int * int) list)];
  [%expect {| ((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)) |}]
;;

let make_move_and_print game_state cell_position =
  let result = Game_state.make_move game_state cell_position in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)]
;;

let initial_8x8 =
  Game_state.create ~winning_sequence_length:4 ~rows:8 ~columns:8 |> ok_exn
;;

let%expect_test "Game_state.make_move in position (3,3) from empty 8x8 board" =
  make_move_and_print initial_8x8 { row = 3; column = 3 };
  [%expect
    {|
    (Ok
     ((board ((((row 3) (column 3)) Red))) (rows 8) (columns 8)
      (winning_sequence_length 4) (decision (In_progress (whose_turn Blue)))
      (last_move (((row 3) (column 3))))))
    |}]
;;

let%expect_test "Game_state.make_move fails for position (8,7) from empty 8x8 board" =
  make_move_and_print initial_8x8 { row = 8; column = 7 };
  [%expect {| (Error Illegal_cell_position) |}]
;;

let%expect_test "Game_state.make_move fails for corner cell (0,0)" =
  make_move_and_print initial_8x8 { row = 0; column = 0 };
  [%expect {| (Error Corner_cell_not_allowed) |}]
;;

let%expect_test "Game_state.make_move fails for corner cell (0,7)" =
  make_move_and_print initial_8x8 { row = 0; column = 7 };
  [%expect {| (Error Corner_cell_not_allowed) |}]
;;

let%expect_test "Game_state.make_move fails for corner cell (7,0)" =
  make_move_and_print initial_8x8 { row = 7; column = 0 };
  [%expect {| (Error Corner_cell_not_allowed) |}]
;;

let%expect_test "Game_state.make_move fails for corner cell (7,7)" =
  make_move_and_print initial_8x8 { row = 7; column = 7 };
  [%expect {| (Error Corner_cell_not_allowed) |}]
;;

let%expect_test "Game_state.make_move fails if playing twice in same position" =
  let move : Move.t = { row = 3; column = 3 } in
  let state_after_3x3_move = Game_state.make_move initial_8x8 move |> ok_exn in
  make_move_and_print state_after_3x3_move move;
  [%expect {| (Error Space_already_filled) |}]
;;

let pretty_print_board ({ board; rows; columns; decision; _ } : Game_state.t) =
  let row_separator =
    List.range 0 columns |> List.map ~f:(fun _ -> "-") |> String.concat ~sep:"-"
  in
  for row = 0 to rows - 1 do
    List.range 0 columns
    |> List.map ~f:(fun column ->
      match Map.find board { row; column } with
      | None -> " "
      | Some player -> Player_kind.sexp_of_t player |> Sexp.to_string
      | exception _ -> " ")
    |> String.concat ~sep:"|"
    |> print_endline;
    if row < rows - 1 then print_endline row_separator
  done;
  print_s [%sexp (decision : Decision.t)]
;;

let print_final_state game_state cell_positions =
  let result =
    List.fold cell_positions ~init:game_state ~f:(fun new_state cell_position ->
      Game_state.make_move new_state cell_position |> ok_exn)
  in
  pretty_print_board result
;;

let%expect_test "Game_state.make_move: Red makes a move in the middle of the board" =
  print_final_state initial_8x8 [ { row = 3; column = 3 } ];
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red| | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (In_progress (whose_turn Blue))
    |}]
;;

let%expect_test "Game_state.make_move: Red makes a move, then Blue makes a move" =
  print_final_state initial_8x8 [ { row = 3; column = 3 }; { row = 4; column = 4 } ];
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red| | | | |
    ----------------
     | | | |Blue| | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (In_progress (whose_turn Red))
    |}]
;;

let%expect_test "Game_state.make_move: Red wins horizontally (4 in a row)" =
  print_final_state
    initial_8x8
    [ { row = 3; column = 3 }
    ; { row = 4; column = 4 }
    ; { row = 3; column = 4 }
    ; { row = 4; column = 5 }
    ; { row = 3; column = 5 }
    ; { row = 4; column = 6 }
    ; { row = 3; column = 6 }
    ];
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red|Red|Red|Red| |
    ----------------
     | | | |Blue|Blue|Blue| |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (Winner Red)
    |}]
;;

let%expect_test "Game_state.make_move: Blue wins vertically (4 in a row)" =
  print_final_state
    initial_8x8
    [ { row = 3; column = 3 }
    ; { row = 4; column = 4 }
    ; { row = 3; column = 4 }
    ; { row = 5; column = 4 }
    ; { row = 3; column = 5 }
    ; { row = 6; column = 4 }
    ; { row = 3; column = 6 }
    ; { row = 7; column = 4 }
    ];
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red|Red|Red|Red| |
    ----------------
     | | | |Blue| | | |
    ----------------
     | | | |Blue| | | |
    ----------------
     | | | |Blue| | | |
    ----------------
     | | | |Blue| | | |
    (Winner Blue)
    |}]
;;

let%expect_test "Game_state.make_move: Red wins diagonally (4 in a row)" =
  print_final_state
    initial_8x8
    [ { row = 3; column = 3 }
    ; { row = 4; column = 4 }
    ; { row = 4; column = 4 }
    ; { row = 5; column = 5 }
    ; { row = 5; column = 5 }
    ; { row = 6; column = 6 }
    ; { row = 6; column = 6 }
    ; { row = 7; column = 7 }
    ];
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red| | | | |
    ----------------
     | | | |Red| | | |
    ----------------
     | | | | |Red| | |
    ----------------
     | | | | | |Red| |
    ----------------
     | | | | | | |Red|
    (Winner Red)
    |}]
;;

let%expect_test "Game_state.get_all_moves for 8x8 board (excluding corners)" =
  let all_moves = Game_state.get_all_moves initial_8x8 in
  print_s [%message "All moves for 8x8 board" (all_moves : Move.t list)];
  [%expect
    {|
    ("All moves for 8x8 board"
     (all_moves
      (((row 0) (column 1)) ((row 0) (column 2)) ((row 0) (column 3))
       ((row 0) (column 4)) ((row 0) (column 5)) ((row 0) (column 6))
       ((row 1) (column 0)) ((row 1) (column 1)) ((row 1) (column 2))
       ((row 1) (column 3)) ((row 1) (column 4)) ((row 1) (column 5))
       ((row 1) (column 6)) ((row 1) (column 7)) ((row 2) (column 0))
       ((row 2) (column 1)) ((row 2) (column 2)) ((row 2) (column 3))
       ((row 2) (column 4)) ((row 2) (column 5)) ((row 2) (column 6))
       ((row 2) (column 7)) ((row 3) (column 0)) ((row 3) (column 1))
       ((row 3) (column 2)) ((row 3) (column 3)) ((row 3) (column 4))
       ((row 3) (column 5)) ((row 3) (column 6)) ((row 3) (column 7))
       ((row 4) (column 0)) ((row 4) (column 1)) ((row 4) (column 2))
       ((row 4) (column 3)) ((row 4) (column 4)) ((row 4) (column 5))
       ((row 4) (column 6)) ((row 4) (column 7)) ((row 5) (column 0))
       ((row 5) (column 1)) ((row 5) (column 2)) ((row 5) (column 3))
       ((row 5) (column 4)) ((row 5) (column 5)) ((row 5) (column 6))
       ((row 5) (column 7)) ((row 6) (column 0)) ((row 6) (column 1))
       ((row 6) (column 2)) ((row 6) (column 3)) ((row 6) (column 4))
       ((row 6) (column 5)) ((row 6) (column 6)) ((row 6) (column 7))
       ((row 7) (column 1)) ((row 7) (column 2)) ((row 7) (column 3))
       ((row 7) (column 4)) ((row 7) (column 5)) ((row 7) (column 6)))))
    |}]
;;

let%expect_test "Test HW1 states: initial_state" =
  let hw1_initial = Hw1.initial_state in
  pretty_print_board hw1_initial;
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red| | | | |
    ----------------
     | | | |Blue| | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (In_progress (whose_turn Red))
    |}]
;;

let%expect_test "Test HW1 states: state_after_move_at_3x4" =
  let hw1_state = Hw1.state_after_move_at_3x4 in
  pretty_print_board hw1_state;
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red|Red| | | |
    ----------------
     | | | |Blue| | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (In_progress (whose_turn Blue))
    |}]
;;

let%expect_test "Test HW1 states: before_terminal_state" =
  let hw1_before = Hw1.before_terminal_state in
  pretty_print_board hw1_before;
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red|Red|Red| | |
    ----------------
     | | | |Blue|Blue| | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (In_progress (whose_turn Red))
    |}]
;;

let%expect_test "Test HW1 states: terminal_state" =
  let hw1_terminal = Hw1.terminal_state in
  pretty_print_board hw1_terminal;
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red|Red|Red|Red| |
    ----------------
     | | | |Blue|Blue| | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (Winner Red)
    |}]
;;

let random_walk (initial_state : Game_state.t) ~random_seed =
  let rec random_walk (state : Game_state.t) =
    let all_moves = Game_state.get_all_moves state in
    let next_states =
      List.filter_map all_moves ~f:(fun move ->
        Game_state.make_move state move |> Result.ok)
    in
    let random_state = List.random_element next_states |> Option.value_exn in
    match Decision.is_game_over random_state.decision with
    | true -> random_state
    | false -> random_walk random_state
  in
  (* Set random seed. *)
  Core.Random.init random_seed;
  pretty_print_board (random_walk initial_state)
;;

let%expect_test "Pixel Game random walk till terminal state" =
  random_walk initial_8x8 ~random_seed:1;
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red|Red|Red|Red| |
    ----------------
     | | | |Blue|Blue|Blue| |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (Winner Red)
    |}];
  random_walk initial_8x8 ~random_seed:3;
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red|Red|Red|Red| |
    ----------------
     | | | |Blue|Blue|Blue| |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (Winner Red)
    |}];
  random_walk initial_8x8 ~random_seed:1234;
  [%expect
    {|
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | |Red|Red|Red|Red| |
    ----------------
     | | | |Blue|Blue|Blue| |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (Winner Red)
    |}]
;;
