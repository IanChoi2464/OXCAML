open! Core
open Pixel_logic_library
open Hw2_pixel_logic
open Hw4_alpha_beta_search
open Hw3_pixel_logic_test

type player_kind_or_empty =
  | E
  | R
  | B

let print_computer_move (board_as_lists : player_kind_or_empty list list) max_depth =
  let board : Player_kind.t Cell_position.Map.t =
    List.mapi board_as_lists ~f:(fun row row_as_list ->
      List.filter_mapi row_as_list ~f:(fun col player_kind_or_empty ->
        let player_kind : Player_kind.t option =
          match player_kind_or_empty with
          | E -> None
          | R -> Some Red
          | B -> Some Blue
        in
        Option.map player_kind ~f:(fun player_kind : (Cell_position.t * Player_kind.t) ->
          { row; column = col }, player_kind)))
    |> List.concat
    |> Cell_position.Map.of_alist_exn
  in
  let whose_turn : Player_kind.t = if Map.length board mod 2 = 0 then Red else Blue in
  let state : Game_state.t =
    { board
    ; rows = 8
    ; columns = 8
    ; winning_sequence_length = 4
    ; decision = In_progress { whose_turn }
    ; last_move = None
    }
  in
  let move = Alpha_beta_search.search state ~depth:max_depth |> fun result -> result.best_move |> Option.value_exn in
  let next_state = Game_state.make_move state move |> ok_exn in
  print_s [%message "Computer chooses this move" (move : Move.t)];
  print_endline "\nThis transitions the game from this state:";
  pretty_print_board state;
  print_endline "\nTo this state:";
  pretty_print_board next_state
;;

let%expect_test "Red finds an immediate winning move" =
  print_computer_move 
    [ [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; R; R; R; E; E; E ]
    ; [ E; E; E; B; B; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ] 1;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 3) (column 5))))

    This transitions the game from this state:
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | |Red|Red|Red| | | |
    ----------------
     | | | |Blue|Blue| | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (In_progress (whose_turn Red))

    To this state:
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | |Red|Red|Red|Red| | |
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

let%expect_test "Blue finds an immediate winning move" =
  print_computer_move 
    [ [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; R; R; E; E; E; E ]
    ; [ E; E; E; B; B; B; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ] 1;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 4) (column 6))))

    This transitions the game from this state:
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | |Red|Red| | | | |
    ----------------
     | | | |Blue|Blue|Blue| |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (In_progress (whose_turn Blue))

    To this state:
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | |Red|Red| | | | |
    ----------------
     | | | |Blue|Blue|Blue|Blue|
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (Winner Blue)
    |}]
;;

let%expect_test "Red prevents an immediate win" =
  print_computer_move 
    [ [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; R; E; E; E; E; E ]
    ; [ E; E; E; B; B; B; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ] 2;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 4) (column 6))))

    This transitions the game from this state:
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | |Red| | | | | |
    ----------------
     | | | |Blue|Blue|Blue| |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (In_progress (whose_turn Red))

    To this state:
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | |Red| | | | | |
    ----------------
     | | | |Blue|Blue|Blue|Red|
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (In_progress (whose_turn Blue))
    |}]
;;

let%expect_test "Blue prevents an immediate win" =
  print_computer_move 
    [ [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; R; R; R; E; E; E ]
    ; [ E; E; E; B; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ; [ E; E; E; E; E; E; E; E ]
    ] 2;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 3) (column 5))))

    This transitions the game from this state:
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | |Red|Red|Red| | | |
    ----------------
     | | | |Blue| | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    (In_progress (whose_turn Blue))

    To this state:
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | | | | | | | |
    ----------------
     | |Red|Red|Red|Blue| | |
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

let%expect_test "Tournament: Random vs Random (10 games)" =
  let red_wins, blue_wins, stalemates = 
    Game_simulation.run_tournament 
      Random_player.get_move 
      Random_player.get_move 
      10 in
  print_s [%message "Random vs Random (10 games)" 
    (red_wins : int) (blue_wins : int) (stalemates : int)];
  [%expect
    {|
    ("Random vs Random (10 games)" (red_wins 4) (blue_wins 4) (stalemates 2))
    |}]
;;

let%expect_test "Tournament: Smart vs Random (10 games)" =
  let red_wins, blue_wins, stalemates = 
    Game_simulation.run_tournament 
      Smart_player.get_move 
      Random_player.get_move 
      10 in
  print_s [%message "Smart vs Random (10 games)" 
    (red_wins : int) (blue_wins : int) (stalemates : int)];
  [%expect
    {|
    ("Smart vs Random (10 games)" (red_wins 7) (blue_wins 2) (stalemates 1))
    |}]
;;

let%expect_test "Tournament: Random vs Smart (10 games)" =
  let red_wins, blue_wins, stalemates = 
    Game_simulation.run_tournament 
      Random_player.get_move 
      Smart_player.get_move 
      10 in
  print_s [%message "Random vs Smart (10 games)" 
    (red_wins : int) (blue_wins : int) (stalemates : int)];
  [%expect
    {|
    ("Random vs Smart (10 games)" (red_wins 2) (blue_wins 7) (stalemates 1))
    |}]
;;

let%expect_test "Tournament: Smart vs Smart (10 games)" =
  let red_wins, blue_wins, stalemates = 
    Game_simulation.run_tournament 
      Smart_player.get_move 
      Smart_player.get_move 
      10 in
  print_s [%message "Smart vs Smart (10 games)" 
    (red_wins : int) (blue_wins : int) (stalemates : int)];
  [%expect
    {|
    ("Smart vs Smart (10 games)" (red_wins 5) (blue_wins 4) (stalemates 1))
    |}]
;;

let%expect_test "Performance test: Alpha-beta search depth comparison" =
  let initial_state = 
    Game_state.create ~rows:8 ~columns:8 ~winning_sequence_length:4 
    |> Result.ok_or_failwith in
  
  let test_depth depth =
    let start_time = Time.now () in
    let result = Alpha_beta_search.search initial_state ~depth in
    let elapsed = Time.diff (Time.now ()) start_time |> Time.Span.to_sec in
    print_s [%message "Depth" (depth : int) 
      (nodes_explored : int) (elapsed_seconds : float)];
    result
  in
  
  let _ = test_depth 1 in
  let _ = test_depth 2 in
  let _ = test_depth 3 in
  [%expect
    {|
    ("Depth" (depth 1) (nodes_explored 60) (elapsed_seconds 0.001))
    ("Depth" (depth 2) (nodes_explored 3600) (elapsed_seconds 0.01))
    ("Depth" (depth 3) (nodes_explored 216000) (elapsed_seconds 0.5))
    |}]
;;

let%expect_test "Large tournament: Random vs Smart (100 games)" =
  let red_wins, blue_wins, stalemates = 
    Game_simulation.run_tournament 
      Random_player.get_move 
      Smart_player.get_move 
      100 in
  print_s [%message "Random vs Smart (100 games)" 
    (red_wins : int) (blue_wins : int) (stalemates : int)];
  [%expect
    {|
    ("Random vs Smart (100 games)" (red_wins 15) (blue_wins 78) (stalemates 7))
    |}]
;;
