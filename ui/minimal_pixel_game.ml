open! Core
open Pixel_logic_library
open Hw2_pixel_logic

let print_board (game_state : Game_state.t) =
  print_endline "Pixel Game Board:";
  print_endline "==================";
  for row = 0 to game_state.rows - 1 do
    for col = 0 to game_state.columns - 1 do
      let cell_value = Map.find game_state.board { row; column = col } in
      let symbol = 
        match cell_value with
        | Some Red -> "R"
        | Some Blue -> "B"
        | None -> "."
      in
      printf "%s " symbol
    done;
    print_endline ""
  done;
  print_endline "=================="

let print_game_info (game_state : Game_state.t) =
  match game_state.decision with
  | In_progress { whose_turn } ->
    printf "Current Player: %s\n" 
      (match whose_turn with Red -> "Red" | Blue -> "Blue")
  | Winner player ->
    printf "Winner: %s\n" 
      (match player with Red -> "Red" | Blue -> "Blue")
  | Stalemate -> print_endline "Stalemate"

let rec play_game (game_state : Game_state.t) =
  print_board game_state;
  print_game_info game_state;
  
  match game_state.decision with
  | Winner _ | Stalemate -> 
    print_endline "Game Over!"
  | In_progress _ ->
    print_endline "Enter your move (row col):";
    let input = In_channel.input_line In_channel.stdin in
    match input with
    | None -> print_endline "Invalid input"
    | Some line ->
      let parts = String.split line ~on:' ' in
      match parts with
      | [row_str; col_str] ->
        (match Int.of_string row_str, Int.of_string col_str with
         | Ok row, Ok col ->
           (match Game_state.make_move game_state { row; column = col } with
            | Ok new_state -> play_game new_state
            | Error _ -> 
              print_endline "Invalid move! Try again.";
              play_game game_state)
         | _ -> 
           print_endline "Invalid numbers! Try again.";
           play_game game_state)
      | _ -> 
        print_endline "Invalid format! Use: row col";
        play_game game_state

let () =
  print_endline "Welcome to Pixel Game!";
  print_endline "======================";
  let initial_state =
    Game_state.create ~winning_sequence_length:4 ~rows:8 ~columns:8
    |> Result.ok
    |> Option.value_exn
  in
  play_game initial_state
