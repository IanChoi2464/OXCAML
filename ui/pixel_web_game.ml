open! Core
open Pixel_logic_library
open Hw2_pixel_logic
open! Bonsai.Let_syntax

let app =
  let initial_state =
    Game_state.create ~winning_sequence_length:4 ~rows:8 ~columns:8
    |> Result.ok
    |> Option.value_exn
  in
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_state)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state in
  
  let game_info = 
    match game_state.decision with
    | In_progress { whose_turn } ->
      Printf.sprintf "Current Player: %s" 
        (match whose_turn with Red -> "Red" | Blue -> "Blue")
    | Winner player ->
      Printf.sprintf "Winner: %s" 
        (match player with Red -> "Red" | Blue -> "Blue")
    | Stalemate -> "Stalemate"
  in
  
  Bonsai.Vdom.Node.div
    [ Bonsai.Vdom.Node.h1 [ Bonsai.Vdom.Node.text "Pixel Game" ]
    ; Bonsai.Vdom.Node.p [ Bonsai.Vdom.Node.text game_info ]
    ; Bonsai.Vdom.Node.p [ Bonsai.Vdom.Node.text "OCaml/Bonsai Implementation" ]
    ]

let () = Bonsai.Start.start app