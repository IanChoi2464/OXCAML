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
  
  (* Simple HTML representation *)
  let board_html = 
    let rows = List.init game_state.rows ~f:(fun row ->
      let cells = List.init game_state.columns ~f:(fun col ->
        let cell_value = Map.find game_state.board { row; column = col } in
        let content = 
          match cell_value with
          | Some Red -> "🔴"
          | Some Blue -> "🔵" 
          | None -> "⚪"
        in
        Printf.sprintf "<td>%s</td>" content
      ) |> String.concat ~sep:""
      in
      Printf.sprintf "<tr>%s</tr>" cells
    ) |> String.concat ~sep:""
  in
  
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
    ; Bonsai.Vdom.Node.div
        ~attrs:[ Bonsai.Vdom.Attr.class_ "game-board" ]
        [ Bonsai.Vdom.Node.inner_html
            ~tag:"table"
            ~this_html_is_sanitized_and_is_totally_safe_trust_me:
              (Printf.sprintf "<tbody>%s</tbody>" board_html)
            ()
        ]
    ; Bonsai.Vdom.Node.p [ Bonsai.Vdom.Node.text game_info ]
    ]

let () = Bonsai.Start.start app