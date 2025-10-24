open! Core
open Pixel_logic_library
open Hw1
open Hw2_pixel_logic
open! Bonsai.Let_syntax

let lookup_cell (game_state : Game_state.t) ~row ~column =
  Map.find game_state.board { row; column }
;;

let viewbox = Vdom.Attr.create "viewBox" "0 0 100 100"

let red_stone =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ viewbox ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      "<circle cx='50' cy='50' r='35' fill='red' />"
    ()
;;

let blue_stone =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ viewbox ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      "<circle cx='50' cy='50' r='35' fill='blue' />"
    ()
;;

let pixel_game_board_static ~(game_state : Game_state.t) =
  let is_corner_cell ~row ~column =
    (row = 0 && column = 0) ||
    (row = 0 && column = game_state.columns - 1) ||
    (row = game_state.rows - 1 && column = 0) ||
    (row = game_state.rows - 1 && column = game_state.columns - 1)
  in
  let render_cell ~row ~column =
    let cell_value = lookup_cell game_state ~row ~column in
    let is_corner = is_corner_cell ~row ~column in
    let mark =
      match cell_value with
      | Some Red -> red_stone
      | Some Blue -> blue_stone
      | None -> Vdom.Node.none
    in
    let attrs =
      [ Vdom.Attr.class_ "column"
      ; Vdom.Attr.style
          Css_gen.(
            left
              (`Percent
                  (Percent.of_percentage
                     (Int.to_float column *. 100.0 /. Int.to_float game_state.columns)))
            @> width
                 (`Percent
                     (Percent.of_percentage (100.0 /. Int.to_float game_state.columns))))
      ]
    in
    let attrs =
      attrs
      @ if row < game_state.rows - 1 then [ Vdom.Attr.class_ "border_bottom" ] else []
    in
    let attrs =
      attrs
      @
      if column < game_state.columns - 1 then [ Vdom.Attr.class_ "border_right" ] else []
    in
    let attrs =
      attrs
      @ if is_corner then [ Vdom.Attr.class_ "corner_cell" ] else []
    in
    Vdom.Node.div
      ~attrs
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "max_size" ]
          [ mark ]
      ]
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game" ]
    (List.init game_state.rows ~f:(fun row ->
       Vdom.Node.div
         ~attrs:
           [ Vdom.Attr.class_ "row"
           ; Vdom.Attr.style
               Css_gen.(
                 top
                   (`Percent
                       (Percent.of_percentage
                          (Int.to_float row *. 100.0 /. Int.to_float game_state.rows)))
                 @> height
                      (`Percent
                          (Percent.of_percentage (100.0 /. Int.to_float game_state.rows))))
           ]
         (List.init game_state.columns ~f:(fun column -> render_cell ~row ~column))))
;;

let test_state_component ~(game_state : Game_state.t) ~(title : string) =
  let%arr game_state = game_state in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "test-state-container" ]
    [ Vdom.Node.h2 ~attrs:[ Vdom.Attr.class_ "test-title" ] [ Vdom.Node.text title ]
    ; pixel_game_board_static ~game_state
    ; Vdom.Node.p
        ~attrs:[ Vdom.Attr.class_ "game-info" ]
        [ Vdom.Node.text
            (match game_state.decision with
             | In_progress { whose_turn } ->
               Printf.sprintf "Current Player: %s"
                 (match whose_turn with
                  | Red -> "Red"
                  | Blue -> "Blue")
             | Winner player ->
               Printf.sprintf "Winner: %s"
                 (match player with
                  | Red -> "Red"
                  | Blue -> "Blue")
             | Stalemate -> "Stalemate")
        ]
    ]
;;

let app =
  let%sub initial_state = Bonsai.const initial_state in
  let%sub state_after_move = Bonsai.const state_after_move_at_3x4 in
  let%sub before_terminal = Bonsai.const before_terminal_state in
  let%sub terminal_state = Bonsai.const terminal_state in
  
  let%arr initial_state = initial_state
  and state_after_move = state_after_move
  and before_terminal = before_terminal
  and terminal_state = terminal_state in
  
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "test-app" ]
    [ Vdom.Node.h1 ~attrs:[ Vdom.Attr.class_ "main-title" ] [ Vdom.Node.text "Pixel Game - HW1 States Test" ]
    ; test_state_component ~game_state:initial_state ~title:"1. Initial State"
    ; test_state_component ~game_state:state_after_move ~title:"2. State After Move at (3,4)"
    ; test_state_component ~game_state:before_terminal ~title:"3. Before Terminal State"
    ; test_state_component ~game_state:terminal_state ~title:"4. Terminal State (Red Wins)"
    ]
;;

let () = Bonsai.Start.start app
