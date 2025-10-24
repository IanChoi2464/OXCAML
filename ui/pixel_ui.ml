open! Core
open Pixel_logic_library
open Pixel_game
open! Bonsai
open! Bonsai.Let_syntax

(* Pixel 게임 초기 상태 생성 *)
let create_initial_pixel_game () =
  let initial_board = 
    Core.Map.empty (module Cell_position)
    |> Core.Map.set ~key:(Cell_position.create ~row:3 ~column:3) ~data:Player.Red
    |> Core.Map.set ~key:(Cell_position.create ~row:4 ~column:4) ~data:Player.Blue
  in
  { Game_state.board = initial_board
  ; rows = 8
  ; columns = 8
  ; winning_sequence_length = 4
  ; decision = Decision.In_progress { whose_turn = Player.Red }
  ; last_move = None
  }

(* Pixel 게임 상태 타입 - 슬라이더 제약 추가 *)
module Pixel_game_state = struct
  type t = {
    game_state : Game_state.t;
    slider_x : int;
    slider_y : int;
    slider_moved : bool; (* 이 턴에 슬라이더를 움직였는지 *)
  }

  let create () = {
    game_state = create_initial_pixel_game ();
    slider_x = 3;
    slider_y = 3;
    slider_moved = false;
  }
end

(* 슬라이더 컴포넌트 - 제약 로직 추가 *)
let slider_component ~value ~on_change ~min_val ~max_val ~label ~disabled =
  let%arr value = value
  and on_change = on_change
  and disabled = disabled in
  Bonsai.div
    ~attrs:[ Bonsai.class_ "slider-container" ]
    [ Bonsai.label
        ~attrs:[ Bonsai.class_ "slider-label" ]
        [ Bonsai.text label ]
    ; Bonsai.input
        ~attrs:
          [ Bonsai.type_ "range"
          ; Bonsai.min (Int.to_string min_val)
          ; Bonsai.max (Int.to_string max_val)
          ; Bonsai.value (Int.to_string value)
          ; Bonsai.on_input (fun _ev value_str ->
              if not disabled then
                let value = Int.of_string value_str in
                on_change value)
          ; Bonsai.class_ "slider"
          ; Bonsai.disabled disabled
          ]
        ()
    ; Bonsai.span
        ~attrs:[ Bonsai.class_ "slider-value" ]
        [ Bonsai.text (Int.to_string value) ]
    ]
;;

(* 돌 렌더링 *)
let render_stone ~player =
  let color = match player with
  | Player.Red -> "red"
  | Player.Blue -> "blue"
  in
  Bonsai.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ Bonsai.Attr.create "viewBox" "0 0 100 100" ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      (Printf.sprintf "<circle cx='50' cy='50' r='40' fill='%s' stroke='black' stroke-width='2' />" color)
    ()
;;

(* 게임 보드 렌더링 - 슬라이더 제약 반영 *)
let pixel_board ~pixel_state ~set_pixel_state =
  let render_cell ~row ~column =
    let cell_pos = Cell_position.create ~row ~column in
    let cell_value = Core.Map.find pixel_state.game_state.board cell_pos in
    let is_corner = (row = 0 && column = 0) || (row = 0 && column = 7) || 
                    (row = 7 && column = 0) || (row = 7 && column = 7) in
    let is_slider_position = row = pixel_state.slider_y && column = pixel_state.slider_x in
    
    let stone, clickable_attr =
      match cell_value with
      | Some player -> render_stone ~player, Bonsai.Attr.empty
      | None when is_corner -> Bonsai.Node.none, Bonsai.Attr.empty
      | None when Decision.is_game_over pixel_state.game_state.decision -> Bonsai.Node.none, Bonsai.Attr.empty
      | None when not is_slider_position -> Bonsai.Node.none, Bonsai.Attr.empty
      | None -> (* 슬라이더 위치이고 비어있는 칸 *)
        let attr = Bonsai.Attr.(
          class_ "clickable-cell"
          @ on_click (fun _ ->
              match Game_state.make_move pixel_state.game_state cell_pos with
              | Ok new_game_state -> 
                set_pixel_state { 
                  pixel_state with 
                  game_state = new_game_state;
                  slider_moved = false;
                }
              | Error _ -> ())
        ) in
        Bonsai.Node.none, attr
    in

    let cell_attrs = [
      Bonsai.Attr.class_ "board-cell";
      Bonsai.Attr.style Css_gen.(
        left (`Px (Int.to_float column * 50.0))
        @> top (`Px (Int.to_float row * 50.0))
        @> width (`Px 50.0)
        @> height (`Px 50.0)
        @> if is_corner then background_color (`Name "lightgray") 
           else if is_slider_position then background_color (`Name "yellow")
           else background_color (`Name "lightgreen")
      );
      clickable_attr
    ] in

    Bonsai.Node.div
      ~attrs:cell_attrs
      [ stone ]
  in

  Bonsai.Node.div
    ~attrs:[ Bonsai.Attr.class_ "pixel-board" ]
    (List.init 8 ~f:(fun row ->
       List.init 8 ~f:(fun column -> render_cell ~row ~column)
       |> Bonsai.Node.div ~attrs:[ Bonsai.Attr.class_ "board-row" ]))
;;

(* 메인 앱 컴포넌트 *)
let app (local_, graph) =
  let initial_state = Pixel_game_state.create () in
  let pixel_state, set_pixel_state = Bonsai.state initial_state graph in

  let%arr pixel_state = pixel_state
  and set_pixel_state = set_pixel_state in

  let handle_slider_x_change new_x =
    if not pixel_state.slider_moved then
      set_pixel_state { pixel_state with slider_x = new_x; slider_moved = true }
  in

  let handle_slider_y_change new_y =
    if not pixel_state.slider_moved then
      set_pixel_state { pixel_state with slider_y = new_y; slider_moved = true }
  in

  Bonsai.Node.div
    ~attrs:[ Bonsai.Attr.class_ "pixel-game" ]
    [ Bonsai.Node.h1 [ Bonsai.Node.text "Pixel Game" ]
    ; Bonsai.Node.div
        ~attrs:[ Bonsai.Attr.class_ "game-container" ]
        [ pixel_board ~pixel_state ~set_pixel_state
        ; Bonsai.Node.div
            ~attrs:[ Bonsai.Attr.class_ "sliders" ]
            [ slider_component 
                ~value:pixel_state.slider_x 
                ~on_change:handle_slider_x_change 
                ~min_val:0 
                ~max_val:7 
                ~label:"X Position"
                ~disabled:pixel_state.slider_moved
            ; slider_component 
                ~value:pixel_state.slider_y 
                ~on_change:handle_slider_y_change 
                ~min_val:0 
                ~max_val:7 
                ~label:"Y Position"
                ~disabled:pixel_state.slider_moved
            ]
        ]
    ]
;;

let () = Bonsai.Start.start app
