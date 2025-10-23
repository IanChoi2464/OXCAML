(* hw2_pixel_logic.ml *)
open! Core

(* ---------------- Player ---------------- *)

module Player_kind = struct
  type t =
    | Red
    | Blue
  [@@deriving sexp, compare, equal]

  let opposite = function
    | Red -> Blue
    | Blue -> Red
  ;;
end

(* ---------------- Cell / Move ---------------- *)

module Cell_position = struct
  module T = struct
    type t =
      { row : int
      ; column : int
      }
    [@@deriving sexp, compare]
  end

  include T
  (* [Comparable.Make]을 include 하면 [Cell_position.Map.t] / [Cell_position.Set.t]가 함께 생김 *)
  include Comparable.Make (T)

  let create ~row ~column = { row; column }
end

module Move = Cell_position

(* ---------------- Decision ---------------- *)

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
  [@@deriving sexp, compare, equal]

  let is_game_over = function
    | Stalemate | Winner _ -> true
    | In_progress _ -> false
  ;;
end

(* ---------------- Game State ---------------- *)

module Game_state = struct
  type t =
    { board : Player_kind.t Cell_position.Map.t
    ; rows : int
    ; columns : int
    ; winning_sequence_length : int
    ; decision : Decision.t
    ; last_move : Move.t option
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t =
      | Board_too_big_or_small
      | Unwinnable_sequence_length
    [@@deriving sexp, compare]
  end

  let create ~rows ~columns ~winning_sequence_length
    : (t, Create_error.t list) Result.t
    =
    let size_ok =
      rows > 0 && columns > 0 && rows <= 50 && columns <= 50
    in
    let seq_ok =
      winning_sequence_length > 0
      && (winning_sequence_length <= rows || winning_sequence_length <= columns)
    in
    match size_ok, seq_ok with
    | true, true ->
      Ok
        { board = Cell_position.Map.empty
        ; rows
        ; columns
        ; winning_sequence_length
        ; decision = In_progress { whose_turn = Player_kind.Red }
        ; last_move = None
        }
    | _ ->
      Error
        ( (if size_ok then [] else [ Create_error.Board_too_big_or_small ])
        @ (if seq_ok then [] else [ Create_error.Unwinnable_sequence_length ]) )
  ;;

  (* ----- helpers ----- *)

  let is_legal_cell_position { rows; columns; _ } (pos : Cell_position.t) =
    0 <= pos.row && pos.row < rows && 0 <= pos.column && pos.column < columns
  ;;

  let value_if_all_the_same (xs : Player_kind.t list) : Player_kind.t option =
    match xs with
    | [] -> None
    | hd :: tl ->
      if List.for_all tl ~f:(Player_kind.equal hd) then Some hd else None
  ;;

  (* 8 방향(delta_row, delta_col) *)
  let deltas = List.init 3 ~f:(fun i -> i - 1)

  let all_directions =
    List.cartesian_product deltas deltas
    |> List.filter ~f:(fun (dr, dc) -> not (dr = 0 && dc = 0))
  ;;

  (* 시작점에서 한 방향으로 k칸 모아보기 *)
  let check_direction_starting_from
        ~vertical_delta
        ~horizontal_delta
        (t : t)
        (start : Cell_position.t)
    : Player_kind.t option
    =
    let cells =
      List.range 0 t.winning_sequence_length
      |> List.filter_map ~f:(fun i ->
        let r = start.row + (i * vertical_delta) in
        let c = start.column + (i * horizontal_delta) in
        Map.find t.board { Cell_position.row = r; column = c })
    in
    if List.length cells >= t.winning_sequence_length
    then value_if_all_the_same cells
    else None
  ;;

  (* 한 점에서 8방향 체크 *)
  let check_all_directions (t : t) (pos : Cell_position.t) : Player_kind.t option =
    all_directions
    |> List.filter_map ~f:(fun (dr, dc) ->
      check_direction_starting_from ~vertical_delta:dr ~horizontal_delta:dc t pos)
    |> value_if_all_the_same
  ;;

  (* 보드 전체에서 승리여부 탐색 *)
  let check_winner (t : t) : Player_kind.t option =
    Map.filter_keys t.board ~f:(fun pos ->
      Option.is_some (check_all_directions t pos))
    |> Map.min_elt
    |> Option.map ~f:snd
  ;;

  module Move_error = struct
    type t =
      | Game_is_over
      | Space_already_filled
      | Illegal_cell_position
    [@@deriving sexp, compare]
  end

  let current_turn (t : t) : Player_kind.t option =
    match t.decision with
    | In_progress { whose_turn } -> Some whose_turn
    | _ -> None
  ;;

  let get_all_moves (t : t) : Move.t list =
    List.cartesian_product (List.range 0 t.rows) (List.range 0 t.columns)
    |> List.map ~f:(fun (row, column) -> Cell_position.create ~row ~column)
  ;;

  let make_move (t : t) (pos : Move.t)
    : (t, Move_error.t) Result.t
    =
    match t.decision with
    | _ when not (is_legal_cell_position t pos) ->
      Error Move_error.Illegal_cell_position
    | Winner _ | Stalemate ->
      Error Move_error.Game_is_over
    | In_progress { whose_turn } ->
      (match Map.find t.board pos with
       | Some _ -> Error Move_error.Space_already_filled
       | None ->
        let board = Map.set t.board ~key:pos ~data:whose_turn in
        let decision =
        match check_winner { t with board } with
        | Some winner -> Decision.Winner winner
        | None ->
            if Map.length board = t.rows * t.columns
            then Decision.Stalemate
            else Decision.In_progress { whose_turn = Player_kind.opposite whose_turn }

         in
         Ok { t with board; decision; last_move = Some pos })
  ;;

  (* 렌더링/디버깅용 *)
  let to_ascii (t : t) : string =
    let char_of = function
      | Player_kind.Red -> 'R'
      | Player_kind.Blue -> 'B'
    in
    let buf = Buffer.create (t.rows * (t.columns + 1)) in
    for r = 0 to t.rows - 1 do
      for c = 0 to t.columns - 1 do
        match Map.find t.board { row = r; column = c } with
        | None -> Buffer.add_char buf '.'
        | Some p -> Buffer.add_char buf (char_of p)
      done;
      if r < t.rows - 1 then Buffer.add_char buf '\n'
    done;
    Buffer.contents buf
  ;;

  module For_testing = struct
    let all_directions = all_directions
    let check_all_directions = check_all_directions
  end
end
