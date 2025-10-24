open! Core

(* Pixel 게임 기본 타입들 *)
module Player = struct
  type t = Red | Blue
  [@@deriving sexp, compare, equal]

  let opposite = function
    | Red -> Blue
    | Blue -> Red
  ;;
end

module Cell_position = struct
  module T = struct
    type t = { row : int; column : int }
    [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make (T)
  
  let create ~row ~column = { row; column }
end

module Move = Cell_position

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player.t }
    | Winner of Player.t
    | Stalemate
  [@@deriving sexp, compare, equal]

  let is_game_over = function
    | Stalemate | Winner _ -> true
    | In_progress _ -> false
  ;;
end

module Game_state = struct
  type t = {
    board : Player.t Cell_position.Map.t;
    rows : int;
    columns : int;
    winning_sequence_length : int;
    decision : Decision.t;
    last_move : Move.t option;
  }
  [@@deriving sexp, compare, equal]

  let create ~rows ~columns ~winning_sequence_length =
    { board = Core.Map.empty (module Cell_position)
    ; rows
    ; columns
    ; winning_sequence_length
    ; decision = Decision.In_progress { whose_turn = Player.Red }
    ; last_move = None
    }

  let is_legal_cell_position { rows; columns; _ } (pos : Cell_position.t) =
    0 <= pos.row && pos.row < rows && 0 <= pos.column && pos.column < columns

  let value_if_all_the_same (xs : Player.t list) : Player.t option =
    match xs with
    | [] -> None
    | hd :: tl ->
      if List.for_all tl ~f:(Player.equal hd) then Some hd else None

  let deltas = List.init 3 ~f:(fun i -> i - 1)
  let all_directions =
    List.cartesian_product deltas deltas
    |> List.filter ~f:(fun (dr, dc) -> not (dr = 0 && dc = 0))

  let check_direction_starting_from
        ~vertical_delta
        ~horizontal_delta
        (t : t)
        (start : Cell_position.t)
    : Player.t option
    =
    let cells =
      List.range 0 t.winning_sequence_length
      |> List.filter_map ~f:(fun i ->
        let r = start.row + (i * vertical_delta) in
        let c = start.column + (i * horizontal_delta) in
        Core.Map.find t.board { Cell_position.row = r; column = c })
    in
    if List.length cells >= t.winning_sequence_length
    then value_if_all_the_same cells
    else None

  let check_all_directions (t : t) (pos : Cell_position.t) : Player.t option =
    all_directions
    |> List.filter_map ~f:(fun (dr, dc) ->
      check_direction_starting_from ~vertical_delta:dr ~horizontal_delta:dc t pos)
    |> value_if_all_the_same

  let check_winner (t : t) : Player.t option =
    Core.Map.filter_keys t.board ~f:(fun pos ->
      Option.is_some (check_all_directions t pos))
    |> Core.Map.min_elt
    |> Option.map ~f:snd

  module Move_error = struct
    type t =
      | Game_is_over
      | Space_already_filled
      | Illegal_cell_position
    [@@deriving sexp, compare]
  end

  let current_turn (t : t) : Player.t option =
    match t.decision with
    | In_progress { whose_turn } -> Some whose_turn
    | _ -> None

  let get_all_moves (t : t) : Move.t list =
    List.cartesian_product (List.range 0 t.rows) (List.range 0 t.columns)
    |> List.map ~f:(fun (row, column) -> Cell_position.create ~row ~column)

  let make_move (t : t) (pos : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | _ when not (is_legal_cell_position t pos) ->
      Error Move_error.Illegal_cell_position
    | Winner _ | Stalemate ->
      Error Move_error.Game_is_over
    | In_progress { whose_turn } ->
      (match Core.Map.find t.board pos with
       | Some _ -> Error Move_error.Space_already_filled
       | None ->
        let board = Core.Map.set t.board ~key:pos ~data:whose_turn in
        let decision =
        match check_winner { t with board } with
        | Some winner -> Decision.Winner winner
        | None ->
            if Core.Map.length board = t.rows * t.columns
            then Decision.Stalemate
            else Decision.In_progress { whose_turn = Player.opposite whose_turn }
         in
         Ok { t with board; decision; last_move = Some pos })

  let to_ascii (t : t) : string =
    let char_of = function
      | Player.Red -> 'R'
      | Player.Blue -> 'B'
    in
    let buf = Buffer.create (t.rows * (t.columns + 1)) in
    for r = 0 to t.rows - 1 do
      for c = 0 to t.columns - 1 do
        match Core.Map.find t.board { row = r; column = c } with
        | None -> Buffer.add_char buf '.'
        | Some p -> Buffer.add_char buf (char_of p)
      done;
      if r < t.rows - 1 then Buffer.add_char buf '\n'
    done;
    Buffer.contents buf
end
