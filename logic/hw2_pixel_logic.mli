(* hw2_pixel_logic.mli *)
open! Core

module Player_kind : sig
  type t =
    | Red
    | Blue
  [@@deriving sexp, compare, equal]

  val opposite : t -> t
end

module Cell_position : sig
  module T : sig
    type t =
      { row : int
      ; column : int
      }
    [@@deriving sexp, compare]
  end

  type t = T.t

  include Comparable.S with type t := t

  val create : row:int -> column:int -> t
end

module Move : module type of Cell_position

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
  [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

module Game_state : sig
  type t =
    { board : Player_kind.t Cell_position.Map.t
    ; rows : int
    ; columns : int
    ; winning_sequence_length : int
    ; decision : Decision.t
    ; last_move : Move.t option
    }
  [@@deriving sexp, compare, equal]

  module Create_error : sig
    type t =
      | Board_too_big_or_small
      | Unwinnable_sequence_length
    [@@deriving sexp, compare]
  end

  val create
    :  rows:int
    -> columns:int
    -> winning_sequence_length:int
    -> (t, Create_error.t list) Result.t

  val is_legal_cell_position : t -> Cell_position.t -> bool

  module Move_error : sig
    type t =
      | Game_is_over
      | Space_already_filled
      | Illegal_cell_position
    [@@deriving sexp, compare]
  end

  val current_turn : t -> Player_kind.t option
  val get_all_moves : t -> Move.t list

  val make_move : t -> Move.t -> (t, Move_error.t) Result.t

  (** Debug/preview helper that renders the board as ASCII:
      '.' for empty, 'R' for Red, 'B' for Blue. *)
  val to_ascii : t -> string

  module For_testing : sig
    val all_directions : (int * int) list
    val check_all_directions : t -> Cell_position.t -> Player_kind.t option
  end
end
