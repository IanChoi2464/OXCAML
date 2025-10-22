(** 
  Pixel Game (2-player version)
  Interface (.mli)
  ----------------
  This file defines the public types and values 
  that other modules can use.
*)

(** Player type: only two players exist *)
module Player_kind : sig
  type t = Red | Blue
end

(** A board cell coordinate *)
module Cell_position : sig
  type t =
    { row : int
    ; column : int
    }
end

(** Game decision state *)
module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
end

(** Slider axis: row or column *)
module Slider_axis : sig
  type t = Row | Column
end

(** A move = choosing one slider and setting its index *)
module Move : sig
  type t =
    { slider : Slider_axis.t
    ; index : int
    }
end

(** Main game state *)
module Game_state : sig
  type t =
    { board : (Cell_position.t * Player_kind.t) list
    ; rows : int
    ; columns : int
    ; row_slider : int
    ; col_slider : int
    ; winning_sequence_length : int
    ; decision : Decision.t
    }
end

(** Example predefined game states (for testing or demo) *)
val initial_state : Game_state.t
val move1 : Move.t
val state_after_move1 : Game_state.t
val move2 : Move.t
val state_after_move2 : Game_state.t
val before_terminal_state : Game_state.t
val winning_move : Move.t
val terminal_state : Game_state.t
