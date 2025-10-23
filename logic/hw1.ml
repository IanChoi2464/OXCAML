(*========================================
   Pixel (2-player version)
   Base OCaml game state and examples
  ========================================*)

module Player_kind = struct
  type t =
    | Red
    | Blue
end

module Cell_position = struct
  type t =
    { row : int
    ; column : int
    }
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
end

module Slider_axis = struct
  type t =
    | Row
    | Column
end

module Move = struct
  type t =
    { slider : Slider_axis.t
    ; index : int
    }
  (* e.g., { slider = Row; index = 3 } or { slider = Column; index = 5 } *)
end

module Game_state = struct
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

open Player_kind
open Cell_position
open Decision
open Move
open Game_state
open Slider_axis

(*---------------------------------------- Initial empty 7x7 board
  ---------------------------------------*)
(*=
 . . . . . . .
 . . . . . . .
 . . . . . . .
 . . . . . . .
 . . . . . . .
 . . . . . . .
 . . . . . . .
*)
let initial_state : Game_state.t =
  { board = []
  ; rows = 7
  ; columns = 7
  ; row_slider = 0
  ; col_slider = 0
  ; winning_sequence_length = 4
  ; decision = In_progress { whose_turn = Red }
  }
;;

(* First move: move row slider to 2 → intersection (2, 0), place Red *)
let move1 : Move.t = { slider = Row; index = 2 }

let state_after_move1 : Game_state.t =
  { board = [ { row = 2; column = 0 }, Red ]
  ; rows = 7
  ; columns = 7
  ; row_slider = 2
  ; col_slider = 0
  ; winning_sequence_length = 4
  ; decision = In_progress { whose_turn = Blue }
  }
;;

(* Second move: move column slider to 2 → intersection (2, 2), place Blue *)
let move2 : Move.t = { slider = Column; index = 2 }

let state_after_move2 : Game_state.t =
  { board = [ { row = 2; column = 0 }, Red; { row = 2; column = 2 }, Blue ]
  ; rows = 7
  ; columns = 7
  ; row_slider = 2
  ; col_slider = 2
  ; winning_sequence_length = 4
  ; decision = In_progress { whose_turn = Red }
  }
;;

(* Before terminal state: Red has 3 in a row, about to win with (3,3) *)
let before_terminal_state : Game_state.t =
  { board =
      [ { row = 3; column = 0 }, Red
      ; { row = 3; column = 1 }, Red
      ; { row = 3; column = 2 }, Red
      ; { row = 2; column = 2 }, Blue
      ]
  ; rows = 7
  ; columns = 7
  ; row_slider = 3
  ; col_slider = 0
  ; winning_sequence_length = 4
  ; decision = In_progress { whose_turn = Red }
  }
;;

(* Red moves column slider to 3 → intersection (3,3) = win *)
let winning_move : Move.t = { slider = Column; index = 3 }

let terminal_state : Game_state.t =
  { board =
      [ { row = 3; column = 0 }, Red
      ; { row = 3; column = 1 }, Red
      ; { row = 3; column = 2 }, Red
      ; { row = 3; column = 3 }, Red
      ; { row = 2; column = 2 }, Blue
      ]
  ; rows = 7
  ; columns = 7
  ; row_slider = 3
  ; col_slider = 3
  ; winning_sequence_length = 4
  ; decision = Winner Red
  }
;;
