(* hw1.mli - Pixel Game Types and Values *)

type player_kind =
  | Red
  | Blue

type cell_position =
  { row : int
  ; column : int
  }

type decision =
  | In_progress of { whose_turn : player_kind }
  | Winner of player_kind
  | Stalemate

type game_state =
  { board : (cell_position * player_kind) list
  ; rows : int
  ; columns : int
  ; winning_sequence_length : int
  ; decision : decision
  }

type move = cell_position

(* Initial state with Red at (3,3) and Blue at (4,4) *)
val initial_state : game_state

(* Move at position (3,4) *)
val move_at_3x4 : move

(* State after Red moves to (3,4) *)
val state_after_move_at_3x4 : game_state

(* State before terminal state - Red wins with 4 in a row *)
val before_terminal_state : game_state

(* Move that leads to terminal state *)
val move_to_terminal_state : move

(* Terminal state with Red as winner *)
val terminal_state : game_state
