(* hw1.ml - Pixel Game Types and Values *)

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

(*=
Initial Pixel Game State:
- 8x8 board with corners (0,0), (0,7), (7,0), (7,7) unusable
- Red starts at (3,3), Blue at (4,4)
- Need 4 in a row to win
*)
let initial_state : game_state =
  { board = 
      [ { row = 3; column = 3 }, Red
      ; { row = 4; column = 4 }, Blue
      ]
  ; rows = 8
  ; columns = 8
  ; winning_sequence_length = 4
  ; decision = In_progress { whose_turn = Red }
  }
;;

let move_at_3x4 : move = { row = 3; column = 4 }

(*=
State after Red moves to (3,4):
- Red: (3,3), (3,4)
- Blue: (4,4)
- Blue's turn next
*)
let state_after_move_at_3x4 : game_state =
  { board = 
      [ { row = 3; column = 3 }, Red
      ; { row = 3; column = 4 }, Red
      ; { row = 4; column = 4 }, Blue
      ]
  ; rows = 8
  ; columns = 8
  ; winning_sequence_length = 4
  ; decision = In_progress { whose_turn = Blue }
  }
;;

(*=
Before terminal state - Red has 3 in a row horizontally:
- Red: (3,3), (3,4), (3,5)
- Blue: (4,4), (4,5)
- Red's turn to win
*)
let before_terminal_state : game_state =
  { board = 
      [ { row = 3; column = 3 }, Red
      ; { row = 3; column = 4 }, Red
      ; { row = 3; column = 5 }, Red
      ; { row = 4; column = 4 }, Blue
      ; { row = 4; column = 5 }, Blue
      ]
  ; rows = 8
  ; columns = 8
  ; winning_sequence_length = 4
  ; decision = In_progress { whose_turn = Red }
  }
;;

let move_to_terminal_state : move = { row = 3; column = 6 }

(*=
Terminal state - Red wins with 4 in a row horizontally:
- Red: (3,3), (3,4), (3,5), (3,6) - WINNER!
- Blue: (4,4), (4,5)
*)
let terminal_state : game_state =
  { board = 
      [ { row = 3; column = 3 }, Red
      ; { row = 3; column = 4 }, Red
      ; { row = 3; column = 5 }, Red
      ; { row = 3; column = 6 }, Red
      ; { row = 4; column = 4 }, Blue
      ; { row = 4; column = 5 }, Blue
      ]
  ; rows = 8
  ; columns = 8
  ; winning_sequence_length = 4
  ; decision = Winner Red
  }
;;
