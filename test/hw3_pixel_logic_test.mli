open Core
open Pixel_logic_library
open Hw2_pixel_logic

val ok_exn : ('a, 'e) Result.t -> 'a

val create_state
  :  rows:int
  -> columns:int
  -> k:int
  -> Game_state.t

(** 편의용: (row, column)에서 바로 Move.t 생성 *)
val pos : int -> int -> Move.t

(** For_testing.check_all_directions 전용 Cell_position 헬퍼 *)
val cell : int -> int -> Cell_position.t

(** 다형 Result 프린터: ok/err 각각의 Sexp 프린터를 인자로 받음 *)
val print_result
  :  ('a, 'e) Result.t
  -> ok_to_sexp:('a -> Core.Sexp.t)
  -> err_to_sexp:('e -> Core.Sexp.t)
  -> unit

(** 보드 ASCII와 현재 decision 출력 *)
val print_board : Game_state.t -> unit

(** 에러시 예외로 터뜨리는 착수 도우미 *)
val place_exn : Game_state.t -> Move.t -> Game_state.t

(** 끝날 때까지 랜덤으로 진행 *)
val random_walk_until_terminal
  :  rows:int
  -> cols:int
  -> k:int
  -> seed:int
  -> Game_state.t
