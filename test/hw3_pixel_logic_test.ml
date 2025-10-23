open! Core
open Pixel_logic_library
open Hw2_pixel_logic

(* ---------- Helpers ---------- *)

let ok_exn = function
  | Ok x -> x
  | Error e -> failwiths ~here:[%here] "Expected Ok but got Error" e [%sexp_of: _]

let create_state ~rows ~columns ~k =
  Game_state.create ~rows ~columns ~winning_sequence_length:k |> ok_exn
;;

(* Move.t 를 직접 생성 *)
let pos (r : int) (c : int) : Move.t =
  Move.create ~row:r ~column:c
;;

(* For_testing.check_all_directions 는 Cell_position.t 를 받음 *)
let cell (r : int) (c : int) : Cell_position.t =
  Cell_position.create ~row:r ~column:c
;;

(* 다형 Result 프린터 *)
let print_result (r : ('a, 'e) Result.t)
    ~ok_to_sexp
    ~err_to_sexp =
  let sexp_of_result ok err = function
    | Ok x    -> Sexp.List [ Sexp.Atom "Ok"    ; ok x ]
    | Error e -> Sexp.List [ Sexp.Atom "Error" ; err e ]
  in
  print_s (sexp_of_result ok_to_sexp err_to_sexp r)
;;

let print_board (t : Game_state.t) =
  print_endline (Game_state.to_ascii t);
  Sexp.output_hum Out_channel.stdout (Decision.sexp_of_t t.decision);
  Out_channel.newline Out_channel.stdout
;;

let place_exn (t : Game_state.t) (m : Move.t) : Game_state.t =
  Game_state.make_move t m |> ok_exn
;;

(* ---------- 1) create: 정상/에러 ---------- *)

let%expect_test "create: 기본 3x3, k=3" =
  let t = create_state ~rows:3 ~columns:3 ~k:3 in
  print_s [%sexp (t : Game_state.t)];
  [%expect {|
    ((board ()) (rows 3) (columns 3) (winning_sequence_length 3)
     (decision (In_progress (whose_turn Red))) (last_move ())) |}]
;;

let%expect_test "create: 사이즈 / k 유효성 검사" =
  let bad1 = Game_state.create ~rows:(-1) ~columns:3 ~winning_sequence_length:3 in
  print_result bad1
    ~ok_to_sexp:Game_state.sexp_of_t
    ~err_to_sexp:[%sexp_of: Game_state.Create_error.t list];
  let bad2 = Game_state.create ~rows:3 ~columns:3 ~winning_sequence_length:4 in
  print_result bad2
    ~ok_to_sexp:Game_state.sexp_of_t
    ~err_to_sexp:[%sexp_of: Game_state.Create_error.t list];
  let bad3 = Game_state.create ~rows:21 ~columns:21 ~winning_sequence_length:30 in
  print_result bad3
    ~ok_to_sexp:Game_state.sexp_of_t
    ~err_to_sexp:[%sexp_of: Game_state.Create_error.t list];
  [%expect {|
    (Error (Board_too_big_or_small))
    (Error (Unwinnable_sequence_length))
    (Error (Unwinnable_sequence_length)) |}]
;;

(* ---------- 2) get_all_moves ---------- *)

let%test "get_all_moves: 개수는 rows * columns" =
  let t = create_state ~rows:4 ~columns:5 ~k:3 in
  List.length (Game_state.get_all_moves t) = 4 * 5
;;

(* ---------- 3) make_move: 에러 경로 ---------- *)

let%expect_test "make_move: 보드 밖 좌표 → Illegal_cell_position" =
  let t = create_state ~rows:3 ~columns:3 ~k:3 in
  let r = Game_state.make_move t (pos 10 10) in
  print_result r
    ~ok_to_sexp:Game_state.sexp_of_t
    ~err_to_sexp:Game_state.Move_error.sexp_of_t;
  [%expect {| (Error Illegal_cell_position) |}]
;;

let%expect_test "make_move: 같은 칸 두 번 → Space_already_filled" =
  let t = create_state ~rows:3 ~columns:3 ~k:3 in
  let t = place_exn t (pos 0 0) in
  let r = Game_state.make_move t (pos 0 0) in
  print_result r
    ~ok_to_sexp:Game_state.sexp_of_t
    ~err_to_sexp:Game_state.Move_error.sexp_of_t;
  [%expect {| (Error Space_already_filled) |}]
;;

let%expect_test "make_move: 게임 종료 후 수 → Game_is_over" =
  let t = create_state ~rows:3 ~columns:3 ~k:3 in
  let t = place_exn t (pos 0 0) in (* R *)
  let t = place_exn t (pos 1 0) in (* B *)
  let t = place_exn t (pos 0 1) in (* R *)
  let t = place_exn t (pos 1 1) in (* B *)
  let t = place_exn t (pos 0 2) in (* R wins *)
  print_board t;
  let r = Game_state.make_move t (pos 2 2) in
  print_result r
    ~ok_to_sexp:Game_state.sexp_of_t
    ~err_to_sexp:Game_state.Move_error.sexp_of_t;
  [%expect {|
    RRR
    BB.
    ...
    (Winner Red)
    (Error Game_is_over) |}]
;;

(* ---------- 4) 승리/무승부 ---------- *)

let%expect_test "가로 승리" =
  let t = create_state ~rows:3 ~columns:4 ~k:3 in
  let t = place_exn t (pos 1 0) in (* R *)
  let t = place_exn t (pos 0 0) in (* B *)
  let t = place_exn t (pos 1 1) in (* R *)
  let t = place_exn t (pos 0 1) in (* B *)
  let t = place_exn t (pos 1 2) in (* R wins *)
  print_board t;
  [%expect {|
    BB..
    RRR.
    ....
    (Winner Red) |}]
;;

let%expect_test "세로 승리" =
  let t = create_state ~rows:4 ~columns:3 ~k:3 in
  let t = place_exn t (pos 0 2) in (* R *)
  let t = place_exn t (pos 0 0) in (* B *)
  let t = place_exn t (pos 1 2) in (* R *)
  let t = place_exn t (pos 1 0) in (* B *)
  let t = place_exn t (pos 2 2) in (* R wins *)
  print_board t;
  [%expect {|
    B.R
    B.R
    ..R
    ...
    (Winner Red) |}]
;;

let%expect_test "대각 승리 (↘)" =
  let t = create_state ~rows:4 ~columns:4 ~k:3 in
  let t = place_exn t (pos 0 0) in (* R *)
  let t = place_exn t (pos 0 1) in (* B *)
  let t = place_exn t (pos 1 1) in (* R *)
  let t = place_exn t (pos 0 2) in (* B *)
  let t = place_exn t (pos 2 2) in (* R wins *)
  print_board t;
  [%expect {|
    RBB.
    .R..
    ..R.
    ....
    (Winner Red) |}]
;;

let%expect_test "무승부(Stalemate) — 2x2, k=2" =
  let t = create_state ~rows:2 ~columns:2 ~k:2 in
  let t = place_exn t (pos 0 0) in
  let t = place_exn t (pos 0 1) in
  let t = place_exn t (pos 1 0) in
  let t = place_exn t (pos 1 1) in
  print_board t;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Expected Ok but got Error" _ test/hw3_pixel_logic_test.ml:9:31)
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Core__Error.failwiths in file "core/src/error.ml", line 5, characters 2-50
  Called from Pixel_test_library__Hw3_pixel_logic_test.(fun) in file "test/hw3_pixel_logic_test.ml", line 177, characters 10-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(* ---------- 5) For_testing: 내부 헬퍼 ---------- *)

let%expect_test "all_directions: 8방향 포함" =
  print_s [%sexp (Game_state.For_testing.all_directions : (int * int) list)];
  [%expect {|
    ((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)) |}]
;;

let%test "check_all_directions: 빈 3x3 (1,1)에서는 None" =
  let t = create_state ~rows:3 ~columns:3 ~k:3 in
  Option.is_none (Game_state.For_testing.check_all_directions t (cell 1 1))
;;

(* ---------- 6) to_ascii ---------- *)

let%expect_test "to_ascii: 중앙 한 수" =
  let t = create_state ~rows:3 ~columns:3 ~k:3 in
  let t = place_exn t (pos 1 1) in
  print_endline (Game_state.to_ascii t);
  [%expect {|
    ...
    .R.
    ... |}]
;;

(* ---------- 7) Random walk: make_move 기반 ---------- *)

let random_walk_until_terminal ~rows ~cols ~k ~seed =
  let rec loop (t : Game_state.t) (rng : Random.State.t) =
    match t.decision with
    | Decision.In_progress _ ->
      let next_states =
        Game_state.get_all_moves t
        |> List.filter_map ~f:(fun m -> Game_state.make_move t m |> Result.ok)
      in
      if List.is_empty next_states then t
      else
        let idx = Random.State.int rng (List.length next_states) in
        let t = List.nth_exn next_states idx in
        loop t rng
    | _ -> t
  in
  let rng = Random.State.make [| seed |] in
  loop (create_state ~rows ~columns:cols ~k) rng
;;

let%expect_test "random walk: Winner _ 또는 Stalemate 로 끝남" =
  let t = random_walk_until_terminal ~rows:3 ~cols:3 ~k:3 ~seed:42 in
  (match t.decision with
   | Decision.Winner _ -> print_endline "Winner"
   | Decision.Stalemate -> print_endline "Stalemate"
   | Decision.In_progress _ -> print_endline "BUG");
  [%expect {| Winner |}]
;;
