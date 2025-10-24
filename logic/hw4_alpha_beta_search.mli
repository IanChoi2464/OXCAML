open! Core

module Alpha_beta_search : sig
  type evaluation = int

  type search_result =
    { best_move : Hw2_pixel_logic.Move.t option
    ; evaluation : evaluation
    ; nodes_explored : int
    }

  val search
    :  Hw2_pixel_logic.Game_state.t
    -> depth:int
    -> search_result

  val evaluate_position : Hw2_pixel_logic.Game_state.t -> evaluation

  module For_testing : sig
    val evaluate_position : Hw2_pixel_logic.Game_state.t -> evaluation
  end
end

module Random_player : sig
  val get_move : Hw2_pixel_logic.Game_state.t -> Hw2_pixel_logic.Move.t option
end

module Smart_player : sig
  val get_move : Hw2_pixel_logic.Game_state.t -> Hw2_pixel_logic.Move.t option
  val get_move_with_timeout : Hw2_pixel_logic.Game_state.t -> float -> Hw2_pixel_logic.Move.t option
end

module Game_simulation : sig
  type game_result =
    | Red_wins
    | Blue_wins
    | Stalemate

  val simulate_game
    :  (Hw2_pixel_logic.Game_state.t -> Hw2_pixel_logic.Move.t option)
    -> (Hw2_pixel_logic.Game_state.t -> Hw2_pixel_logic.Move.t option)
    -> Hw2_pixel_logic.Game_state.t
    -> game_result

  val run_tournament
    :  (Hw2_pixel_logic.Game_state.t -> Hw2_pixel_logic.Move.t option)
    -> (Hw2_pixel_logic.Game_state.t -> Hw2_pixel_logic.Move.t option)
    -> int
    -> (int * int * int) (* Red wins, Blue wins, Stalemates *)
end
