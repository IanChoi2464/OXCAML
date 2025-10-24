open! Core
open Hw2_pixel_logic

let heuristic_value (node : Game_state.t) =
  match node.decision with
  | Stalemate -> 0
  | In_progress _ ->
    (* For Pixel game, we can add a simple heuristic based on:
       - Number of stones in a row (potential for winning)
       - Center control
       - Blocking opponent's threats
    *)
    let board = node.board in
    let red_count = Map.count board ~f:(fun _ player -> Player_kind.equal player Red) in
    let blue_count = Map.count board ~f:(fun _ player -> Player_kind.equal player Blue) in
    (* Simple heuristic: more stones = better position *)
    red_count - blue_count
  | Winner player_kind ->
    (match player_kind with
     | Red -> Int.max_value
     | Blue -> Int.min_value)
;;

let children node ~(sort_by_whose_turn : Player_kind.t) =
  let compare =
    match sort_by_whose_turn with
    | Red -> Int.descending
    | Blue -> Int.ascending
  in
  let moves = Game_state.get_all_moves node in
  List.filter_map moves ~f:(fun move -> Game_state.make_move node move |> Result.ok)
  (* Sorting the children by heuristic values gives the best alpha-beta pruning. *)
  |> List.sort ~compare:(Comparable.lift ~f:heuristic_value compare)
;;

(*=
https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning

function alpha_beta(node, depth, α, β, maximizing_player) is
    if depth == 0 or node is terminal then
        return the heuristic value of node
    if maximizing_player then
        value := −∞
        for each child of node do
            value := max(value, alpha_beta(child, depth − 1, α, β, FALSE))
            if value ≥ β then
                break (* β cutoff *)
            α := max(α, value)
        return value
    else
        value := +∞
        for each child of node do
            value := min(value, alpha_beta(child, depth − 1, α, β, TRUE))
            if value ≤ α then
                break (* α cutoff *)
            β := min(β, value)
        return value


alphabeta(origin, depth, −∞, +∞, TRUE)
*)
let rec alpha_beta (node : Game_state.t) depth alpha beta nodes_explored =
  let nodes_explored = nodes_explored + 1 in
  match node.decision with
  | In_progress { whose_turn } when depth > 0 ->
    (match whose_turn with
     | Red ->
       List.fold_until
         (children node ~sort_by_whose_turn:whose_turn)
         ~init:(Int.min_value, alpha, nodes_explored)
         ~finish:(fun (value, _alpha, nodes_explored) -> value, nodes_explored)
         ~f:(fun (value, alpha, nodes_explored) child ->
           let child_value, nodes_explored = 
             alpha_beta child (depth - 1) alpha beta nodes_explored in
           let value = Int.max value child_value in
           let alpha = Int.max alpha value in
           if value >= beta then Stop (value, nodes_explored) else Continue (value, alpha, nodes_explored))
     | Blue ->
       List.fold_until
         (children node ~sort_by_whose_turn:whose_turn)
         ~init:(Int.max_value, beta, nodes_explored)
         ~finish:(fun (value, _beta, nodes_explored) -> value, nodes_explored)
         ~f:(fun (value, beta, nodes_explored) child ->
           let child_value, nodes_explored = 
             alpha_beta child (depth - 1) alpha beta nodes_explored in
           let value = Int.min value child_value in
           let beta = Int.min beta value in
           if value <= alpha then Stop (value, nodes_explored) else Continue (value, beta, nodes_explored)))
  | _ -> heuristic_value node, nodes_explored
;;

let alpha_beta (node : Game_state.t) ~depth =
  match node.decision with
  | Winner _ | Stalemate -> None
  | In_progress { whose_turn } ->
    let moves = Game_state.get_all_moves node in
    let moves_and_children =
      List.filter_map moves ~f:(fun move ->
        Game_state.make_move node move
        |> Result.ok
        |> Option.map ~f:(fun child -> move, child))
    in
    let moves_and_children_and_values =
      List.map moves_and_children ~f:(fun (move, child) ->
        let value, _ = alpha_beta child (depth - 1) Int.min_value Int.max_value 0 in
        move, child, value)
    in
    let best_move =
      (match whose_turn with
       | Red -> List.max_elt
       | Blue -> List.min_elt)
        moves_and_children_and_values
        ~compare:(fun (_move, _child, v1) (_move, _child, v2) -> Int.compare v1 v2)
      |> Option.map ~f:(fun (move, _child, _value) -> move)
    in
    best_move
;;

module Alpha_beta_search = struct
  type evaluation = int

  type search_result =
    { best_move : Move.t option
    ; evaluation : evaluation
    ; nodes_explored : int
    }

  let search (node : Game_state.t) ~depth =
    match node.decision with
    | Winner _ | Stalemate -> 
      { best_move = None; evaluation = heuristic_value node; nodes_explored = 0 }
    | In_progress { whose_turn } ->
      let moves = Game_state.get_all_moves node in
      let moves_and_children =
        List.filter_map moves ~f:(fun move ->
          Game_state.make_move node move
          |> Result.ok
          |> Option.map ~f:(fun child -> move, child))
      in
      let moves_and_children_and_values =
        List.map moves_and_children ~f:(fun (move, child) ->
          let value, nodes_explored = 
            alpha_beta child (depth - 1) Int.min_value Int.max_value 0 in
          move, child, value, nodes_explored)
      in
      let best_move, best_value, total_nodes =
        (match whose_turn with
         | Red -> 
           let best = List.max_elt moves_and_children_and_values
             ~compare:(fun (_move, _child, v1, _n1) (_move, _child, v2, _n2) -> Int.compare v1 v2) in
           let total = List.sum (module Int) moves_and_children_and_values ~f:(fun (_, _, _, n) -> n) in
           best, Option.value_exn best |> fun (_, _, v, _) -> v, total
         | Blue -> 
           let best = List.min_elt moves_and_children_and_values
             ~compare:(fun (_move, _child, v1, _n1) (_move, _child, v2, _n2) -> Int.compare v1 v2) in
           let total = List.sum (module Int) moves_and_children_and_values ~f:(fun (_, _, _, n) -> n) in
           best, Option.value_exn best |> fun (_, _, v, _) -> v, total)
      in
      { best_move = Option.map best_move ~f:(fun (move, _, _, _) -> move)
      ; evaluation = best_value
      ; nodes_explored = total_nodes
      }

  let evaluate_position = heuristic_value

  module For_testing = struct
    let evaluate_position = heuristic_value
  end
end

module Random_player = struct
  let get_move (state : Game_state.t) =
    match state.decision with
    | Winner _ | Stalemate -> None
    | In_progress _ ->
      let moves = Game_state.get_all_moves state in
      List.random_element moves
end

module Smart_player = struct
  let get_move (state : Game_state.t) =
    match state.decision with
    | Winner _ | Stalemate -> None
    | In_progress _ ->
      (* Use alpha-beta search with depth 3 for reasonable performance *)
      Alpha_beta_search.search state ~depth:3 |> fun result -> result.best_move

  let get_move_with_timeout (state : Game_state.t) timeout_seconds =
    match state.decision with
    | Winner _ | Stalemate -> None
    | In_progress _ ->
      (* Try different depths until timeout *)
      let start_time = Time.now () in
      let rec try_depth depth =
        let elapsed = Time.diff (Time.now ()) start_time |> Time.Span.to_sec in
        if elapsed >= timeout_seconds then
          (* Use previous depth result or random if no previous result *)
          Random_player.get_move state
        else
          let result = Alpha_beta_search.search state ~depth in
          match result.best_move with
          | Some _ -> result.best_move
          | None -> 
            if depth < 6 then try_depth (depth + 1)
            else Random_player.get_move state
      in
      try_depth 1
end

module Game_simulation = struct
  type game_result =
    | Red_wins
    | Blue_wins
    | Stalemate

  let simulate_game red_player blue_player initial_state =
    let rec play_game state =
      match state.decision with
      | Winner Red -> Red_wins
      | Winner Blue -> Blue_wins
      | Stalemate -> Stalemate
      | In_progress { whose_turn } ->
        let move = 
          match whose_turn with
          | Red -> red_player state
          | Blue -> blue_player state
        in
        match move with
        | None -> Stalemate (* No valid moves *)
        | Some move ->
          match Game_state.make_move state move with
          | Ok new_state -> play_game new_state
          | Error _ -> Stalemate (* Invalid move *)
    in
    play_game initial_state

  let run_tournament red_player blue_player num_games =
    let red_wins = ref 0 in
    let blue_wins = ref 0 in
    let stalemates = ref 0 in
    
    for _ = 1 to num_games do
      let initial_state = 
        Game_state.create ~rows:8 ~columns:8 ~winning_sequence_length:4 
        |> Result.ok_or_failwith in
      match simulate_game red_player blue_player initial_state with
      | Red_wins -> incr red_wins
      | Blue_wins -> incr blue_wins
      | Stalemate -> incr stalemates
    done;
    
    (!red_wins, !blue_wins, !stalemates)
end
