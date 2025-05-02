#use "inter.ml";;
#use "CPgraphics.ml";;

(* -------------------------- *)
(* Tir du joueur             *)
(* -------------------------- *)

let shoot_at (enemy_grid : grid) (x, y : int * int) : bool =
  if x < 0 || x >= grid_size || y < 0 || y >= grid_size then false
  else
    match enemy_grid.(x).(y) with
    | Empty -> enemy_grid.(x).(y) <- Miss; false
    | Ship -> enemy_grid.(x).(y) <- Hit; true
    | _ -> false (* déjà tiré *)
;;

let rec player_turn (enemy_grid : grid) (x0, y0) : unit =
  let rec wait_for_valid_shot () =
    let status = wait_next_event [Button_down] in
    if status.button then
      let (x, y) = coord_to_cell (status.mouse_x, status.mouse_y) (x0, y0) in
      match enemy_grid.(x).(y) with
      | Hit | Miss -> wait_for_valid_shot () (* déjà tiré *)
      | _ ->
        ignore (shoot_at enemy_grid (x, y))
    else wait_for_valid_shot ()
  in
  wait_for_valid_shot ()
;;
