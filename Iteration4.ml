#use "inter.ml";;
#use "CPgraphics.ml";;


(* ---------------------------- *)
(* Interaction utilisateur     *)
(* ---------------------------- *)

let coord_to_cell (x, y) (x0, y0) : int * int =
  ((x - x0) / cell_size, (y - y0) / cell_size)

let cell_to_coord (x, y) (x0, y0) : int * int =
  (x0 + x * cell_size, y0 + y * cell_size)

let preview_ship (x0, y0) (x, y) (len : int) (dir : orientation) =
  set_color cyan;
  for i = 0 to len - 1 do
    let xi = if dir = Horizontal then x + i else x in
    let yi = if dir = Vertical then y + i else y in
    fill_rect (
      x0 + xi * cell_size + 1,
      y0 + yi * cell_size + 1,
      cell_size - 2,
      cell_size - 2
    )
  done
;;

let rec place_ship_interactively (grid : grid) (x0, y0) (length : int) : unit =
  let placing = ref true in
  let selected = ref (0, 0) in
  let orientation = ref Horizontal in

  while !placing do
    display_grids_with_ships grid (init_grid ());
    let (x, y) = !selected in
    preview_ship x0 y0 x y length !orientation;
    synchronize ();
    let status = wait_next_event [Button_down; Key_pressed] in

    if status.button then (
      let mx, my = coord_to_cell (status.mouse_x, status.mouse_y) (x0, y0) in
      if mx >= 0 && mx < grid_size && my >= 0 && my < grid_size then
        selected := (mx, my)
    );

    if status.keypressed then (
      match status.key with
      | "h" | "H" -> orientation := Horizontal
      | "v" | "V" -> orientation := Vertical
      | "\r" ->
        let (x, y) = !selected in
        if place_ship grid x y length !orientation then placing := false
      | _ -> ()
    )
  done
;;

let place_all_ships_interactively (grid : grid) (x0, y0) : unit =
  let ships = [5; 4; 3; 3; 2] in
  List.iter (fun len ->
    place_ship_interactively grid (x0, y0) len
  ) ships
;;

(* -------------------------- *)
(* Fonction principale mise à jour *)
(* -------------------------- *)

let battleship_game () : unit =
  init_params ();
  let player1_grid = init_grid () in
  let player2_grid = init_grid () in

  place_all_ships_interactively player1_grid (margin, margin);
  display_grids_with_ships player1_grid player2_grid;
  ignore (read_key ());
  close_graph ()
;;

(* Lancement *)
let () = battleship_game ()