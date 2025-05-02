#use "inter.ml";;
#use "cpgraphics.ml";;

(* ------------------- *)
(* Paramètres de base *)
(* ------------------- *)

let grid_size = 10
let cell_size = 40
let margin = 50
let window_width = 2 * (grid_size * cell_size + margin * 2)
let window_height = grid_size * cell_size + margin * 2

(* Couleurs *)
let grid_color = black
let background_color = white
let label_color = blue
let player_color = red
let ship_color = grey

(* --------------------------- *)
(* Initialisation de la fenetre *)
(* --------------------------- *)

let init_params () : unit =
  open_graph (window_width, window_height);
  set_window_title "Bataille Navale"
;;

(* --------------------------- *)
(* Types et logique de données *)
(* --------------------------- *)

type orientation = Horizontal | Vertical

type cell = {
  mutable has_ship : bool;
  mutable is_hit : bool;
}

type grid = cell array array

let init_grid () : grid =
  Array.init grid_size (fun _ ->
    Array.init grid_size (fun _ -> { has_ship = false; is_hit = false }))
;;

let is_valid_position (grid : grid) (x : int) (y : int) (len : int) (dir : orientation) : bool =
  try
    for i = 0 to len - 1 do
      let xi = if dir = Horizontal then x + i else x in
      let yi = if dir = Vertical then y + i else y in
      if xi >= grid_size || yi >= grid_size || grid.(yi).(xi).has_ship then raise Exit
    done;
    true
  with Exit -> false
;;

let place_ship (grid : grid) (x : int) (y : int) (len : int) (dir : orientation) : bool =
  if not (is_valid_position grid x y len dir) then false
  else (
    for i = 0 to len - 1 do
      let xi = if dir = Horizontal then x + i else x in
      let yi = if dir = Vertical then y + i else y in
      grid.(yi).(xi).has_ship <- true
    done;
    true
  )
;;

(* ---------------------------- *)
(* Dessin de la grille de jeu  *)
(* ---------------------------- *)

let draw_grid (x0, y0) : unit =
  set_color grid_color;
  for i = 0 to grid_size do
    (* Lignes horizontales *)
    moveto (x0, y0 + i * cell_size);
    lineto (x0 + grid_size * cell_size, y0 + i * cell_size);
    (* Lignes verticales *)
    moveto (x0 + i * cell_size, y0);
    lineto (x0 + i * cell_size, y0 + grid_size * cell_size);
  done
;;

let draw_labels (x0, y0) : unit =
  set_color label_color;
  for i = 0 to grid_size - 1 do
    let c = Char.chr (Char.code 'A' + i) in
    moveto (x0 + i * cell_size + cell_size / 3, y0 - 20);
    draw_char c
  done;
  for j = 0 to grid_size - 1 do
    moveto (x0 - 20, y0 + j * cell_size + cell_size / 3);
    draw_string (string_of_int (j + 1))
  done
;;

let draw_player_names () : unit =
  set_color player_color;
  set_text_size 20;
  moveto (margin + cell_size * 2, window_height - margin / 2);
  draw_string "Joueur 1";
  moveto (margin + grid_size * cell_size + margin + cell_size * 2, window_height - margin / 2);
  draw_string "Joueur 2"
;;

let draw_ships (grid : grid) (x0, y0) : unit =
  set_color ship_color;
  for y = 0 to grid_size - 1 do
    for x = 0 to grid_size - 1 do
      if grid.(y).(x).has_ship then
        fill_rect (
          x0 + x * cell_size + 1,
          y0 + y * cell_size + 1,
          cell_size - 2,
          cell_size - 2
        )
    done
  done
;;

let display_grids_with_ships (grid1 : grid) (grid2 : grid) : unit =
  clear_graph ();
  let pos1 = (margin, margin) in
  let pos2 = (margin + grid_size * cell_size + margin, margin) in
  draw_grid pos1;
  draw_grid pos2;
  draw_labels pos1;
  draw_labels pos2;
  draw_player_names ();
  draw_ships grid1 pos1;
  (* Tu peux aussi afficher ceux du joueur 2 si besoin *)
;;

(* -------------------------- *)
(* Fonction principale du jeu *)
(* -------------------------- *)

let battleship_game () : unit =
  init_params ();
  let player1_grid = init_grid () in
  let player2_grid = init_grid () in

  (* Placement de 3 bateaux sur la grille du joueur 1 *)
  ignore (place_ship player1_grid 0 0 4 Horizontal);
  ignore (place_ship player1_grid 2 3 3 Vertical);
  ignore (place_ship player1_grid 5 5 2 Horizontal);

  display_grids_with_ships player1_grid player2_grid;
  ignore (read_key ());
  close_graph ()
;;

(* Lancement *)
let () = battleship_game ()
