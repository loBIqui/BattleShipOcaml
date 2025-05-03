#use "inter.ml";;
#use "CPgraphics.ml";;

(* Initialisation des paramètres du jeu *)
let grid_size = 10;;
let cell_size = 40;;
let margin = 50;;
let window_width = 2 * (grid_size * cell_size + margin) + margin;;
let window_height = grid_size * cell_size + 2 * margin;;

let init_params () =
  Graphics.open_graph (" " ^ string_of_int window_width ^ "x" ^ string_of_int window_height);
  Graphics.set_window_title "Bataille Navale";;

(* Dessine la grille d'un joueur à une position donnée *)
let draw_grid_at x_offset y_offset =
  for i = 0 to grid_size do
    let x = x_offset + i * cell_size in
    Graphics.moveto x y_offset;
    Graphics.lineto x (y_offset + grid_size * cell_size)
  done;
  for j = 0 to grid_size do
    let y = y_offset + j * cell_size in
    Graphics.moveto x_offset y;
    Graphics.lineto (x_offset + grid_size * cell_size) y
  done;;

let draw_grid () =
  let left_grid_x = margin in
  let right_grid_x = margin + grid_size * cell_size + margin in
  let grid_y = margin in
  draw_grid_at left_grid_x grid_y;
  draw_grid_at right_grid_x grid_y;;

(* Étiquettes des lignes et colonnes *)
let draw_labels () =
  let draw_label x y text =
    Graphics.moveto x y;
    Graphics.draw_string text
  in
  for i = 0 to grid_size - 1 do
    let label = String.make 1 (Char.chr (65 + i)) in (* A, B, C... *)
    let x = margin + i * cell_size + cell_size / 2 in
    let y = window_height - margin / 2 in
    draw_label x y label
  done;
  for j = 0 to grid_size - 1 do
    let label = string_of_int (j + 1) in
    let x = margin / 2 in
    let y = margin + j * cell_size + cell_size / 2 in
    draw_label x y label
  done;;

(* Noms des joueurs *)
let draw_player_names () =
  let p1_x = margin + (grid_size * cell_size) / 2 in
  let p2_x = margin * 2 + grid_size * cell_size + (grid_size * cell_size) / 2 in
  let y = window_height - margin / 2 in
  Graphics.moveto (p1_x - 30) y;
  Graphics.draw_string "Joueur";
  Graphics.moveto (p2_x - 30) y;
  Graphics.draw_string "Ordinateur";;

let display_empty_grids () =
  draw_grid (); draw_labels (); draw_player_names ();;

let battleship_game () =
  init_params ();
  display_empty_grids ();
