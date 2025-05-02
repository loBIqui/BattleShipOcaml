#use "inter.ml" ;;
#use "CPgraphics.ml" ;;

(**
essai pour la doc pour le type t_param

@author Killian LAPLAUD *)
type t_params = {margin : int ; cell_size : int ; message_size : int ; grid_size : int ; ship_sizes : (string*int) list } ;;

type t_cell = EMPTY | SHIP | HIT | MISS ;;

type t_direction = HORIZONTAL | VERTICAL ;;

type t_grid = t_cell_position array array ;;

type t_cell_position = {
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  status : t_cell ref
} ;;

type t_ship = {
  name : string;
  positions : (int * int) 
};;

(**

@author Killian LAPLAUD *)
let init_params () =
  {margin = 30 ;
  cell_size = 15 ;
  message_size = 60 ;
  grid_size = 10 ;
  ship_sizes = [("Porte-avions", 5); ("Croiseur", 4); ("Contre-torpilleur", 3); ("Sous-marin", 3); ("Torpilleur", 2)]} 
;;



(**
@author Tibo PATISSOU*)
let cell_make (bl_x : int) (bl_y : int) (p_init_params : t_params) : t_cell_position =
  {
    x1 = bl_x;
    y1 = bl_y;
    x2 = bl_x + p_init_params.cell_size;
    y2 = bl_y + p_init_params.cell_size;
    status = ref EMPTY;
  }
;;

(**
@author Tibo PATISSOU*)
let make_blank_grid (p_init_params : t_params) : t_grid =
  Array.init p_init_params.grid_size (fun _ ->
    Array.init p_init_params.grid_size (fun _ ->
      { x1 = 0; y1 = 0; x2 = 0; y2 = 0; status = ref Empty }
    )
  )
;;

(**
@author Tibo PATISSOU*)
let edit_grid (p_blank_grid : t_grid) (p_init_params : t_params) : unit =
  for i = 0 to p_init_params.grid_size - 1 do
    for j = 0 to p_init_params.grid_size - 1 do
      p_blank_grid.(i).(j).x1 <- p_init_params.margin + i * p_init_params.cell_size;
      p_blank_grid.(i).(j).x2 <- p_blank_grid.(i).(j).x1 + p_init_params.cell_size;
      p_blank_grid.(i).(j).y1 <- p_init_params.margin + j * p_init_params.cell_size;
      p_blank_grid.(i).(j).y2 <- p_blank_grid.(i).(j).y1 + p_init_params.cell_size;
    done;
  done;
;;

(**
@author Tibo PATISSOU
let new_display_empty_grid (p_full_grid,p_cell,p_init_params: t_grid *t_cell *t_params) : unit =
  (**pas finis*)
  for i = 0 to p_init_params.grid_size - 1 do
    for j = 0 to p_init_params.grid_size - 1 do
      draw_rectangle(p_full_grid.(i).(j).x1,p_full_grid.(i).(j).x2,p_full_grid.(i).(j).y1,p_full_grid.(i).(j).y2)
*)


(**
@author Tibo PATISSOU*)
let cell_to_pixel (p_cell : t_cell_position) : (int * int) =
  let x = p_cell.x1 in
  let y = p_cell.y1 in
  (x, y)
;;

let pixel_to_cell_x (p_coords, p_grid : t_coords*t_grid) : int =
  let l_find : int ref = ref 0 in
  (
  for i = 0 to Array.length(p_grid.(0)) -1 do
    if t_coords.x >= p_grid.(0).(i).x1 && t_coords.x <= p_grid.(0).(i).x2 then
      l_find := i
    else l_find := !l_find ;
  !l_find
  )
  ;;
let pixel_to_cell_y (p_coords, p_grid : t_coords*t_grid) : int =
  let l_find : int ref = ref 0 in
  (
  for i = 0 to Array.length(p_grid) -1 do
    if t_coords.y >= p_grid.(i).y1 && t_coords.y <= p_grid.(i).y2 then
      l_find := i
    else l_find := !l_find ;
  !l_find
  )
  ;;

let color_cell (p_color, p_cell : t_color*t_cell) : unit =
  set_color(p_color) ;;
  fill_rect (p_cell.x1,p_cell.y1,p_cell.x2,p_cell.y2)
;;

(**

@author Axel Ortiz *)
let display_empty_grids () : unit = 
  let params = init_params () in
  let grid_size = params.grid_size in
  let cell_size = params.cell_size in
  let margin = params.margin in
  let grid_pixel_size = grid_size * cell_size in
  let total_width = (2 * grid_pixel_size) + (3 * margin) in
  let total_height = grid_pixel_size + (2 * margin) + 40 in  (* Ajout d'espace pour le texte *)

  (** Ouvrir la fenêtre graphique **)
  open_graph (total_width, total_height);
  clear_graph ();

  (** Dessiner le texte "Ordinateur" au-dessus de la première grille **)
  moveto (margin + (grid_pixel_size / 2) - 30, total_height - margin -15);
  draw_string "Ordinateur";

  (** Dessiner le texte "Joueur" au-dessus de la seconde grille **)
  moveto (2 * margin + grid_pixel_size + (grid_pixel_size / 2) - 30, total_height - margin - 15);
  draw_string "Joueur";

  (** Definiton de la grille **)
  let draw_grid x_offset y_offset =
    (** Dessiner les lettres en haut **)
    for i = 0 to grid_size - 1 do
      let x = x_offset + (i * cell_size) + (cell_size / 2) - 5 in
      let y = y_offset + grid_pixel_size + 5 in
      moveto (x, y);
      draw_string (String.make 1 (char_of_int (65 + i)));
    done;

    (** Dessiner les chiffres sur le côté gauche dans l'ordre inverse **)
    for j = 0 to grid_size - 1 do
      let x = x_offset - 15 in
      let y = y_offset + ((grid_size - 1 - j) * cell_size) + (cell_size / 2) - 5 in
      moveto (x, y);
      draw_string (string_of_int (j + 1));
    done;

    (** Dessiner les lignes de la grille **)
    for i = 0 to grid_size do
      moveto (x_offset, y_offset + (i * cell_size));
      lineto (x_offset + grid_pixel_size, y_offset + (i * cell_size));
      moveto (x_offset + (i * cell_size), y_offset);
      lineto (x_offset + (i * cell_size), y_offset + grid_pixel_size);
    done
  in

  (** Dessiner la grille du joueur 1 **)
  draw_grid margin margin;

  (** Dessiner la grille du joueur 2 **)
  draw_grid (2 * margin + grid_pixel_size) margin;
;;

(**
@author Killian LAPLAUD*)
let random_direction () : t_direction =
  if Random.bool() then HORIZONTAL 
  else VERTICAL 

;;


(**
@author Killian LAPLAUD
@author Tibo*)
let random_case () =
  let params = init_params () in
  (Random.int(params.grid_size) , Random.int(params.grid_size))
  
;;

(**
@author Killian LAPLAUD
*)
let read_mouse () : (string * (int * int)) =
  let params = init_params () in
  let grid_size = params.grid_size in
  let cell_size = params.cell_size in
  let margin = params.margin in
  let grid_pixel_size = grid_size * cell_size in

  (* Attendre un clic de souris *)
  let (x, y) = 
    if button_down() then 
      mouse_pos()
    else (
      wait_button_down();  (* Attendre un clic *)
      mouse_pos()          (* Retourner la position après le clic *)
    )
  in

  (* Vérifier si le clic est dans la grille de l'ordinateur *)
  if x >= margin && x < margin + grid_pixel_size &&
     y >= margin && y < margin + grid_pixel_size then
    let xx = (x - margin) / cell_size in
    let yy = grid_size - 1 - ((y - margin) / cell_size) in
    ("Ordinateur", (xx, yy))

  (* Vérifier si le clic est dans la grille du joueur *)
  else if x >= (2 * margin + grid_pixel_size) && x < (2 * margin + 2 * grid_pixel_size) &&
          y >= margin && y < margin + grid_pixel_size then
    let xx = (x - (2 * margin + grid_pixel_size)) / cell_size in
    let yy = grid_size - 1 - ((y - margin) / cell_size) in
    ("Joueur", (xx, yy))

  (* Si le clic est en dehors des grilles *)
  else
    ("Aucune", ( -1 , -1))
;;


(**

@author Killian LAPLAUD *)
let battleship_game () : unit =
  set_window_title "Bataille Navale";
  display_empty_grids() ;;


  clear_graph() ;;

  
  battleship_game() ;;


    