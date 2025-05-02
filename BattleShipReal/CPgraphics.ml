#use "inter.ml";;
#use "cpgraphics.ml";;

(* ==================== *)
(*     CONSTANTES       *)
(* ==================== *)

let cell_size = 30         (* Taille d'une case en pixels *)
let grid_size = 10         (* Nombre de cases par grille (10x10) *)
let margin_x1 = 100        (* Position X de la première grille *)
let margin_x2 = 500        (* Position X de la deuxième grille *)
let margin_y = 100         (* Position Y de la grille *)

(* ==================== *)
(*     TYPES            *)
(* ==================== *)

type player = Player1 | Player2

(* ==================== *)
(*   INIT GRAPHICS      *)
(* ==================== *)

let init_params () : unit =
  open_graph (800, 600);
  set_window_title "Bataille Navale";
  clear_graph ()

(* ==================== *)
(*     DESSIN GRILLE    *)
(* ==================== *)

let draw_single_grid (x_offset : int) : unit =
  for i = 0 to grid_size do
    let pos = x_offset + i * cell_size in
    moveto (pos, margin_y);
    lineto (pos, margin_y + grid_size * cell_size);
  done;
  for i = 0 to grid_size do
    let pos = margin_y + i * cell_size in
    moveto (x_offset, pos);
    lineto (x_offset + grid_size * cell_size, pos);
  done

let draw_grid () : unit =
  draw_single_grid margin_x1;  (* Grille Joueur 1 *)
  draw_single_grid margin_x2   (* Grille Joueur 2 *)

(* ==================== *)
(*   DESSIN LABELS      *)
(* ==================== *)

let draw_labels () : unit =
  for i = 0 to grid_size - 1 do
    let letter = Char.escaped (char_of_int (int_of_char 'A' + i)) in
    let number = string_of_int (i + 1) in

    (* Colonnes (lettres) *)
    moveto (margin_x1 + 10 + i * cell_size, margin_y - 30);
    draw_string letter;
    moveto (margin_x2 + 10 + i * cell_size, margin_y - 30);
    draw_string letter;

    (* Lignes (chiffres) *)
    moveto (margin_x1 - 30, margin_y + 10 + i * cell_size);
    draw_string number;
    moveto (margin_x2 - 30, margin_y + 10 + i * cell_size);
    draw_string number;
  done

(* ==================== *)
(*   NOMS JOUEURS       *)
(* ==================== *)

let draw_player_names () : unit =
  set_text_size 24;
  moveto (margin_x1 + 50, margin_y + grid_size * cell_size + 40);
  draw_string "Joueur 1";
  moveto (margin_x2 + 50, margin_y + grid_size * cell_size + 40);
  draw_string "Joueur 2"

(* ==================== *)
(*   AFFICHAGE GLOBAL   *)
(* ==================== *)

let display_empty_grids () : unit =
  draw_grid ();
  draw_labels ();
  draw_player_names ()

(* ==================== *)
(*    JEU PRINCIPAL     *)
(* ==================== *)

let battleship_game () : unit =
  init_params ();
  display_empty_grids ()
