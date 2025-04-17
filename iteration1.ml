#use "topfind" ;;   
#require "graphics" ;;  
#require "unix" ;;
#use "CPgraphics.ml"

let display_empty_grids () : unit = 
  let params = init_params () in
  let grid_size = params.grid_size in
  let cell_size = params.cell_size in
  let margin = params.margin in
  let grid_pixel_size = grid_size * cell_size in
  let total_width = (2 * grid_pixel_size) + (3 * margin) in
  let total_height = grid_pixel_size + (2 * margin) in

  (** Ouvrir la fenêtre graphique **)
  open_graph (total_width, total_height);
  set_window_title "Bataille Navale";
  clear_graph ();

  (** Definiton de la grille **)
  let draw_grid x_offset y_offset =
    for i = 0 to grid_size do
      (*** Lignes horizontales puis verticales **)
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

  let test_display_empty_grids () =
    open_graph (800, 600);
    set_window_title "Test - Grilles vides";
    display_empty_grids ();
    wait 5;
    close_graph ();
  ;;