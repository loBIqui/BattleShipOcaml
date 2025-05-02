#use "inter.ml";;
#use "CPgraphics.ml";;

(* Liste des positions initiales pour les bateaux *)
let positions_list =
  [5; 4; 3; 3; 2] (* Taille des bateaux *)

(* Vérifie si un bateau peut être placé *)
let can_place_ship grid x y size orientation =
  try
    for i = 0 to size - 1 do
      let xi = if orientation = "H" then x + i else x in
      let yi = if orientation = "V" then y + i else y in
      if grid.(xi).(yi) <> 0 then raise Exit
    done;
    true
  with Exit | Invalid_argument _ -> false

(* Place un bateau sur la grille *)
let place_ship_on_grid grid x y size orientation =
  for i = 0 to size - 1 do
    let xi = if orientation = "H" then x + i else x in
    let yi = if orientation = "V" then y + i else y in
    grid.(xi).(yi) <- size
  done

(* Placement automatique des bateaux *)
let auto_placing_ships () =
  let grid = Array.make_matrix 10 10 0 in
  let orientations = ["H"; "V"] in
  List.iter (fun size ->
    let placed = ref false in
    while not !placed do
      let x = Random.int 10 in
      let y = Random.int 10 in
      let orientation = List.nth orientations (Random.int 2) in
      if can_place_ship grid x y size orientation then (
        place_ship_on_grid grid x y size orientation;
        placed := true
      )
    done
  ) positions_list;
  grid

(* Colorie une cellule *)
let color_cell x y color =
  let cell = 30 in
  set_color color;
  fill_rect (100 + x * cell, 100 + y * cell, cell, cell);
  set_color black;
  draw_rect (100 + x * cell, 100 + y * cell, cell, cell)

(* Convertit une cellule logique en coordonnées pixels *)
let cell_to_pixel x y =
  let cell = 30 in
  (100 + x * cell, 100 + y * cell)

(* Affiche la grille avec les bateaux *)
let display_grid grid =
  for x = 0 to 9 do
    for y = 0 to 9 do
      if grid.(x).(y) <> 0 then
        color_cell x y blue
    done
  done
