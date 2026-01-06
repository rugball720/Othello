type othello = int array array * int;;

(*Crée l'état initial du jeu*)
let init_jeu () =
  let plateau = Array.make_matrix 8 8 0 in
  plateau.(3).(3) <- 2;
  plateau.(4).(3) <- 1;
  plateau.(3).(4) <- 1;
  plateau.(4).(4) <- 2;
  plateau, 1;;

let copier_plateau plateau =
    let copie = Array.copy plateau in
    for i = 0 to Array.length plateau - 1 do
        copie.(i) <- Array.copy plateau.(i)
    done;
    copie;;

(*Permet de savoir si un coup est valide*)
let coup_est_possible (plateau, _) (x, y) =
  plateau.(x).(y) = 0 (*Case vide*)
  &&
  (*Une case adjacente non vide*)
  (
    (*gauche*)
    (x > 0 && plateau.(x-1).(y) <> 0)
    ||
    (*droite*)
    (x < 7 && plateau.(x+1).(y) <> 0)
    || 
    (*bas*)
    (y > 0 && plateau.(x).(y-1) <> 0)
    ||
    (*haut*)
    (y < 7 && plateau.(x).(y+1) <> 0)
  );;

(*Renvoie le nombre de jetons du joueur 1 moins le nombre de jetons du joueur 2*)
let scorer (plateau, _) =
  let nb_noirs = ref 0 in
  let nb_blancs = ref 0 in
  for x = 0 to 7 do
    for y = 0 to 7 do
      if plateau.(x).(y) = 1 then incr nb_noirs
      else if plateau.(x).(y) = 2 then incr nb_blancs
    done
  done;
  !nb_noirs - !nb_blancs;;

let autre_joueur j = if j = 1 then 2 else 1

(*Représente les 8 directions des cases adjacentes à une case*)
type direction = G | D | H | B | GH | GB | DH | DB

let liste_directions = [G; D; H; B; GH; GB; DH; DB]

(*Renvoie vrai si il est nécessaire de retourner des jetons après le placement
du jeton en position (x, y) dans la direction dir*)
let rec faut_retourner (plateau, j) (x, y) dir =
  let est_au_bord, x_suivant, y_suivant =
    match dir with
    | G -> x = 0, x-1, y
    | D -> x = 7, x+1, y
    | H -> y = 0, x, y-1
    | B -> y = 7, x, y+1
    | GH -> x = 0 || y = 0, x-1, y-1
    | GB -> x = 0 || y = 7, x-1, y+1
    | DH -> x = 7 || y = 0, x+1, y-1
    | DB -> x = 7 || y = 7, x+1, y+1
  in
  if est_au_bord then false
  else if plateau.(x_suivant).(y_suivant) = autre_joueur j then
    faut_retourner (plateau, j) (x_suivant, y_suivant) dir
  else if plateau.(x_suivant).(y_suivant) = j then true
  else false

(*Retourne les jetons qui doivent être retournés dans la direction dir à partir de la position (x, y)*)
let rec retourner (plateau, j) (x, y) dir =
  let est_au_bord, x_suivant, y_suivant =
    match dir with
    | G -> x = 0, x-1, y
    | D -> x = 7, x+1, y
    | H -> y = 0, x, y-1
    | B -> y = 7, x, y+1
    | GH -> x = 0 || y = 0, x-1, y-1
    | GB -> x = 0 || y = 7, x-1, y+1
    | DH -> x = 7 || y = 0, x+1, y-1
    | DB -> x = 7 || y = 7, x+1, y+1
  in
  if est_au_bord || plateau.(x_suivant).(y_suivant) = j then ()
  else
  begin
    plateau.(x_suivant).(y_suivant) <- j;
    retourner (plateau, j) (x_suivant, y_suivant) dir
  end

(*Place un jeton sur le plateau en retournant des jetons si cela est nécessaire*)
let jouer_coup (plateau, j) (x, y) =
  plateau.(x).(y) <- j;
  List.iter
    (
      fun dir ->
        if faut_retourner (plateau, j) (x, y) dir
        then retourner (plateau, j) (x, y) dir
    )
    liste_directions
  
let afficher_etat (plateau, _) =
  let char_from_jeton jeton = match jeton with
    | 0 -> '.'
    | 1 -> 'X'
    | 2 -> 'O'
    | _ -> failwith "jeton < 0 ou > 2"
  in
  let l = ref 0 in
  print_string "  ";
  Array.iter print_int [|0; 1; 2; 3; 4; 5; 6; 7|];
  print_newline ();
  Array.iter
    (
      fun ligne ->
      print_int !l;
      incr l;
      print_string " ";
      Array.iter (fun case -> print_char (char_from_jeton case)) ligne; print_newline ()
    )
    plateau;;

(*Fait s'affronter les deux stratégies*)
let partie strategie1 strategie2 affichage =
  let strats = [|strategie1; strategie2|] in
  let etat = ref (init_jeu ()) in
  let nb_jetons = ref 4 in
  let temps = [|0.; 0.|] in
  let fini = ref false in
  let score = ref None in
  while !nb_jetons < 8 * 8 && not !fini do
    let plateau, j = !etat in
    if affichage then
    begin
      Printf.printf "Au tour du joueur %d\n" j;
      afficher_etat !etat;
      print_newline ();
      print_newline ()
    end;
    let debut = Sys.time () in
    let coup = strats.(j-1) (copier_plateau plateau, j) in
    Printf.printf "Coup choisi %d %d\n\n" (fst coup) (snd coup);
    temps.(j-1) <- temps.(j-1) +. Sys.time () -. debut;
    if temps.(j-1) > 120. then
      begin
        Printf.printf
          "Temps trop long pour le joueur %d !\n"
          j
        ;
        fini := true;
        score := Some (if j = 1 then -60 else 60)
      end;
    if coup_est_possible (plateau, j) coup then jouer_coup (plateau, j) coup
    else
      begin
        Printf.printf
          "Coup impossible par le joueur %d !\n" j;
        Printf.printf "Le coup impossible est %d %d\n" (fst coup) (snd coup);
        fini := true;
        score := Some (if j = 1 then -64 else 64)
      end;
    incr nb_jetons;
    etat := (plateau, autre_joueur j)
  done;
  afficher_etat !etat;
  Printf.printf "Temps J1 : %fs J2 :%fs\n" temps.(0) temps.(1);
  scorer !etat

(*Renvoie la liste des coups possible pour un plateau donné*)
let lister_coups_possibles (plateau, _) =
  let liste_coups = ref [] in
  let nb_coups = ref 0 in
  for x = 0 to 7 do
    for y = 0 to 7 do
      if coup_est_possible (plateau, 0) (x, y) then
      begin
        liste_coups := (x, y)::!liste_coups;
        incr nb_coups
      end
    done
  done;
  (* print_string "Coups possibles :\n"; *)
  (* List.iter (fun (x, y) -> Printf.printf "%d %d\n" x y) !liste_coups; *)
  (* print_newline (); *)
  !liste_coups, !nb_coups


(*Joue un coup aléatoire parmi ceux possibles*)
let strategie_aleatoire (plateau, j) =
  Random.self_init();
  let liste_coups, nb_coups = lister_coups_possibles (plateau, j) in
  let coup_choisi = Random.int nb_coups in
  List.nth liste_coups coup_choisi

let points = [| [|5;5;5;5;5;5;5;5|];
                [|5;1;1;1;1;1;1;5|];
                [|5;1;1;2;2;1;1;5|];
                [|5;1;2;1;1;2;1;5|];
                [|5;1;2;1;1;2;1;5|];
                [|5;1;1;2;2;1;1;5|];
                [|5;1;1;1;1;1;1;5|];
                [|5;5;5;5;5;5;5;5|] |];;

let f n = n mod 2 - n / 2

let heuristique1 (plateau,_) =
  let score = ref 0 in
  for i = 0 to 7 do
    for j = 0 to 7 do
      score := !score + points.(i).(j) * f (plateau.(i).(j))
    done;
  done;
  !score

let maxi lst = List.fold_left (fun acc x -> max acc x) (min_int,(-1,-1)) lst
let mini lst = List.fold_left (fun acc x -> min acc x) (max_int,(-1,-1)) lst

let rec minimax_rec (plateau, j) depth h =
  let liste_coups, nb_coups = lister_coups_possibles (plateau,j) in
  if depth = 0 then
    (h (plateau,j),(-1,-1))
  else if nb_coups = 0 then ( (*doit on passer son tour ?*)
    let _,nb_coups_autre_joueur = lister_coups_possibles (plateau,autre_joueur j) in
    if nb_coups_autre_joueur = 0 then (*fin de partie*)
      (scorer (plateau,j),(-1,-1))
    else
      minimax_rec (plateau,autre_joueur j) depth h
  )
  else begin
    let traiter coup =
      let copie = copier_plateau plateau in
      jouer_coup (copie,j) coup;
      let score,_ = minimax_rec (copie,(autre_joueur j)) (depth-1) h in
      score,coup
    in
    let lst_score = List.map traiter liste_coups in
    if j = 1 then maxi lst_score
    else mini lst_score
  end

let minimax depth h (plateau,j) =
  snd (minimax_rec (plateau,j) depth h)

let rec alphabeta_aux depth h (plateau,j) alpha beta =
  let coups_possibles, nb_coups = lister_coups_possibles (plateau,j) in
  if depth = 0 then
    (h (plateau,j),(-1,-1))
  else if nb_coups = 0 then begin(*doit on passer son tour*)
    let _,nb_coups_autre_joueur =
  end


let () = 
print_int (partie strategie_aleatoire (minimax 3 heuristique1)  true);
print_newline()
