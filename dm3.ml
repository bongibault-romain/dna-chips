(* DM2 - OCAML - Bongibault Romain *)

(* Question 1 *)

let copy_matrix m =
  let lines = Array.length m in
  let columns = if lines > 0 then Array.length m.(0) else 0 in
  let p = Array.make_matrix lines columns 0 in

  for i = 0 to lines - 1 do
    for j = 0 to columns - 1 do
      p.(i).(j) <- m.(i).(j);
    done;
  done;
  
  p
;;

(* Tests Question 1 *)

(* Matrice carré d'ordre 0 *)
let tests_q1_matrix_0 = Array.make_matrix 0 0 4;;
assert ((copy_matrix tests_q1_matrix_0) = tests_q1_matrix_0);;

(* Matrice carrée *)
let tests_q1_matrix_1 = Array.make_matrix 2 2 4;;
assert ((copy_matrix tests_q1_matrix_1) = tests_q1_matrix_1);;

(* Matrice ayant plus de colones que de lignes *)
let tests_q1_matrix_2 = Array.make_matrix 2 4 8;;
assert ((copy_matrix tests_q1_matrix_2) = tests_q1_matrix_2);;

(* Matrice ayant plus de lignes de que de colones *)
let tests_q1_matrix_3 = Array.make_matrix 9 1 0;;
assert ((copy_matrix tests_q1_matrix_3) = tests_q1_matrix_3);;

(* Matrice carré d'ordre 5 avec des valeurs différentes *)
let tests_q1_matrix_4 = [|[|1;3;4;6;9|];[|15;76;8;15;8|];[|0;1;7;8;4|];[|1;9;0;16;89|];[|1;2;3;9;6|]|];;
assert ((copy_matrix tests_q1_matrix_4) = tests_q1_matrix_4);;

(* Question 2 *)
let map f m = 
  let lines = Array.length m in
  let columns = if lines > 0 then Array.length m.(0) else 0 in
  let p = Array.make_matrix lines columns m.(0).(0) in

  for i = 0 to lines - 1 do
    for j = 0 to columns - 1 do
      p.(i).(j) <- f m.(i).(j);
    done;
  done;
  
  p
;;

(* Tests Question 2 *)
let tests_q2_function = fun x -> 2*x + 5;;

(* Matrice carré d'ordre 0 *)
let tests_q2_matrix_0 = Array.make_matrix 0 0 4;;
assert ((map tests_q2_function tests_q2_matrix_0) = tests_q2_matrix_0);;

(* Matrice carrée *)
let tests_q2_matrix_1 = [|[|4;4|];[|4;4|]|];;
assert ((map tests_q2_function tests_q2_matrix_1) = [|[|13;13|];[|13;13|]|]);;

(* Matrice ayant plus de colones que de lignes *)
let tests_q2_matrix_2 = [|[|8;8;8;8|];[|8;8;8;8|]|];;
assert ((map tests_q2_function tests_q2_matrix_2) = [|[|21;21;21;21|];[|21;21;21;21|]|]);;

(* Matrice ayant plus de lignes de que de colones *)
let tests_q2_matrix_3 = [|[|0|];[|0|];[|0|];[|0|];[|0|];[|0|];[|0|];[|0|];[|0|]|];;
assert ((map tests_q2_function tests_q2_matrix_3) = [|[|5|];[|5|];[|5|];[|5|];[|5|];[|5|];[|5|];[|5|];[|5|]|]);;

(* Matrice carré d'ordre 5 avec des valeurs différentes *)
let tests_q2_matrix_4 = [|[|1;3;4;6;9|];[|15;76;8;15;8|];[|0;1;7;8;4|];[|1;9;0;16;89|];[|1;2;3;9;6|]|];;
assert ((map tests_q2_function tests_q2_matrix_4) = [|[|7;11;13;17;23|];[|35;157;21;35;21|];[|5;7;19;21;13|];[|7;23;5;37;183|];[|7;9;11;23;17|]|]);;

(* Question 3 *)

let blit m p dx dy = 
  let m_lines = Array.length m in
  let m_columns = if m_lines > 0 then Array.length m.(0) else 0 in
  let p_lines = Array.length p in
  let p_columns = if p_lines > 0 then Array.length p.(0) else 0 in

  for i = 0 to m_lines - 1 do
    for j = 0 to m_columns - 1 do 
      if (i + dy) < p_lines && (j + dx) < p_columns then 
        p.(i + dy).(j + dx) <- m.(i).(j);
    done;
  done;
;;

(* Question 4 - a *)

let sym_h m =
  let lines = Array.length m in
  let columns = if lines > 0 then Array.length m.(0) else 0 in
  let p = Array.make_matrix lines columns m.(0).(0) in
  
  for i = 0 to lines - 1 do
    for j = 0 to columns - 1 do  
      p.(i).(j) <- m.(lines - 1 - i).(j);
    done;
  done;
  p
;;

(* Question 4 - b *)

let sym_y m =
  let lines = Array.length m in
  let columns = if lines > 0 then Array.length m.(0) else 0 in
  let p = Array.make_matrix lines columns m.(0).(0) in
  
  for i = 0 to lines - 1 do
    for j = 0 to columns - 1 do  
      p.(i).(j) <- m.(i).(columns -  1 - j);
    done;
  done;
  p
;;

(* Question 5 *)

let rec chip_f n = match n with 
  | 1 -> [|[|"A";"C"|];[|"G"; "T"|]|]
  | _ -> let before = chip_f (n - 1) in
    let before_size = Array.length before in
    let next_size = 2 * before_size in
    let p = Array.make_matrix next_size next_size "" in

    for i = 0 to next_size - 1 do
      for j = 0 to next_size - 1 do
        p.(i).(j) <- (match i,j with
          | i,j when i < before_size && j < before_size -> "A" 
          | i,j when i < before_size && j >= before_size -> "C" 
          | i,j when i >= before_size && j < before_size -> "G" 
          | _ -> "T") ^ before.(i mod before_size).(j mod before_size) 
      done;
    done;

    p;;

(* Question 6 *)

let masque b k m = 
  let lines = Array.length m in
  let columns = if lines > 0 then Array.length m.(0) else 0 in
  let p = Array.make_matrix lines columns false in
  
  for i = 0 to (lines - 1) do
    for j = 0 to (columns - 1) do
      if m.(i).(j).[(String.length m.(i).(j)) - k] = b then 
        p.(i).(j) <- true;
    done;
  done;    
  
  p
;;

(* Question 7 *)

let rec dessin m =
  let lines = Array.length m in
  let columns = if lines > 0 then Array.length m.(0) else 0 in

  print_newline ();

  for i = 0 to lines - 1 do
    print_string "|";

    for j = 0 to columns - 1 do
      if m.(i).(j) then 
        print_string "x"
      else 
        print_string " ";
        
      print_string "|";
      done;
    
    print_newline ();
  done;
;;

(* Question 8 *)

let rec chip_g n = match n with 
  | 1 -> chip_f 1
  | n -> let before = chip_g (n - 1) in
  let before_size = Array.length before in
  let next_size = 2 * before_size in
  let p = Array.make_matrix next_size next_size "" in

  blit before p 0 0;
  blit (sym_y before) p before_size 0;
  blit (sym_h (chip_f (n - 1))) p 0 before_size;
  blit (sym_y (sym_h before)) p before_size before_size;

  for i = 0 to next_size - 1 do
    for j = 0 to next_size - 1 do
      p.(i).(j) <- (match i,j with
        | i,j when i < before_size && j < before_size -> "A" 
        | i,j when i < before_size && j >= before_size -> "C" 
        | i,j when i >= before_size && j < before_size -> "G" 
        | _ -> "T") ^ p.(i).(j) 
    done;
  done;

  p
;;

(* Question 9 *)

(*
* Non, il y a toujours autant de trou. En effet, la puce F(n) contient la totalités des possibilités 
* d'arrangements des lettres A,T,G et C sur n caractères, donc il y aura toujours autant de trou dans 
* les masques. 
*)

(* DM2 - OCAML - Bongibault Romain *)