
type case = int * int * int
type dimension = int
  
let est_dans_losange (c:case) (dim:dimension) : bool =
  match c with
  | (i, j, k) -> 
      if ((i > dim && -dim <= 0 && -dim <= k && k < 0) ||
          (i < -dim && 0 < j && j <= dim && 0 < k && k <= dim))
      then true
      else false 

3)
let est_dans_etoile (c:case) (dim:dimension) : bool =
  match c with
  | (i, j, k) -> 
      if (-dim <= i && i <= 2*dim &&
          -dim <= j && j <= 2*dim &&
          -dim <= k && k <= 2*dim)
      then true
      else false

4)
let tourner_case (m:int) (c:case) : case= 
  let rec tourner (m: int) (c: case)  : case = 
    match c with
    | (i, j, k) -> 
        if m = 0 then c (* Si m est égal à zéro, nous avons terminé les rotations *)
        else tourner  (m - 1) (-k, -i, -j) (* Effectuer une rotation et réduire m de 1 *) 
  in tourner m c

5)
let translate (c:case) (v:vecteur): case =
  let (i, j, k) = c in
  let (x, y, z) = v in
  (i + x, j + y, k + z)

6)
let diff_case (c1:case) (c2:case): vecteur=
  let (i, j, k) = c1 in
  let (x, y, z) = c2 in
  (i - x, j - y, k - z)

7)
let sont_cases_voisines (c1: case) (c2: case) : bool = 
  let diff=diff_case c1 c2 in
  match diff with
  | (x, y, z) ->
      if abs(x)<=1 && abs(y)<=1 && abs(z)<=1 
      then true
      else false

8)
let calcul_pivot (c1: case) (c2: case) : case_option =
  match c1, c2 with
  | (i1, j1, k1), (i2, j2, k2) ->
      if i1 = i2 || j1 = j2 || k1 = k2 then (* Vérifie si les cases sont alignées *)
        let diff_i = abs (abs(i2) - abs(i1)) in
        let diff_j = abs (abs(j2) - abs(j1)) in
        let diff_k = abs (abs(k2) - abs(k1)) in 
        if diff_i mod 2 = 0 || diff_j mod 2 = 0 || diff_k mod 2 = 0 then
          let i_mid = (i1 + i2) / 2 in
          let j_mid = (j1 + j2) / 2 in
          let k_mid = (k1 + k2) / 2 in
          Some (i_mid, j_mid, k_mid) (* Retourne la case à mi-chemin *) 
        else
          None (* Le nombre de cases entre les deux cases est pair, donc il n'y a pas de case à mi-chemin *)
      else
        None (* Les cases ne sont pas alignées *)

9)
let vec_et_dist (c1: case) (c2: case) : vecteur * int =
  match c1, c2 with
  | (i1, j1, k1), (i2, j2, k2) ->
      let distance = int_of_float(sqrt(float_of_int((i2-i1)*(i2-i1)+(j2-j1)*(j2-j1)+(k2-k1)*(k2-k1)))) in
      let diff = diff_case c1 c2 in
      match diff with
      | (x, y, z) -> 
          let dx = if x > 0 then 1 else if x < 0 then -1 else 0 in
          let dy = if y > 0 then 1 else if y < 0 then -1 else 0 in
          let dz = if z > 0 then 1 else if z < 0 then -1 else 0 in
          ((dx, dy, dz), distance)
