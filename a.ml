open Printf
open ExtLib

let maxiter = 50
let epsabs = 1e-4
let epsrel = 1e-4
let p = 3

let expb t y sigma =
  let expb_f ~x ~f =
    assert (Gsl.Vector.length x = p);
    let n = Gsl.Vector.length f in
    assert (Array.length t = n);
    assert (Array.length y = n);
    assert (Array.length sigma = n);
    let na = x.{0} in
    let m = x.{1} in
    let s = x.{2} in
    for i = 0 to pred n do
      (* model Yi = na * exp(-(x-m)**2 / (2*s**2)) *)
      let open! Flop in
      let ex = exp (-((t.(i)-m)**2. / (2.*s**2.))) in
      let yi = na * ex in
      f.{i} <- (yi - y.(i)) / sigma.(i)
    done
  in
  let expb_df ~x ~j =
    let (n, p) = Gsl.Matrix.dims j in
    assert (Gsl.Vector.length x = p);
    assert (Array.length sigma = n);
    let na = x.{0} in
    let m = x.{1} in
    let s = x.{2} in
    for i = 0 to pred n do
      (* Jacobian matrix J(i,j) = dfi / dxj,        *) 
      (* where fi = (Yi - yi)/sigma[i],             *) 
      (*       Yi = na * exp(-(x-m)**2 / (2*s**2)) *)
      (* and the xj are the parameters (A,lambda,b) *) 
      let sgm = sigma.(i) in
      let open! Flop in
      let ex = exp (-((t.(i)-m)**2. / (2.*s**2.))) in
      j.{i, 0} <- ex / sgm;
      j.{i, 1} <- na * (t.(i)-m)     * ex / s**2. / sgm;
      j.{i, 2} <- na * (t.(i)-m)**2. * ex / s**3. / sgm
    done
  in
  let expb_fdf ~x ~f ~j =
    expb_f ~x ~f ;
    expb_df ~x ~j
  in
  { Gsl.Fun.multi_f = expb_f ;
    Gsl.Fun.multi_df = expb_df ;
    Gsl.Fun.multi_fdf = expb_fdf ;
  } 

let print_pos fit =
  let pos = Gsl.Vector.create p in
  Gsl.Multifit_nlin.position fit pos;
  printf "%g\t%g\t%g\n" pos.{0} pos.{1} pos.{2}

let solve fit =
  let rec aux n =
    if n >= maxiter then
      printf "exceed maxiter\n"
    else begin
      print_pos fit;
      Gsl.Multifit_nlin.iterate fit;
      let status = Gsl.Multifit_nlin.test_delta fit ~epsabs ~epsrel in
      if status then
	printf "converged\n"
      else begin
        printf "iter: %d\n" (succ n);
	aux (succ n)
      end
    end
  in
  aux 0

let get_data () =
  (* returns a pair of lists from a list list *)
  let f l =
    let rec f' la lb = function
        [] -> la, lb
      | [a; b] :: l -> f' (a :: la) (b :: lb) l
      | _ -> failwith "f"
    in
    let la, lb = f' [] [] l in
    (List.rev la), (List.rev lb)
  in
  let l = List.map
            (fun x -> List.map float_of_string (Pcre.split x))
            (List.of_enum (Std.input_lines stdin))
  in
  let lx, ly = f l in
  let x = Array.of_list lx in
  let y = Array.of_list ly in
  (x, y)

let () =
  let t, y = get_data () in
  let n = Array.length t in
  assert (n = Array.length y);
  let sigma = Array.make n 0.1 in
  let xinit = Gsl.Vector.of_array [| 1.0; 0.1; 1.0 |] in
  let fit = Gsl.Multifit_nlin.make Gsl.Multifit_nlin.LMSDER ~n ~p (expb t y sigma) xinit in
  solve fit;
  print_pos fit
