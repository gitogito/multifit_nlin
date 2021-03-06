open Printf
open ExtLib

let maxiter = 50
let epsabs = 1e-4
let epsrel = 1e-4
let xinit = Gsl.Vector.of_array [| 1.0; 1.0; 1.0; 1.0 |]
let num_x = Gsl.Vector.length xinit

let my_fun x t =
  assert (Gsl.Vector.length x = num_x);
  let open! Flop in
  x.{0} * t ** 3.0 +
  x.{1} * t ** 2.0 +
  x.{2} * t ** 1.0 +
  x.{3} * t ** 0.0

let df_my_fun n x t =
  assert (Gsl.Vector.length x = num_x);
  let x_copy = Gsl.Vector.copy x in
  let f x' =
    x_copy.{n} <- x';
    my_fun x_copy t
  in
  let res = Gsl.Deriv.central ~f ~x:x.{n} ~h:epsabs in
  res.Gsl.Fun.res

let multi_funs t y sigma =
  let multi_f ~x ~f =
    assert (Gsl.Vector.length x = num_x);
    let n = Gsl.Vector.length f in
    assert (Array.length t = n);
    assert (Array.length y = n);
    assert (Array.length sigma = n);
    for i = 0 to pred n do
      let open! Flop in
      let yi = my_fun x t.(i) in
      f.{i} <- (yi - y.(i)) / sigma.(i)
    done
  in
  let multi_df ~x ~j =
    let (n, p) = Gsl.Matrix.dims j in
    assert (Gsl.Vector.length x = p);
    assert (Array.length sigma = n);
    for i = 0 to pred n do
      let sgm = sigma.(i) in
      for k = 0 to num_x - 1 do
        j.{i, k} <- df_my_fun k x t.(i) /. sgm
      done
    done
  in
  let multi_fdf ~x ~f ~j =
    multi_f ~x ~f;
    multi_df ~x ~j
  in
  { Gsl.Fun.multi_f = multi_f;
    Gsl.Fun.multi_df = multi_df;
    Gsl.Fun.multi_fdf = multi_fdf;
  } 

let print_pos fit =
  let pos = Gsl.Vector.create num_x in
  Gsl.Multifit_nlin.position fit pos;
  printf "%g" pos.{0};
  for i = 1 to num_x - 1 do
    printf "\t%g" pos.{i}
  done;
  print_newline ()

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
  let fit = Gsl.Multifit_nlin.make Gsl.Multifit_nlin.LMSDER ~n ~p:num_x (multi_funs t y sigma) xinit in
  solve fit;
  print_pos fit
