open Z3

let parse_line line =
  if Str.string_match (Str.regexp "\\[\\(.*\\)\\]") line 0 then
    let lights_string = Str.matched_group 1 line in
    let lights_count = String.length lights_string in
    let lights =
      List.mapi
        (fun i c -> if c = '#' then Some i else None)
        (List.init lights_count (String.get lights_string))
      |> List.filter_map Fun.id
    in
    if Str.string_match (Str.regexp ".*{\\([0-9,]+\\)}") line 0 then
      let joltage_requirements_string = Str.matched_group 1 line in
      let joltage_requirements =
        Str.split (Str.regexp ",") joltage_requirements_string
        |> List.map int_of_string
      in

      let rest_string =
        String.sub line (lights_count + 3)
          (String.length line
          - String.length joltage_requirements_string
          - lights_count - 6)
      in
      let buttons =
        String.split_on_char ' ' rest_string
        |> List.filter (fun s -> String.length s > 0)
        |> List.map (fun s ->
            String.sub s 1 (String.length s - 2)
            |> String.split_on_char ',' |> List.map int_of_string)
      in

      (lights, lights_count, buttons, joltage_requirements)
    else failwith "No joltage requirements found."
  else failwith "No lights found."

let all_possibilities n =
  let rec aux k =
    if k = 0 then [ [] ]
    else
      let rest = aux (k - 1) in
      List.concat
        [
          List.map (fun lst -> 0 :: lst) rest;
          List.map (fun lst -> 1 :: lst) rest;
        ]
  in
  aux n

let compute_fewest_presses_1 lights light_count buttons =
  let lights_arr =
    Array.init light_count (fun i -> if List.mem i lights then 1 else 0)
  in

  let button_count = List.length buttons in
  let possibilities = all_possibilities button_count in
  let working =
    List.filter
      (fun presses ->
        let light_states = Array.make light_count 0 in
        List.iteri
          (fun i press ->
            if press = 1 then
              List.iter
                (fun light_idx ->
                  light_states.(light_idx) <-
                    (light_states.(light_idx) + 1) mod 2)
                (List.nth buttons i))
          presses;
        light_states = lights_arr)
      possibilities
  in

  List.fold_left
    (fun acc presses ->
      let count = List.fold_left ( + ) 0 presses in
      if count < acc then count else acc)
    max_int working

let solve_button_presses_2 ~buttons ~target =
  let ctx = Z3.mk_context [] in
  let opt = Optimize.mk_opt ctx in
  let n = List.length buttons in

  let xs =
    List.init n (fun i ->
        Arithmetic.Integer.mk_const_s ctx (Printf.sprintf "x_%d" i))
  in

  (* non-negativity constraints *)
  List.iter
    (fun xi ->
      let zero = Arithmetic.Integer.mk_numeral_i ctx 0 in
      let ge0 = Arithmetic.mk_ge ctx xi zero in
      Optimize.add opt [ ge0 ])
    xs;

  (* row-sum constraints *)
  List.iteri
    (fun j target_j ->
      let sum_expr =
        List.fold_left2
          (fun acc xi btn ->
            if List.mem j btn then Arithmetic.mk_add ctx [ acc; xi ] else acc)
          (Arithmetic.Integer.mk_numeral_i ctx 0)
          xs buttons
      in
      let eq =
        Boolean.mk_eq ctx sum_expr
          (Arithmetic.Integer.mk_numeral_i ctx target_j)
      in
      Optimize.add opt [ eq ])
    target;

  (* minimize total button presses *)
  let total_presses = Arithmetic.mk_add ctx xs in
  ignore (Optimize.minimize opt total_presses);

  match Optimize.check opt with
  | Solver.SATISFIABLE ->
      let model = Optimize.get_model opt |> Option.get in
      let solution =
        List.map
          (fun xi ->
            match Model.eval model xi true with
            | Some v -> Expr.to_string v
            | None -> failwith "variable missing")
          xs
      in
      solution
  | _ -> failwith "No solution found"

let part1 lines =
  List.map parse_line lines
  |> List.map (fun (lights, lights_count, buttons, _) ->
      compute_fewest_presses_1 lights lights_count buttons)
  |> List.fold_left ( + ) 0

let part2 lines =
  List.map parse_line lines
  |> List.map (fun (_, _, buttons, target) ->
      let xs = solve_button_presses_2 ~buttons ~target in
      List.map int_of_string xs |> List.fold_left ( + ) 0)
  |> List.fold_left ( + ) 0
