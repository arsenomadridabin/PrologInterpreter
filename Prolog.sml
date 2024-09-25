(* Prolog Interpreter *)

(* List of Exceptions defined *)
exception unsolvable and non_unifiable and occur_check and length;

(* An empty substitution *)
fun emptySubst (v, i) = Var (v, i);


(* Create a list of pairs out of two lists *)
fun pair_up(nil,nil) = nil
    | pair_up(x::xs,y::ys) = (x,y)::pair_up(xs,ys)
  | pair_up(_) = raise length;

(* Function for occur check .. required for Unification*)
fun occurs v (Var x) = v = x
  | occurs v (Fun (_, args)) = List.exists (occurs v) args;

fun update (v,t) (S) v1 = if v = v1 then t else S v1;

fun value S (Var x) = S x
  | value S (Fun (f, args)) = Fun (f, map (value S) args);

(* Find value of variables using substitution : recursive when result type Var
* *) 
fun recursiveValue S (Var x) =
    let
        val result = S x  (* Apply the substitution S directly using x *)
    in
        case result of
            Var y => recursiveValue S (Var y)  (* If the result is another Var, continue resolving *)
          | _ => result  (* If result is a Fun or another non-Var, return it *)
    end
  | recursiveValue S (Fun (f, args)) = Fun (f, map (recursiveValue S) args);  (* Recursively apply value to arguments of Fun *)

(* Unification *)
fun unify((t1,t2),S) =
    let
        val t1 = value S t1 and t2 = value S t2
    in
        case (t1,t2) of
            (Var x, Var y) => if x = y then S else update (x,t2) S
          | (Var x, _) => if occurs x t2 then raise occur_check else update (x,t2) S
          | (_, Var x) => if occurs x t1 then raise occur_check else update (x,t1) S
          | (Fun (f1, args1), Fun (f2, args2)) =>
          (
            if f1 = f2 then
            (
            List.foldr unify S (pair_up(args1,args2))
            )
            else
            (
            raise non_unifiable
            )
            )
    end;

(* Add the depth information in a variable *)
fun rename l (Var (x, _)) = Var (x, l)
  | rename l (Fun (f, args)) = Fun (f, map (rename l) args);


(* Working Solver *)
fun solve (goals, db) =
    let
        (* Base solve function with matching patterns for different cases *)
        fun solve(nil, _, _, S) = S  (* All goals resolved *)
        | solve(_, nil, _, _) =
            raise unsolvable
        | solve(_, Headless _ :: _, _, _) =
            raise unsolvable
        | solve (goal :: goals, rules, level, S) =
            let
                (* Attempt each rule on the current goal, backtracking if necessary *)
                fun attemptRules [] = 
                  (
                  raise unsolvable
                  )
                | attemptRules (Headed (head, body) :: rest) =
                    let
                        val renamedHead = rename level head
                        val renamedBody = map (rename level) body
                        val newSubst = unify ((goal, renamedHead), S)
                        handle non_unifiable => 
                            attemptRules rest
                    in
                        (
                        solve (goals @ renamedBody, rules, level + 1, newSubst)
                        handle unsolvable =>(
                            attemptRules rest)
                        )
                    end
            in              
                attemptRules rules
            end
    in
        solve (goals, db, 1, emptySubst)
    end;


(* Collects all variables from a term *)
fun collectVars (Var (s, n)) = [(s, n)]
  | collectVars (Fun (_, args)) = List.concat (List.map collectVars args)

fun collectVarsFromGoals goals = List.concat (List.map collectVars goals);

(* OutQuery *)
fun OutQuery (goals, db) =
    let
      val S = solve (goals, db)

      (* Collect variables from goals *)
      val collections = collectVarsFromGoals goals

      (* Reverse list of variables *)
      val reversedCollections = List.rev collections

      (* Remove duplicates *)
        val uniqueCollections = List.foldl (fn (x, acc) => if List.exists (fn y
        => x = y) acc then acc else x::acc) [] reversedCollections

      (* look up values of variable in S *)
        val values = List.map (fn x => (Var x, recursiveValue S (Var x))) uniqueCollections
    in
      (* pass list of (variable, value) pairs to OutSol *)
      OutSol values
    end
    handle unsolvable => OutLine "No";


(* Prolog *)

fun Prolog (x as (Headed (Var _, _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (x as (Headless (Var _ :: _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (Headed (Fun ("init", nil),nil)) = InitDB ()
  | Prolog (x as (Headed _)) = Assert x
  | Prolog (x as Headless y) =
    (OutLine ("query:  " ^ PrintClause x);
     OutQuery (y, !db)
    )
