(* Co-installability tools
 * http://coinst.irill.org/
 * Copyright (C) 2010-2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * These programs are free software; you can redistribute them and/or
 * modify them under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

module F (R : Repository.S) = struct

open R

module Quotient = Quotient.F(R)

module Conflicts = Conflicts.F (R)


(* Our JSON package description format: *)

type package = {
  package_name     : string;
  package_depend   : string list list; (* Conjunction of disjunctions of dependencies. *)
  package_conflict : string list;
}

let make_package ~name ?(depend=[[]]) ?(conflict=[]) () = {
  package_name     = name;
  package_depend   = depend;
  package_conflict = conflict;
}


(* Printing JSON packages: *)

(* Pretty-print or squeeze into a single one line omitting default values? *)
type json_printing = Pretty | Dense

let json_of_list (l : string list) = Printf.sprintf "[%s]" (String.concat ", " l)

let json_of_package ?(printing=Pretty) package =
  
  let json_of_conjunction_of_disjunctions (conjunction_of_disjunctions : string list list) =
    json_of_list (List.map json_of_list conjunction_of_disjunctions) in

  (* Half-prepared output: *)
  let name     =                                     package.package_name in
  let depend   = json_of_conjunction_of_disjunctions package.package_depend in
  let conflict = json_of_list                        package.package_conflict in

  match printing with
  | Pretty -> 
      Printf.sprintf 
        "{\n  \"name\"     : %s,\n  \"depend\"   : %s,\n  \"conflict\" : %s\n}" 
        name depend conflict

  | Dense  -> 
      Printf.sprintf "{ \"name\" : %s%s%s }" 
        name
        (match package.package_depend with
         | [[]] -> "" (* Default *)
         | _    -> Printf.sprintf ", \"depend\" : %s" depend)
        (match package.package_conflict with
         | []   -> "" (* Default *)
         | _    -> Printf.sprintf ", \"conflict\" : %s" conflict)


(* Helper functions: *)

(* Transform an iterator over a collection into a map function returning a list. *)
let map_of_iter (collection_iter : ('a -> unit) -> 'c -> unit) (f : 'a -> 'b) (collection : 'c) : 'b list =
  (* Prepare an empty list for the result. *)
  let (result_list : (('b list) ref)) = ref [] in
  (* Use the provoided iter function to iterate over every element of the colection and add it to the list. *)
  collection_iter (fun collection_element ->
    let result = f collection_element in
    result_list := result :: !result_list;
  ) collection;
  (* The list is now inversed with respect to the iter order, we need to reverse it. *)
  result_list := List.rev !result_list;
  !result_list

(* The simpliest map_of_iter which returns a list of the elements of the collection. *)
let list_of_iter (collection_iter : ('a -> unit) -> 'c -> unit) (collection : 'c) : 'a list = 
  map_of_iter collection_iter (fun x -> x) collection

(* Collection iterators with usual sequence of arguments (i.e. first the function, then the collection). *)
(* We define them here to be able to use them consistently with map_of_iter and list_of_iter. *)
let quotient_iter (f : R.Package.t -> unit) (quotient : Quotient.t)  : unit = Quotient.iter f quotient
and formula_iter  (f : R.Disj.t    -> unit) (formula  : R.Formula.t) : unit = Formula.iter  formula f
and disj_iter     (f : R.Package.t -> unit) (disj     : R.Disj.t)    : unit = R.Disj.iter   disj    f



let output
      ?options
      ?package_weight
      ?package_emph
      ?(edge_color = fun _ _ _ -> Some "blue") ?(grayscale =false)
      file ?(mark_all = false) ?(mark_reversed = false) ?(roots = [])
      quotient deps confl =

  (* Mark the packages to be included in the graph *)
  let marks = Hashtbl.create 101 in
  let marked i = Hashtbl.mem marks i in
  let has_dependencies p =
    let dep = PTbl.get deps p in
    not (Formula.implies Formula._true dep ||
         Formula.implies (Formula.lit p) dep)
  in
  let rec mark p =
    if not (marked p) then begin
      Hashtbl.add marks p ();
      PSet.iter mark (Conflict.of_package confl p)
    end
  in
  if mark_all then
    Quotient.iter (fun p -> Hashtbl.add marks p ()) quotient
  else if roots = [] then begin
    Quotient.iter
      (fun p ->
         if has_dependencies p then begin
           mark p;
           Formula.iter (PTbl.get deps p) (fun d -> Disj.iter d mark)
         end)
      quotient;
    if mark_reversed then begin
      let m = Hashtbl.copy marks in
      Hashtbl.clear marks;
      Quotient.iter
        (fun p -> if not (Hashtbl.mem m p) then Hashtbl.add marks p ())
        quotient
    end
  end else (*XXX Find the right algorithm...
         Work on transitive closure of dependencies
         Mark all conflicts; marks all packages at the other side of
         these conflicts and all the alternative in the dependency.
         Proceed recursively...

         Backward mode:
         mark source package and all edges but the one considered

         A package is not relevant if installing it or not has no
         impact on the considered package
       *)
    List.iter mark roots;

  let ch = open_out file in
  let f = Format.formatter_of_out_channel ch in


  (* JZ: Until here I've only removed some code in the "output" function,
         from now I'm replacing it almost completely. *)

  (* List of all the marked packages (i.e. all the equivalence classes). *)
  let (quotient_packages : R.Package.t list) = 
    let all_packages = list_of_iter quotient_iter quotient in
    List.filter marked all_packages in

  
  (* Some string_of_* functions to extract the names of things: *)
  let string_of_eq_class (package : R.Package.t) : string =
    Format.fprintf Format.str_formatter "\"%a\"" (Quotient.print_class quotient) package;
    Format.flush_str_formatter () in

  let string_of_package (package : R.Package.t) : string =
    (* TODO: Print only the name or the name with version? Maybe add a command line option for this. *)
    Format.fprintf Format.str_formatter "\"%a\"" (R.Package.print_name (Quotient.pool quotient)) package;
(*
    Format.fprintf Format.str_formatter "\"%a\"" (R.Package.print (Quotient.pool quotient)) package;
*)
    Format.flush_str_formatter () in


  (* 1. Dependencies and conflicts between package equivalence classes. *)

  (* List of json packages describing equivalence classes. *)
  let eq_classes_packages : package list = 
    List.map (fun (eq_class : R.Package.t) ->

      (* Prepare required coinst data about the equivalence class. *)
      let (coinst_dependencies : Formula.t) = PTbl.get deps eq_class in
      let (coinst_conflicts    : PSet.t)    = Conflict.of_package confl eq_class in

      (* A list of lists of R.Package.t representing a conjunction of disjunctions of dependencies. *)
      let conjunction_of_disjunctions =
        map_of_iter formula_iter (fun (coinst_disjunction : R.Disj.t) ->
          list_of_iter disj_iter coinst_disjunction
      ) coinst_dependencies in

      (* A list of R.Package.t representing conflicts. *)
      let conflict_list = list_of_iter PSet.iter coinst_conflicts in

      (* The JSON package: *)
      make_package
        ~name:     (string_of_eq_class eq_class)
        ~depend:   (List.map (List.map string_of_eq_class) conjunction_of_disjunctions)
        ~conflict: (List.map string_of_eq_class conflict_list)
        ()

    ) quotient_packages in
  
  (* Debug: *)(* Quotient.print quotient deps; *)


  (* 2. Links between packages and their equivalence classes. *)

  (* List of json packages describing real packages: each package depends on his equivalence class. *)
  let packages_in_eq_classes : package list =
    List.flatten (List.map (fun (eq_class : R.Package.t) ->
      
      (* Prepare required coinst data about the representants of this equivalence class. *)
      let (representants : R.PSet.t) = 
        try Quotient.clss quotient eq_class 
        with Not_found -> failwith "Json module: Problem with Quotient.clss!" in

      if R.PSet.cardinal representants = 1
      (* If the class of equivalence contains just one package, we do not need
         to make this package depend on itself. Moreover this package will be
         already outputed in the equivalence class part so we are done here. *)
      then []
      (* Else each package in the equivalence class should depend on that class. *)
      else map_of_iter PSet.iter (fun (package : R.Package.t) -> 

          (* The JSON package: *)
          make_package
            ~name:   (string_of_package package)
            ~depend: ([[string_of_eq_class eq_class]])
            ()

        ) representants 
      
    ) quotient_packages) in


  (* Print the whole list of equivalence class descriptions and package implementations. *)
  Format.fprintf f 
    "[\n%s,\n%s\n]@." 
    (String.concat ",\n" (List.map (json_of_package ~printing:Pretty) eq_classes_packages))
    (String.concat ",\n" (List.map (json_of_package ~printing:Dense)  packages_in_eq_classes));

  close_out ch


let output_list f g l =
  Format.fprintf f "@[<1>[";
  begin match l with
    []     -> ()
  | x :: r -> g f x; List.iter (fun x -> Format.fprintf f ",@,%a" g x) r
  end;
  Format.fprintf f "]@]"

let output_packages quotient f s =
  output_list f
    (fun f p ->
       Format.fprintf f "\"%a\""
         (R.Package.print_name (Quotient.pool quotient)) p)
    s

let output_classes quotient f l =
  output_list f
    (fun f p ->
       Format.fprintf f "@[<1>[\"%a\",@,%a]@]"
         (R.Package.print_name (Quotient.pool quotient)) p
         (output_packages quotient) (PSet.elements (Quotient.clss quotient p)))
    l

let output_sets quotient f l = output_list f (output_packages quotient) l

let output_non_coinstallable_sets file quotient sets =
  let packages = List.fold_left PSet.union PSet.empty sets in
  let ch = open_out file in
  let f = Format.formatter_of_out_channel ch in
  Format.fprintf f
    "@[<1>{@[<2>\"classes\":@,%a;@]@,@[<2>\"incompatibilities\":@,%a@]}@]@."
    (output_classes quotient) (PSet.elements packages)
    (output_sets quotient) (List.map PSet.elements sets);
  close_out ch

end
