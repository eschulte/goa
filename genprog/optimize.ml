(* Optimize metrics in a population of software variants
 *
 * Starting with an initial software object, generate a population of
 * variant implementations and then evolve to optimize some metric
 * such as fastest execution, least communication, lowest energy
 * consumption etc...
 *
 * Options to Use
 *
 * --popsize 100
 * --tournament-size 2
 * --test-script ./host-test.sh
 * --test-command __TEST_SCRIPT__ __SOURCE_NAME__ __FITNESS_FILE__
 * 
 *)
open Printf
open Cil
open Global
open Elf
open Population
open Asmrep

let optimize_feature = ref ""
let optimize_maximize = ref false

let _ =
  options := !options @
    [
      "--optimize-feature", Arg.Set_string optimize_feature,
      " Specify the non-functional feature to optimize";
      "--optimize-maximize", Arg.Set optimize_maximize,
      " Specify the non-functional feature to optimize";
    ]

class optRep = object (self : 'self_type)
  inherit asmRep as super

  (** A hash to store keyed fitness values *)
  val test_results = Hashtbl.create 255

  (** [CLG] Calculate multi-objective fitness based on program
      neutrality and the values stored in test_results.  If the
      program is not functionally neutral then its fitness is zero, if
      it is, then its fitness is based on how well it maximizes or
      minimizes some non-functional aspects. *)
  method multi_objective_fitness result =
    if result then
      0.0
    else
      let value = Hashtbl.find test_results optimize_feature in
        if !optimize_maximize then value else (1.0 /. value)

  (** Need a way to return non-scalar fitness back from fitness
      
      Given the appropriate commands (above), you can replicate this
      by overriding the method internal_test_case_postprocess status
      fitness_file to process the fitness_file to which the output is
      saved to do whatever you want to the fitness.
      internal_test_case_postprocess returns an array of floating
      point values corresponding to the different possible dimensions
      along which fitness is evaluated.  all the output-to-stats and
      related code in the code on your website then go in
      internal_test_case_postprocess in your representation
      subclass. *)
  method private internal_test_case_postprocess status fitness_file =
    let str = file_to_string fitness_file in 
    let result = match status with 
      | Unix.WEXITED(0) -> true 
      | _ -> false
    in 
    let lines = Str.split (Str.regexp "[\r\n]+") str in
    let pairs = List.map (fun l -> Str.split (Str.regexp "[, \t]+") l) lines in
      liter (fun pair ->
               let key = ref (List.nth pair 0) in
               let value = ref (float_of_string (List.nth pair 1)) in
                 Hashtbl.add test_results key !value)
        pairs;
      result, [| (self#multi_objective_fitness result) |]

end

let do_opt file (rep :('a,'b) Rep.representation) =
  (* load the rep, either from a cache or from source *)
  rep#load file;
  rep#debug_info();
  ()

let main () = begin
  Arg.parse !options (fun x -> ()) "optimize [options] program";
  printf "Optimizing %s for %s\n" !program_to_repair !optimize_feature;
  do_opt !program_to_repair ((new optRep) :>('a,'b) Rep.representation);
end ;;
main ()
