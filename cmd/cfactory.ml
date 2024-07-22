open Params
open Command
open Str
open Src

let tbl = Hashtbl.create 10

(** [name] is name is call command. *)
let name = ref ""

(** [description] is description of a command. *)
let description = ref ""

(** [param_lst] is list of parameters, in order how they should be called. *)
let param_lst = ref [ Params.make_flag_param "fake" "fake description" ]

(** [cmd] is a function that takes a list of parameters and executes a command *)
let cmd = ref (fun (l : ptype list) -> ())

(** [update_tbl ()] updates [tbl] with new command. *)
let update_tbl () =
  let cmdt = Command.make !name !description !param_lst !cmd in
  Hashtbl.replace tbl !name cmdt

(* command for help ================================================== *)

let _ =
  name := "help";
  description := "prints summaries of all commands";
  param_lst := [];
  (cmd :=
     fun plst ->
       let len c = String.length (Command.help c) in
       let max_len = Hashtbl.fold (fun k v acc -> max acc (len v)) tbl 0 in
       let summ c =
         Command.help c
         ^ String.make (max 0 (max_len - len c) + 1) ' '
         ^ Command.summary c
       in
       match Hashtbl.fold (fun k v acc -> v :: acc) tbl [] with
       | [] -> print_endline ""
       | c1 :: t ->
           let prespace = "  " in
           "Here's a list of all the Mit commands:\n"
           ^ List.fold_left
               (fun acc c -> acc ^ "\n" ^ prespace ^ summ c)
               (prespace ^ summ c1)
               t
           ^ "\n\n\
              For more details, call 'mit <command> --help' for more in-depth \
              details about a command."
           |> print_endline);
  update_tbl ()

(* command for init ================================================== *)
let _ =
  name := "init";
  description := "initializes a mit repo in current working directory";
  param_lst := [];
  (cmd := fun plst -> Mit.init ());
  update_tbl ()

(* command for clean ================================================== *)
let _ =
  name := "clean";
  description := "removes mit repo from current working directory";
  param_lst := [];
  (cmd :=
     fun plst ->
       print_string
         "Are you sure you want to delete the current mit repo? (Y/n): ";
       let input = read_line () in
       match String.lowercase_ascii input with
       | "y" | "yes" -> Mit.clean ()
       | _ -> ());
  update_tbl ()

(* command for status ================================================== *)
let _ =
  name := "status";
  description := "prints changes made to repo";
  param_lst := [];
  (cmd := fun plst -> Mit.status ());
  update_tbl ()

(* command for commit ================================================== *)
let _ =
  name := "commit";
  description := "make a commit with a given author and message";
  param_lst :=
    [
      Params.make_string_param None "author of this commit";
      Params.make_string_param None "message of this commit";
    ];
  (cmd :=
     fun plst ->
       match plst with
       | a :: m :: _ ->
           let a = Params.extract_string a in
           let m = Params.extract_string m in
           Mit.commit a m
       | _ -> failwith "Unreachable by assumption");
  update_tbl ()

(* command for log ================================================== *)
let _ =
  name := "log";
  description := "look at all past commits";
  param_lst := [];
  (cmd := fun plst -> Mit.log ());
  update_tbl ()

(* command for changes ================================================== *)
let _ =
  name := "change";
  description := "changes between current and last commit";
  param_lst := [];
  (cmd := fun plst -> Mit.change ());
  update_tbl ()

(* command for branch =================================== *)
let _ =
  name := "branch";
  description := "list or create branches";
  param_lst := [ Params.make_string_param (Some "b") "name of new branch" ];
  (cmd :=
     fun plst ->
       match plst with
       | s :: _ ->
           let s = Params.extract_string s in
           Mit.branch s
       | _ -> failwith "Unreachable by assumption");
  update_tbl ()

(* command for checkout =================================== *)
let _ =
  name := "checkout";
  description := "switches or creates branch";
  param_lst :=
    [
      Params.make_string_param None "name of branch to switch to";
      Params.make_flag_param "b" "allows creation of new branch";
    ];
  (cmd :=
     fun plst ->
       match plst with
       | s :: b :: _ ->
           let s = Params.extract_string s in
           let b = Params.extract_bool b in
           Mit.checkout s b
       | _ -> failwith "Unreachable by assumption");
  update_tbl ()
