(* test.ml *)
open Unix

   
let walk_directory_tree dir pattern =
  let re = print_endline ("we are at test walk"^dir^"\\"^pattern);Str.regexp pattern in
  (* pre-compile the regexp *)
  let select str = Str.string_match re str 0 in
  let rec walk acc (*er_string*)  = function
    | [] -> (acc, "test") (*er_string*)
    | dir::tail ->
       try
         let contents = Array.to_list (Sys.readdir dir) in
         let contents = List.rev_map (Filename.concat dir) contents in
         let dirs, files =
           List.fold_left (fun (dirs,files) f ->
               match (stat f).st_kind with
               | S_REG -> (dirs, f::files)  (* Regular file *)
               | S_DIR -> (f::dirs, files)  (* Directory *)
               | _ -> (dirs, files)
             ) ([],[]) contents    
         in
         let matched = List.filter (select) files in
         walk ( matched @ acc) (*er_string*) (dirs @ tail)
       with
       |Sys_error e -> walk (acc) (*er_string^"\n"^ e*) (tail)
  in
  walk [] (*""*) [dir]

  


