(**#!/usr/bin/env ocamlscript
Ocaml.packs :=
  ["extlib";"re";"unix";"cmdliner";"fileutils";"re.posix";"containers"]
 Ocaml.sources := ["shell_utils"]

--**)

open Cmdliner 
open Shell_utils
let home = Unix.getenv "HOME"
let scripts_dir_name = "bin"
            
let _ = Printf.printf "Working directory: %s\n" working_dir

let no_ssl_verify_opt = "GIT_SSL_NO_VERIFY=true"

let pattern =
  let doc = "Specifies a regular expression for branch/tag deletion candidates."
  in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PATTERN")

let for_tags =
  let doc = "Delete tags, not branches." in
  Arg.(value & flag & info ["t";"tag"] ~doc
         ~docv:"TAG")

let local_only =
  let doc = "Delete local branches only." in
  Arg.(value & flag & info ["l";"local"] ~doc
         ~docv:"LOCAL")

let remote_only =
  let doc = "Delete remot branches only." in
  Arg.(value & flag & info ["r";"remote"] ~doc
         ~docv:"REMOTE")

let stop_on_error =
  let doc = "Stop upon any (non-local) git errors." in
  Arg.(value & flag & info ["s";"s"] ~doc
         ~docv:"STOP_ON_ERROR")

open Infix

let print s = Printf.printf "[git-repo-clean]: %s\n" s

let get_branches () =
  let cmd = "git branch -a -r" in
  print_endline cmd; run cmd >>|
  Re.split (Re_posix.compile_pat "\n")
  
let get_tags () = 
  let cmd = "git tag -a -r" in
  print_endline cmd; run cmd >>|
  Re.split (Re_posix.compile_pat "\n")
 

let delete_branch_or_tag url =
  let cmd = Printf.sprintf "git push origin :%s" url in
  print_endline cmd; run cmd

let delete_local_branch url =
  let cmd = Printf.sprintf "git branch -D %s" url in
  print_endline cmd; 
  match run cmd with `Ok _ as o -> o | `Error (_, e) -> 
    `Ok ("No local branch: " ^ url)

let delete_local_tag url =
  let cmd = Printf.sprintf "git tag -d %s" url in 
  print_endline cmd;
  match run cmd with `Ok _ as o -> o | `Error (_, e) -> 
    `Ok ("No local tag: " ^ url)


let strip_origin url =
  match Re.split (Re_posix.compile_pat "origin/") url with
  | [_;rest] -> rest 
  | _ -> url

  
let run pattern for_tags local_only remote_only stop_on_error =
  if local_only && remote_only then `Error (false, "both local and remote only
  does not make sense!") else
  let matches = Re.matches @@ Re_posix.compile_pat pattern in
  begin if for_tags then get_tags () else get_branches () end >>|
  CCList.fold_while (fun res url ->
      match res with `Error _ as e -> begin print_endline "Error!";e, `Stop end
      | `Ok _ -> begin print_endline @@ "Processing url: " ^ url;
      match matches url with [] -> res, `Continue
      | _ -> let url = strip_origin url in
        let res' = begin if not local_only then
            match delete_branch_or_tag url with `Error _ as e -> if
              stop_on_error then e else `Ok ("skipping " ^ url) | `Ok _ as o -> o else `Ok url end >>=
          fun _ -> begin if for_tags then delete_local_tag url else
            if not remote_only then delete_local_branch url else `Ok url end in
      res', `Continue end)
    (`Ok "")

let cmd =
  let doc = "Remove branches or gits from a git repo by regular expression" in
  Term.(ret (pure run $ pattern $ for_tags $ local_only $ remote_only $ stop_on_error)),
  Term.info "git_repo_clean" ~version:"1.0" ~doc 

let () = match Term.eval cmd with `Error _ -> print_endline "Error!";
  exit 1 | _ -> exit 0
