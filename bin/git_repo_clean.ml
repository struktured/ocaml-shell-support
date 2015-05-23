#!/usr/bin/env ocamlscript
let open Ocamlscript.Std in
begin
Ocaml.packs :=
  ["extlib";"re";"unix";"cmdliner";"fileutils";"re.posix";"containers";"containers.data";"yj_scripts"]
end
--
()
open Cmdliner
open Yj_scripts
open Shell
let home = Unix.getenv "HOME"
let scripts_dir_name = "bin"
            
let _ = Printf.printf "Working directory: %s\n" (working_dir scripts_dir_name)

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

let dry_run =
  let doc = "Dry run - do not actually delete any branches or tags." in
  Arg.(value & flag & info ["d";"dry-run"] ~doc
         ~docv:"DRY_RUN")

let invert_match =
  let doc = "Invert the matching." in
  Arg.(value & flag & info ["v";"invert-match"] ~doc
         ~docv:"INVERT_MATCH")

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



let delete_branch_or_tag ~dry_run url =
  let cmd = Printf.sprintf "git push origin :%s" url in
  print_endline cmd;
  if not dry_run then run cmd else `Ok url

let delete_local_branch ~dry_run url =
  let cmd = Printf.sprintf "git branch -D %s" url in
  print_endline cmd; 
  if not dry_run then match run cmd with 
  | `Ok _ as o -> o 
  | `Error _ -> `Ok ("No local branch: " ^ url)
  else `Ok url

let delete_local_tag ~dry_run url =
  let cmd = Printf.sprintf "git tag -d %s" url in 
  print_endline cmd; 
  if not dry_run then match run cmd with 
    | `Ok _ as o -> o 
    | `Error _ -> `Ok ("No local tag: " ^ url) 
  else `Ok url

let strip_origin url =
  match Re.split (Re_posix.compile_pat "origin/") url with
  | [_;rest] -> rest 
  | _ -> url

let perform_deletions ~for_tags ~local_only ~remote_only ~stop_on_error ~dry_run url = 
  strip_origin url |> fun url ->
        let res' = begin if not local_only then
            match delete_branch_or_tag ~dry_run url with `Error _ as e -> if
              stop_on_error then e else `Ok ("skipping " ^ url) | `Ok _ as o -> o else `Ok url end >>=
          fun _ -> begin if for_tags then delete_local_tag ~dry_run url else
            if not remote_only then delete_local_branch ~dry_run url else `Ok url end in
      res', `Continue

let run pattern for_tags local_only remote_only stop_on_error dry_run
    invert_match =
  if local_only && remote_only then `Error (false, "both local and remote only
  does not make sense!") else
  let matches = Re.matches @@ Re_posix.compile_pat pattern in
  begin if for_tags then get_tags () else get_branches () end >>|
  CCList.fold_while (fun res url ->
      match res with `Error _ as e -> e, `Stop
      | `Ok _ -> begin print_endline @@ "Processing url: " ^ url;
      match matches url with [] -> if invert_match then perform_deletions
            ~for_tags ~local_only ~remote_only ~stop_on_error ~dry_run url else res, `Continue
      | _ -> if not invert_match then perform_deletions ~for_tags ~local_only ~remote_only ~stop_on_error
               ~dry_run url else res, `Continue end)
    (`Ok "")

let cmd =
  let doc = "Remove branches or gits from a git repo by regular expression" in
  Term.(ret (pure run $ pattern $ for_tags $ local_only 
             $ remote_only $ stop_on_error $ dry_run $ invert_match)),
  Term.info "git_repo_clean" ~version:"1.0" ~doc 

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
