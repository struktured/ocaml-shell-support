open Shell
module Fun = CCFun
module Params =
struct
 type global_params = {ssl_no_verify : bool option}
  module Command =
    struct
     type clone_params = {url:string; target:string option; branch_or_tag:string option;
        single_branch:bool option}
      type t = Clone of clone_params

    end
  type t = { global_params:global_params option; command:Command.t }

  let create ?global_params command = {global_params;command}
end

module Response =
  struct
    type t = Directory of FilePath.filename
end

let some_or_blank f = function None -> "" | Some s -> f s

let debug s = print_endline @@ "[git] " ^ s

let build_command params = let open Params in
  some_or_blank (fun g -> some_or_blank (fun b ->
    "GIT_SSL_NO_VERIFY=" ^ (string_of_bool b) ^ " ") g.ssl_no_verify)
  params.global_params ^
  "git " ^
  let open Command in
  match params.command with
   | Clone p ->
     let clone_string = "clone " ^ p.url in
     let branch_or_tag = some_or_blank ((^) " -b ") p.branch_or_tag in
     let target = some_or_blank ((^) " ") p.target in
     let single_branch = some_or_blank (fun b -> if b then " --single-branch"
      else " --no-single-branch") p.single_branch in
     clone_string ^ branch_or_tag ^ single_branch ^ target

let with_params params =
  let open Params in
  Shell.run @@ build_command params
   |> function
    | `Error _ as e -> e
    | `Ok s ->
    match params.command with
    | Command.Clone p -> match p.Command.target with
      | Some t -> `Ok (Response.Directory t)
      | None -> `Ok (Response.Directory
        (FilePath.basename p.Command.url |> FilePath.chop_extension))

let run ?global_params command = with_params @@ Params.create ?global_params command

let clone ?ssl_no_verify ?target ?branch_or_tag ?single_branch url =
  let open Params in
  match with_params @@ create (Command.(Clone {url;target;branch_or_tag;single_branch}))
  with
  | `Ok (Response.Directory dir) -> `Ok dir
  | `Error _ as e -> e
