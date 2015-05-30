open Shell
module Params =
struct
  module Command = 
    struct 
      type clone_params = {url:string; target:string option; branch_or_tag:string option}
      type t = Clone of clone_params
    end
  type t = { command:Command.t }

  let create command = {command}
end

module Response =
  struct 
    type t = Directory of string
end

let debug s = print_endline @@ "[Git] " ^ s

let build_command params = let open Params in
  "git " ^
  match params.command with
   | Command.Clone p -> 
     let clone_string = "clone " ^ p.Command.url in
     let branch_or_tag = match p.Command.branch_or_tag with
     | Some b -> " -b " ^ b
     | None -> "" in
     let target = match p.Command.target with
     | Some t -> " " ^ t
     | None -> "" in
     clone_string ^ branch_or_tag ^ target

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

let run command = with_params @@ Params.create command

let clone ?target ?branch_or_tag url =
  let open Params in
  with_params @@ create (Command.(Clone {url;target;branch_or_tag}))

