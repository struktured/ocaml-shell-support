open Shell
module Fun = CCFun

(* If git version fails, use this value *)
let default_single_branch_supported = true

module Params =
struct
  module Globals =
    struct 
      let default_git_binary = "git"
      type t = {git_binary:string; ssl_no_verify : bool option}
      let create ?(git_binary=default_git_binary) ?ssl_no_verify () =
        {git_binary;ssl_no_verify}
    end
  type global_params = Globals.t

  module Command =
    struct
     type clone_params = {url:string; target:string option; branch_or_tag:string option;
        single_branch:bool option}

      type t = Clone of clone_params | Version
    end
  type t = { global_params:global_params; command:Command.t }

  let create ?(global_params=Globals.create ()) command = {global_params;command}
end

module Response =
  struct
    module Version =
    struct
      type t = {increments:int list;name:string} [@@deriving show]
      let to_string t = t.name
      let compare t t' =
        let cmp = CCList.compare CCInt.compare t.increments t'.increments in
        if cmp <> 0 then cmp else CCString.compare t.name t'.name
      let of_string =
        let prefix = "git version" in fun s ->
        CCString.find ~sub:prefix s |> fun pos -> 
        begin 
          if pos < 0 then s else String.sub s (pos + String.length prefix) 
            (String.length s - String.length prefix)
        end |> String.trim |>
        fun name -> 
          let increments = Re_posix.compile_pat "\\." |> 
          fun pat -> Re.split pat name |> CCList.filter_map (fun s -> 
          try Some (int_of_string s) with _ -> None) in
          {increments;name}
    end
    type version = Version.t [@@deriving show]
    type filename = string [@@deriving show]
    type t = Directory of filename | Version of version [@@deriving show]
end

let some_or_blank f = function None -> "" | Some s -> f s

let debug s = print_endline @@ "[git] " ^ s

let rec build_command params = let open Params in
  let g = params.global_params in
  some_or_blank (fun b -> "GIT_SSL_NO_VERIFY=" ^ (string_of_bool b) ^ " ") 
  g.Globals.ssl_no_verify ^
  g.Globals.git_binary ^ " " ^
  let open Command in
  match params.command with
   | Version -> "--version"
   | Clone p ->
     let clone_string = "clone " ^ p.url in
     let branch_or_tag = some_or_blank ((^) " -b ") p.branch_or_tag in
     let target = some_or_blank ((^) " ") p.target in
     let single_branch = some_or_blank (fun b -> if not @@ single_branch_supported() then "" else 
       if b then " --single-branch" else " --no-single-branch") p.single_branch in
     clone_string ^ branch_or_tag ^ single_branch ^ target

and with_params params = let open Params in
  Shell.run @@ build_command params
   |> function
    | `Error _ as e -> e
    | `Ok s ->
    let open Command in
    match params.command with
    | Version -> `Ok (Response.Version (Response.Version.of_string s))
    | Clone p -> 
        match p.target with
      | Some t -> `Ok (Response.Directory t)
      | None -> `Ok (Response.Directory
        (FilePath.basename p.Command.url |> FilePath.chop_extension))

and run ?global_params command = with_params @@ Params.create ?global_params command

and version () = let open Params in
  match with_params @@ create Command.Version with
  | `Ok (Response.Version v) -> `Ok v
  | `Ok o -> `Error (false, "unexpected response from version command: " 
    ^ Response.show o)
  | `Error _ as e -> e

and single_branch_supported =
  let single_branch_version = Response.Version.of_string "1.9.0" in
  fun () -> match version() with 
  | `Ok v -> let cmp = Response.Version.compare single_branch_version v in cmp <= 0
  | _ -> default_single_branch_supported


let clone ?ssl_no_verify ?target ?branch_or_tag ?single_branch url =
  let open Params in
  match with_params @@ create ~global_params:(Globals.create ?ssl_no_verify ()) 
    (Command.(Clone {url;target;branch_or_tag;single_branch}))
  with
  | `Ok (Response.Directory dir) -> `Ok dir
  | `Ok o -> `Error (false, "unexpected response from clone: " ^ Response.show o)
  | `Error _ as e -> e



