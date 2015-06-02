open Shell
module Fun = CCFun

(* If git version fails, use this value *)
let default_single_branch_supported = true

module Params =
struct
  module Globals =
    struct 
      let default_git_binary = "git"
      type t = {git_binary:string; ssl_no_verify : bool option} [@@deriving show]
      let create ?(git_binary=default_git_binary) ?ssl_no_verify () =
        {git_binary;ssl_no_verify}
    end
  type global_params = Globals.t [@@deriving show]

  module Command =
    struct
     type clone_params = {url:string; target:string option; branch_or_tag:string option;
        single_branch:bool option} [@@deriving show]
     type ls_remote_params = {url:string;heads:bool option;tags:bool option} [@@deriving show]

     type t =  | Clone of clone_params 
               | Version 
               | Ls_remote of ls_remote_params
               [@@deriving show] 
    end
  type t = { global_params:global_params; command:Command.t } [@@deriving show]

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

    module Remote_ref = 
    struct
      type ref_type = [`Head | `Tag | `Pull | `Branch | `Unknown] [@@deriving show]
      type t = {commit_id:string;value:string;ref_type:ref_type} [@@deriving show]
      let concat = String.concat "/"
      let of_string s = 
        let pat = Re_posix.compile_pat "[\\w\t]" in
        match Re.split pat s with
        | commit_id::raw_value::_ -> 
            let ref_type, value = Re_posix.compile_pat "/" |> fun pat ->
              Re.split pat (String.trim raw_value) |> 
                function | "refs"::"heads"::rest -> `Branch, (concat rest)
                         | "refs"::"tags"::rest -> `Tag, (concat rest)
                         | "refs"::"pull"::rest -> `Pull, (concat rest)
                         | ["HEAD" as h] -> `Head, h
                         | rest -> `Unknown, (concat rest) in
             Some {commit_id;value;ref_type}
        | [] -> None
        | s::rest -> print_endline @@ "BAD PATTERN: " ^ s; None
      let read_in s = Re_posix.compile_pat "\n" |> fun re -> Re.split re s |> CCList.filter_map of_string

    end
      
    module Directory =
    struct
      type t = string [@@deriving show]
      let of_string t = t
      let to_string t = t

      let some_or_from_url o url =
       match o with
       | Some t -> t
       | None -> (FilePath.basename url |> FilePath.chop_extension)
    end

    type t = | Directory of Directory.t
             | Version of version
             | Remote_refs of Remote_ref.t list [@@deriving show]
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
   | Ls_remote {url;heads;tags} ->
       let ls_remote_string = "ls-remote " in
       let heads = some_or_blank (fun b -> if b then " -h " else "") heads in
       let tags = some_or_blank (fun b -> if b then " -t " else "") tags in
       ls_remote_string ^ heads ^ tags ^ url
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
    | Ls_remote _ -> `Ok (Response.Remote_refs (Response.Remote_ref.read_in s))
    | Clone p -> `Ok (Response.Directory (Response.Directory.some_or_from_url p.target p.url))

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


let ls_remote ?ssl_no_verify ?heads ?tags url =
let open Params in
  match with_params @@ create ~global_params:(Globals.create ?ssl_no_verify ()) 
  (Command.(Ls_remote {url;heads;tags}))
  with
  | `Ok (Response.Remote_refs r) -> `Ok r
  | `Ok o -> `Error (false, "unexpected response from ls-remote: " ^ Response.show o)
  | `Error _ as e -> e

let clone ?ssl_no_verify ?target ?branch_or_tag ?single_branch url =
  let open Params in
  match with_params @@ create ~global_params:(Globals.create ?ssl_no_verify ()) 
    (Command.(Clone {url;target;branch_or_tag;single_branch}))
  with
  | `Ok (Response.Directory dir) -> `Ok dir
  | `Ok o -> `Error (false, "unexpected response from clone: " ^ Response.show o)
  | `Error _ as e -> e



