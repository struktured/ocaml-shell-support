open Cmdliner

let home () = Unix.getenv "HOME"
let scripts_dir_name = "bin"

let opam_system_root () = try Unix.getenv "OPAM_SYSTEM_ROOT" with _ ->
  let ($) = FilePath.concat in
  home() $ "local"

let working_dir exclude =
  FilePath.dirname Sys.argv.(0) |> fun s ->
  match s with "." -> FilePath.parent_dir | _ ->
  match Re.split (Re_posix.compile_pat exclude) s with
    h::hs -> h
  | [] -> FilePath.current_dir

module Infix =
struct
  let (>>=) x f = match x with `Error _ as e -> e | `Ok o -> f o

  let (>>|) x f = match x with `Error _ as e -> e | `Ok o -> `Ok (f o)
end

let read_command_output f s =
  let ic = Unix.open_process_in s in
  (try
     while true do
       f (input_char ic)
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
    Unix.WEXITED 0 -> `Ok s
  | Unix.WEXITED r -> `Error (false, "non-zero return value: " ^ string_of_int r)
  | _ -> invalid_arg ("invalid command: " ^ s)

module Buffer = CCRingBuffer.Byte

let buf_to_string buf =
  let into = Bytes.create (Buffer.length buf) in
  let _ = Buffer.blit_into buf into 0 (Buffer.length buf) in
  Bytes.to_string into

let buf_size = 1024

let run s = let open Infix in
  let buf = Buffer.create buf_size in
    read_command_output (Buffer.push_back buf) s >>|
    fun _ -> buf_to_string buf

let run_exn s = let open Infix in
    let buf = Buffer.create buf_size in
  match read_command_output (Buffer.push_back buf) s with
  | `Ok _ -> buf_to_string buf
  | `Error (b, e) -> failwith(Printf.sprintf "error(%b): %s" b e)

let system cmd =
  match Sys.command cmd with
  | 0 -> `Ok cmd
  | ret -> `Error (false, Printf.sprintf "%s: nonzero exit status: %d" cmd ret)

let in_dir dir f =
  let olddir = Unix.getcwd () in
  try
    Unix.chdir dir; let res = f dir in Unix.chdir olddir; res
  with e -> Unix.chdir olddir;raise e

let ls () = system "ls"
let pwd () = Unix.getcwd ()
let chdir = Unix.chdir
let cp = FileUtil.cp
let mv = FileUtil.mv

let default_editor = "vi"
let editor () = try Unix.getenv "EDITOR" with _ ->
  try Unix.getenv "VISUAL" with _ -> default_editor

let e to_edit =
  system @@ (editor()) ^ " " ^ to_edit

let os_type =
  let open Infix in
  let result = begin
    run "uname -a" >>| String.lowercase >>| function
  | res when
    Re_posix.compile_pat "darwin" |>
    fun re -> Re.execp re res -> `Darwin
  | res when
    Re_posix.compile_pat "linux" |>
    fun re -> Re.execp re res -> `Linux
  | res when
    Re_posix.compile_pat "sunoS" |>
    fun re -> Re.execp re res -> `SunOS
  | res when
    Re_posix.compile_pat "mingw32" |>
    fun re -> Re.execp re res -> `MingW32
  | res when
    Re_posix.compile_pat "mingw64" |>
    fun re -> Re.execp re res -> `MingW64
  | res -> `UnknownOS
  end in match result with
  |  `Ok r -> r
  | `Error _  -> `UnknownOS

let lines_of_chan chan =
  let lines = ref [] in
  try
    while true; do
      lines := input_line chan :: !lines
     done; !lines
  with End_of_file ->
    List.rev !lines

let lines_of_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    lines := lines_of_chan chan;!lines
  with End_of_file -> close_in chan; !lines

let mkdir ?(raise_if_exists=false) dir = try
    Unix.mkdir dir 0o750 
  with Unix.Unix_error (Unix.EEXIST, _, _) as e ->
    if raise_if_exists then raise e else ()
