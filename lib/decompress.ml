module Extension = 
struct type t = [`gz | `bz2 | `zip] [@@deriving show] 

let of_string s = String.lowercase s |> function 
  | str when show `gz = str -> `Ok `gz
  | str when show `bz2 = str -> `Ok `bz2
  | str when show `zip = str -> `Ok `zip
  | str -> `Error (false, "unsupported extension: " ^ str)

let to_string = show
end

let print s = print_endline @@ Printf.sprintf "[decompress] %s" s

let cmd_for filename = let open Shell_utils.Infix in 
  Extension.of_string filename >>| function
  | `gz -> "gzip -f -d " ^ filename
  | `bz2 -> "bzip2 -f -d " ^ filename
  | `zip -> "unzip -o " ^ filename

let _clean_maybe ~clean filename =
  if not clean then `Ok ("skipped clean for: " ^ filename) else
    begin
      let chopped = FilePath.chop_extension filename in 
      try
        FileUtil.rm 
          ~recurse:true 
          ~force:FileUtil.Force 
          [chopped; FilePath.chop_extension chopped]; 
        `Ok filename
      with e -> `Error (true, "failed to clean uncompressed targets of " ^ filename)
    end

let _decompress filename =
  let open Shell_utils.Infix in
  cmd_for filename >>=
  Shell_utils.system >>|
  fun res -> ignore(res); FilePath.chop_extension filename

let run ?(clean=false) filename =
  let open Shell_utils.Infix in
  begin 
    match _clean_maybe ~clean filename with
    | `Ok _ as ok -> ok
    | `Error (true, err) -> `Ok ("failed, but continuing: " ^ err)
    | `Error _ as e -> e end >>= fun res ->
  print @@ "clean result: " ^ res;
  _decompress filename

