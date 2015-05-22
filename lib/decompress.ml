let decompress_gz filename =
  let open Shell_utils.Infix in
  Shell_utils.system @@ "gzip -f -d " ^ filename >>|
  fun res -> ignore(res); FilePath.chop_extension filename

let decompress_bz2 filename =
  let open Shell_utils.Infix in
  Shell_utils.system @@ "bzip2 -f -d " ^ filename >>|
  fun res -> ignore(res); FilePath.chop_extension filename

let decompress_zip filename =
  let open Shell_utils.Infix in
  Shell_utils.system @@ "unzip -o " ^ filename >>|
  fun res -> ignore(res); FilePath.chop_extension filename

let decompress filename =
  match FilePath.get_extension filename with
  | "gz" -> decompress_gz filename
  | "bz2" -> decompress_bz2 filename
  | "zip" -> decompress_zip filename
  | s -> `Error (false, "cannot handle extension: " ^ s)
