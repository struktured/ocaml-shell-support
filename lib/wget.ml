module Params =
struct
  type t = {no_check_certificate:bool option;
            output_documents:string option;
            verbose:bool option;
            url:string}

  let create ?no_check_certificate ?output_documents ?verbose
      url = {no_check_certificate;output_documents;verbose;url}
end

let build_command params = let open Params in
  let maybe_or_blank f t = CCOpt.maybe f "" t in
  "wget" ^ 
  maybe_or_blank ((^) " -O ")  params.output_documents ^
  maybe_or_blank (fun _ -> " -v") params.verbose ^
  maybe_or_blank (fun _ -> " --no-check-certificate") params.no_check_certificate ^
  " " ^
  params.url

let with_params params =
  Shell_utils.run @@ build_command params
 
let run ?no_check_certificate ?output_documents ?verbose url =
  Params.create ?no_check_certificate ?output_documents ?verbose url
  |> with_params


