let field = Printf.sprintf "field_%s_%d"
let field_string i = field "str" i, Rpc.String (Printf.sprintf "short %d" i)
let field_int i = field "i" i, Rpc.Int (Int64.of_int i)
let field_bool i = field "b" i, Rpc.Bool (i mod 2 = 0)
let field_dict i = field "d" i, Rpc.Dict (List.init 5 field_string)
let large_dict_char i = Char.code 'A' + (i mod 32) |> Char.chr

let field_large_dict i =
  field "ld" i, Rpc.Dict [ "large", Rpc.String (String.init 32768 large_dict_char) ]


let field_enum i =
  field "e" i, Rpc.Enum (List.init 5 @@ fun i -> Rpc.String (Printf.sprintf "enum%d" i))


let field_float i = field "f" i, Rpc.Float Float.pi

let rpc =
  (* approximately matches the mix of field types in XAPI VM *)
  Rpc.Dict
    (List.flatten
       [ List.init 29 field_string
       ; List.init 1 field_float
       ; List.init 9 field_bool
       ; List.init 10 field_int
       ; List.init 6 field_dict
       ; List.init 1 field_large_dict
       ; List.init 15 field_enum
       ])


let bench_protocol name of_string to_string =
  let serialized = to_string rpc in
  let open Bechamel in
  let encode () = to_string rpc
  and decode () = of_string serialized in
  Test.make ~name Staged.(stage encode), Test.make ~name Staged.(stage decode)


let sexplib_of_string s = s |> Sexplib.Sexp.of_string |> Csexprpc.t_of_sexp
let sexplib_to_string s = s |> Csexprpc.sexp_of_t |> Sexplib.Sexp.to_string_mach

let tests =
  let xml1, xml2 = bench_protocol "xmlrpc" Xmlrpc.of_string Xmlrpc.to_string
  and json1, json2 = bench_protocol "jsonrpc" Jsonrpc.of_string Jsonrpc.to_string
  and csexp1, csexp2 = bench_protocol "csexp" Csexprpc.of_string Csexprpc.to_string
  and sexp1, sexp2 = bench_protocol "sexplib" sexplib_of_string sexplib_to_string in
  Bechamel.Test.make_grouped
    ~name:"rpc"
    [ Bechamel.Test.make_grouped ~name:"to_string" [ xml1; json1; csexp1; sexp1 ]
    ; Bechamel.Test.make_grouped ~name:"of_string" [ xml2; json2; csexp2; sexp2 ]
    ]


let () = Bechamel_simple_cli.cli tests
