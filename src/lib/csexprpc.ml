(*
 * Copyright (c) Cloud Software Group, Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Sexplib0.Sexp_conv

(** {1 Auto-generated conversions between {!module:Rpc} types and S-expressions} *)

(** {2 {!type:Rpc.t} conversion} *)

type t = Rpc.t =
  | Int of int64
  | Int32 of int32
  | Bool of bool
  | Float of float
  | String of string
  | DateTime of string
  | Enum of t list
  | Dict of (string * t) list
  | Base64 of string
  | Null
(* code below updated by [dune build @lint --auto-promote] *)
[@@deriving_inline sexp]

let _ = fun (_ : t) -> ()

let rec t_of_sexp =
  (let error_source__003_ = "src/lib/csexprpc.ml.t" in
   function
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("int" | "Int") as _tag__006_) :: sexp_args__007_) as
     _sexp__005_ ->
     (match sexp_args__007_ with
      | arg0__008_ :: [] ->
        let res0__009_ = int64_of_sexp arg0__008_ in
        Int res0__009_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__006_
          _sexp__005_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("int32" | "Int32") as _tag__011_) :: sexp_args__012_) as
     _sexp__010_ ->
     (match sexp_args__012_ with
      | arg0__013_ :: [] ->
        let res0__014_ = int32_of_sexp arg0__013_ in
        Int32 res0__014_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__011_
          _sexp__010_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("bool" | "Bool") as _tag__016_) :: sexp_args__017_) as
     _sexp__015_ ->
     (match sexp_args__017_ with
      | arg0__018_ :: [] ->
        let res0__019_ = bool_of_sexp arg0__018_ in
        Bool res0__019_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__016_
          _sexp__015_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("float" | "Float") as _tag__021_) :: sexp_args__022_) as
     _sexp__020_ ->
     (match sexp_args__022_ with
      | arg0__023_ :: [] ->
        let res0__024_ = float_of_sexp arg0__023_ in
        Float res0__024_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__021_
          _sexp__020_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("string" | "String") as _tag__026_) :: sexp_args__027_) as
     _sexp__025_ ->
     (match sexp_args__027_ with
      | arg0__028_ :: [] ->
        let res0__029_ = string_of_sexp arg0__028_ in
        String res0__029_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__026_
          _sexp__025_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("dateTime" | "DateTime") as _tag__031_) :: sexp_args__032_)
     as _sexp__030_ ->
     (match sexp_args__032_ with
      | arg0__033_ :: [] ->
        let res0__034_ = string_of_sexp arg0__033_ in
        DateTime res0__034_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__031_
          _sexp__030_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("enum" | "Enum") as _tag__036_) :: sexp_args__037_) as
     _sexp__035_ ->
     (match sexp_args__037_ with
      | arg0__038_ :: [] ->
        let res0__039_ = list_of_sexp t_of_sexp arg0__038_ in
        Enum res0__039_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__036_
          _sexp__035_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("dict" | "Dict") as _tag__041_) :: sexp_args__042_) as
     _sexp__040_ ->
     (match sexp_args__042_ with
      | arg0__048_ :: [] ->
        let res0__049_ =
          list_of_sexp
            (function
              | Sexplib0.Sexp.List [ arg0__043_; arg1__044_ ] ->
                let res0__045_ = string_of_sexp arg0__043_
                and res1__046_ = t_of_sexp arg1__044_ in
                res0__045_, res1__046_
              | sexp__047_ ->
                Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                  error_source__003_
                  2
                  sexp__047_)
            arg0__048_
        in
        Dict res0__049_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__041_
          _sexp__040_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("base64" | "Base64") as _tag__051_) :: sexp_args__052_) as
     _sexp__050_ ->
     (match sexp_args__052_ with
      | arg0__053_ :: [] ->
        let res0__054_ = string_of_sexp arg0__053_ in
        Base64 res0__054_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__051_
          _sexp__050_)
   | Sexplib0.Sexp.Atom ("null" | "Null") -> Null
   | Sexplib0.Sexp.Atom ("int" | "Int") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("int32" | "Int32") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("bool" | "Bool") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("float" | "Float") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("string" | "String") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("dateTime" | "DateTime") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("enum" | "Enum") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("dict" | "Dict") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("base64" | "Base64") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("null" | "Null") :: _) as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_no_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__002_ ->
     Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__003_ sexp__002_
   | Sexplib0.Sexp.List [] as sexp__002_ ->
     Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__003_ sexp__002_
   | sexp__002_ -> Sexplib0.Sexp_conv_error.unexpected_stag error_source__003_ sexp__002_
   : Sexplib0.Sexp.t -> t)


let _ = t_of_sexp

let rec sexp_of_t =
  (function
   | Int arg0__055_ ->
     let res0__056_ = sexp_of_int64 arg0__055_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Int"; res0__056_ ]
   | Int32 arg0__057_ ->
     let res0__058_ = sexp_of_int32 arg0__057_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Int32"; res0__058_ ]
   | Bool arg0__059_ ->
     let res0__060_ = sexp_of_bool arg0__059_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Bool"; res0__060_ ]
   | Float arg0__061_ ->
     let res0__062_ = sexp_of_float arg0__061_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Float"; res0__062_ ]
   | String arg0__063_ ->
     let res0__064_ = sexp_of_string arg0__063_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "String"; res0__064_ ]
   | DateTime arg0__065_ ->
     let res0__066_ = sexp_of_string arg0__065_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "DateTime"; res0__066_ ]
   | Enum arg0__067_ ->
     let res0__068_ = sexp_of_list sexp_of_t arg0__067_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Enum"; res0__068_ ]
   | Dict arg0__073_ ->
     let res0__074_ =
       sexp_of_list
         (fun (arg0__069_, arg1__070_) ->
           let res0__071_ = sexp_of_string arg0__069_
           and res1__072_ = sexp_of_t arg1__070_ in
           Sexplib0.Sexp.List [ res0__071_; res1__072_ ])
         arg0__073_
     in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Dict"; res0__074_ ]
   | Base64 arg0__075_ ->
     let res0__076_ = sexp_of_string arg0__075_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Base64"; res0__076_ ]
   | Null -> Sexplib0.Sexp.Atom "Null"
   : t -> Sexplib0.Sexp.t)


let _ = sexp_of_t

[@@@deriving.end]

(** {2 {!type:Rpc.call} conversion} *)
type call = Rpc.call =
  { name : string
  ; params : t list
  ; is_notification : bool
  }
(* code below updated by [dune build @lint --auto-promote] *)
[@@deriving_inline sexp]

let _ = fun (_ : call) -> ()

let call_of_sexp =
  (let error_source__094_ = "src/lib/csexprpc.ml.call" in
   function
   | Sexplib0.Sexp.List field_sexps__079_ as sexp__078_ ->
     let name__080_ = Stdlib.ref Stdlib.Option.None
     and params__082_ = Stdlib.ref Stdlib.Option.None
     and is_notification__084_ = Stdlib.ref Stdlib.Option.None
     and duplicates__086_ = Stdlib.ref []
     and extra__087_ = Stdlib.ref [] in
     let rec iter__095_ = function
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom field_name__088_ :: (([] | _ :: []) as _field_sexps__090_))
         :: tail__096_ ->
         let _field_sexp__089_ () =
           match _field_sexps__090_ with
           | x__097_ :: [] -> x__097_
           | [] ->
             Sexplib0.Sexp_conv_error.record_only_pairs_expected
               error_source__094_
               sexp__078_
           | _ -> assert false
         in
         (match field_name__088_ with
          | "name" ->
            (match Stdlib.( ! ) name__080_ with
             | Stdlib.Option.None ->
               let _field_sexp__089_ = _field_sexp__089_ () in
               let fvalue__093_ = string_of_sexp _field_sexp__089_ in
               Stdlib.( := ) name__080_ (Stdlib.Option.Some fvalue__093_)
             | Stdlib.Option.Some _ ->
               Stdlib.( := )
                 duplicates__086_
                 (field_name__088_ :: Stdlib.( ! ) duplicates__086_))
          | "params" ->
            (match Stdlib.( ! ) params__082_ with
             | Stdlib.Option.None ->
               let _field_sexp__089_ = _field_sexp__089_ () in
               let fvalue__092_ = list_of_sexp t_of_sexp _field_sexp__089_ in
               Stdlib.( := ) params__082_ (Stdlib.Option.Some fvalue__092_)
             | Stdlib.Option.Some _ ->
               Stdlib.( := )
                 duplicates__086_
                 (field_name__088_ :: Stdlib.( ! ) duplicates__086_))
          | "is_notification" ->
            (match Stdlib.( ! ) is_notification__084_ with
             | Stdlib.Option.None ->
               let _field_sexp__089_ = _field_sexp__089_ () in
               let fvalue__091_ = bool_of_sexp _field_sexp__089_ in
               Stdlib.( := ) is_notification__084_ (Stdlib.Option.Some fvalue__091_)
             | Stdlib.Option.Some _ ->
               Stdlib.( := )
                 duplicates__086_
                 (field_name__088_ :: Stdlib.( ! ) duplicates__086_))
          | _ ->
            if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
            then Stdlib.( := ) extra__087_ (field_name__088_ :: Stdlib.( ! ) extra__087_)
            else ());
         iter__095_ tail__096_
       | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__078_) :: _ ->
         Sexplib0.Sexp_conv_error.record_only_pairs_expected error_source__094_ sexp__078_
       | [] -> ()
     in
     iter__095_ field_sexps__079_;
     (match Stdlib.( ! ) duplicates__086_ with
      | _ :: _ ->
        Sexplib0.Sexp_conv_error.record_duplicate_fields
          error_source__094_
          (Stdlib.( ! ) duplicates__086_)
          sexp__078_
      | [] ->
        (match Stdlib.( ! ) extra__087_ with
         | _ :: _ ->
           Sexplib0.Sexp_conv_error.record_extra_fields
             error_source__094_
             (Stdlib.( ! ) extra__087_)
             sexp__078_
         | [] ->
           (match
              ( Stdlib.( ! ) name__080_
              , Stdlib.( ! ) params__082_
              , Stdlib.( ! ) is_notification__084_ )
            with
            | ( Stdlib.Option.Some name__081_
              , Stdlib.Option.Some params__083_
              , Stdlib.Option.Some is_notification__085_ ) ->
              { name = name__081_
              ; params = params__083_
              ; is_notification = is_notification__085_
              }
            | _ ->
              Sexplib0.Sexp_conv_error.record_undefined_elements
                error_source__094_
                sexp__078_
                [ ( Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) name__080_) Stdlib.Option.None
                  , "name" )
                ; ( Sexplib0.Sexp_conv.( = ) (Stdlib.( ! ) params__082_) Stdlib.Option.None
                  , "params" )
                ; ( Sexplib0.Sexp_conv.( = )
                      (Stdlib.( ! ) is_notification__084_)
                      Stdlib.Option.None
                  , "is_notification" )
                ])))
   | Sexplib0.Sexp.Atom _ as sexp__078_ ->
     Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__094_ sexp__078_
   : Sexplib0.Sexp.t -> call)


let _ = call_of_sexp

let sexp_of_call =
  (fun { name = name__099_
       ; params = params__101_
       ; is_notification = is_notification__103_
       } ->
     let bnds__098_ = [] in
     let bnds__098_ =
       let arg__104_ = sexp_of_bool is_notification__103_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "is_notification"; arg__104_ ]
       :: bnds__098_
     in
     let bnds__098_ =
       let arg__102_ = sexp_of_list sexp_of_t params__101_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "params"; arg__102_ ] :: bnds__098_
     in
     let bnds__098_ =
       let arg__100_ = sexp_of_string name__099_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "name"; arg__100_ ] :: bnds__098_
     in
     Sexplib0.Sexp.List bnds__098_
   : call -> Sexplib0.Sexp.t)


let _ = sexp_of_call

[@@@deriving.end]

(** {2 {!type:Rpc.response} conversion} *)
type response = Rpc.response =
  { success : bool
  ; contents : t
  ; is_notification : bool
  }
[@@deriving_inline sexp]

let _ = fun (_ : response) -> ()

let response_of_sexp =
  (let error_source__122_ = "src/lib/csexprpc.ml.response" in
   function
   | Sexplib0.Sexp.List field_sexps__107_ as sexp__106_ ->
     let success__108_ = Stdlib.ref Stdlib.Option.None
     and contents__110_ = Stdlib.ref Stdlib.Option.None
     and is_notification__112_ = Stdlib.ref Stdlib.Option.None
     and duplicates__114_ = Stdlib.ref []
     and extra__115_ = Stdlib.ref [] in
     let rec iter__123_ = function
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom field_name__116_ :: (([] | _ :: []) as _field_sexps__118_))
         :: tail__124_ ->
         let _field_sexp__117_ () =
           match _field_sexps__118_ with
           | x__125_ :: [] -> x__125_
           | [] ->
             Sexplib0.Sexp_conv_error.record_only_pairs_expected
               error_source__122_
               sexp__106_
           | _ -> assert false
         in
         (match field_name__116_ with
          | "success" ->
            (match Stdlib.( ! ) success__108_ with
             | Stdlib.Option.None ->
               let _field_sexp__117_ = _field_sexp__117_ () in
               let fvalue__121_ = bool_of_sexp _field_sexp__117_ in
               Stdlib.( := ) success__108_ (Stdlib.Option.Some fvalue__121_)
             | Stdlib.Option.Some _ ->
               Stdlib.( := )
                 duplicates__114_
                 (field_name__116_ :: Stdlib.( ! ) duplicates__114_))
          | "contents" ->
            (match Stdlib.( ! ) contents__110_ with
             | Stdlib.Option.None ->
               let _field_sexp__117_ = _field_sexp__117_ () in
               let fvalue__120_ = t_of_sexp _field_sexp__117_ in
               Stdlib.( := ) contents__110_ (Stdlib.Option.Some fvalue__120_)
             | Stdlib.Option.Some _ ->
               Stdlib.( := )
                 duplicates__114_
                 (field_name__116_ :: Stdlib.( ! ) duplicates__114_))
          | "is_notification" ->
            (match Stdlib.( ! ) is_notification__112_ with
             | Stdlib.Option.None ->
               let _field_sexp__117_ = _field_sexp__117_ () in
               let fvalue__119_ = bool_of_sexp _field_sexp__117_ in
               Stdlib.( := ) is_notification__112_ (Stdlib.Option.Some fvalue__119_)
             | Stdlib.Option.Some _ ->
               Stdlib.( := )
                 duplicates__114_
                 (field_name__116_ :: Stdlib.( ! ) duplicates__114_))
          | _ ->
            if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
            then Stdlib.( := ) extra__115_ (field_name__116_ :: Stdlib.( ! ) extra__115_)
            else ());
         iter__123_ tail__124_
       | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp__106_) :: _ ->
         Sexplib0.Sexp_conv_error.record_only_pairs_expected error_source__122_ sexp__106_
       | [] -> ()
     in
     iter__123_ field_sexps__107_;
     (match Stdlib.( ! ) duplicates__114_ with
      | _ :: _ ->
        Sexplib0.Sexp_conv_error.record_duplicate_fields
          error_source__122_
          (Stdlib.( ! ) duplicates__114_)
          sexp__106_
      | [] ->
        (match Stdlib.( ! ) extra__115_ with
         | _ :: _ ->
           Sexplib0.Sexp_conv_error.record_extra_fields
             error_source__122_
             (Stdlib.( ! ) extra__115_)
             sexp__106_
         | [] ->
           (match
              ( Stdlib.( ! ) success__108_
              , Stdlib.( ! ) contents__110_
              , Stdlib.( ! ) is_notification__112_ )
            with
            | ( Stdlib.Option.Some success__109_
              , Stdlib.Option.Some contents__111_
              , Stdlib.Option.Some is_notification__113_ ) ->
              { success = success__109_
              ; contents = contents__111_
              ; is_notification = is_notification__113_
              }
            | _ ->
              Sexplib0.Sexp_conv_error.record_undefined_elements
                error_source__122_
                sexp__106_
                [ ( Sexplib0.Sexp_conv.( = )
                      (Stdlib.( ! ) success__108_)
                      Stdlib.Option.None
                  , "success" )
                ; ( Sexplib0.Sexp_conv.( = )
                      (Stdlib.( ! ) contents__110_)
                      Stdlib.Option.None
                  , "contents" )
                ; ( Sexplib0.Sexp_conv.( = )
                      (Stdlib.( ! ) is_notification__112_)
                      Stdlib.Option.None
                  , "is_notification" )
                ])))
   | Sexplib0.Sexp.Atom _ as sexp__106_ ->
     Sexplib0.Sexp_conv_error.record_list_instead_atom error_source__122_ sexp__106_
   : Sexplib0.Sexp.t -> response)


let _ = response_of_sexp

let sexp_of_response =
  (fun { success = success__127_
       ; contents = contents__129_
       ; is_notification = is_notification__131_
       } ->
     let bnds__126_ = [] in
     let bnds__126_ =
       let arg__132_ = sexp_of_bool is_notification__131_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "is_notification"; arg__132_ ]
       :: bnds__126_
     in
     let bnds__126_ =
       let arg__130_ = sexp_of_t contents__129_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "contents"; arg__130_ ] :: bnds__126_
     in
     let bnds__126_ =
       let arg__128_ = sexp_of_bool success__127_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "success"; arg__128_ ] :: bnds__126_
     in
     Sexplib0.Sexp.List bnds__126_
   : response -> Sexplib0.Sexp.t)


let _ = sexp_of_response

[@@@deriving.end]

(** {2 exception converter} *)

exception
  Parse_error of
    { offset : int
    ; message : string
    ; input : string
    }
[@@deriving_inline sexp_of]

let () =
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Parse_error] (function
    | Parse_error { offset = offset__134_; message = message__136_; input = input__138_ }
      ->
      let bnds__133_ = [] in
      let bnds__133_ =
        let arg__139_ = sexp_of_string input__138_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "input"; arg__139_ ] :: bnds__133_
      in
      let bnds__133_ =
        let arg__137_ = sexp_of_string message__136_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "message"; arg__137_ ] :: bnds__133_
      in
      let bnds__133_ =
        let arg__135_ = sexp_of_int offset__134_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "offset"; arg__135_ ] :: bnds__133_
      in
      Sexplib0.Sexp.List
        (Sexplib0.Sexp.Atom "src/lib/csexprpc.ml.Parse_error" :: bnds__133_)
    | _ -> assert false)


[@@@deriving.end]

(** {1 Convert S-expressions to and from strings using {!module:Csexp}, an efficient, yet still human-readable encoding.} *)

module Wire = Csexp.Make (Sexplib0.Sexp)

let wire_of_sexp = Wire.to_string

let sexp_of_wire input =
  match Wire.parse_string input with
  | Ok r -> r
  | Error (offset, message) -> raise (Parse_error { offset; message; input })


(** {1 Convert {!module:Rpc} types to/from string} *)

let to_string t = t |> sexp_of_t |> wire_of_sexp
let of_string s = s |> sexp_of_wire |> t_of_sexp
let string_of_call call = call |> sexp_of_call |> wire_of_sexp
let call_of_string s = s |> sexp_of_wire |> call_of_sexp
let string_of_response response = response |> sexp_of_response |> wire_of_sexp
let response_of_string s = s |> sexp_of_wire |> response_of_sexp
