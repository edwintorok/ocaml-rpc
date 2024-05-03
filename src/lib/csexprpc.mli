exception
  Parse_error of
    { offset : int
    ; message : string
    ; input : string
    }

val to_string : Rpc.t -> string
val of_string : string -> Rpc.t

val string_of_call : Rpc.call -> string
val call_of_string : string -> Rpc.call

val string_of_response : Rpc.response -> string
val response_of_string : string -> Rpc.response
