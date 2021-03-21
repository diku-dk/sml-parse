signature SIMPLE_TOKEN = sig

  type reg = Region.reg

  datatype token = Symb of string
                 | Id of string
                 | Num of string

  val pp_token : token -> string

  val tokenise : {sep_chars         : string,                  (* single-char symbols *)
                  symb_chars        : string,                  (* multi-char symbols *)
                  is_id             : string -> bool,          (* is a string an id? *)
                  is_num            : string -> bool}          (* is a string a number? *)
                 -> {srcname:string,input:string}
                 -> (token*reg) list
end
