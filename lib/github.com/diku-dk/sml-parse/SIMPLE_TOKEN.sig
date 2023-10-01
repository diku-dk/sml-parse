(** Simple tokenisation library.

The tokenisation library has basic support for symbols, identifiers,
and numbers. It also associates region information to each individual
token and thereby provides good support for error handling.

*)

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

(**

[type reg] The region type specifying location information in the source.

[type token] The token type.

[pp_token t] returns a string representing the token t.

[tokenise {sep_chars,symb_chars,is_id,is_num} src] returns a list of
tokens paired with region information. Characters in sep_chars are
those that should be considered single symbols (e.g., parentheses),
whereas those in symb_chars are those that may be used to form
combined symbols (e.g., & for &&). The functions is_id and is_num are
meant to identify whether a string is an identifier or a number,
respectively. The argument src specifies the input source.

*)
