(** Char tokenisation library.

The tokenisation library is very basic and considers every char a token. The
library does not treat white space except that it returns region information
that respects the location in the input (e.g., newline characters has influence
on the returned region information).

*)

signature CHAR_TOKEN = sig

  type reg = Region.reg

  type token = char

  val pp_token : token -> string

  val tokenise : {srcname:string,input:string}
                 -> (token*reg) list
end

(**

[type reg] The region type specifying location information in the source.

[type token] The token type.

[pp_token t] returns a string representing the token t.

[tokenise src] returns a list of tokens paired with region information. The
argument src specifies the input source.

*)
