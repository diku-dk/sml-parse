(** Simple parser combinator library.

This monadic parser combinator library provides a series of combinators for
assembling parsers. The library also keeps track of position information, which
makes it ideal for providing good error messages.

*)

signature PARSE = sig
  type token
  datatype ('a,'b) either = OK of 'a | NO of 'b
  type locerr = Region.loc * (unit -> string)
  type tokenstream = (token*Region.reg) list
  type 'a p

  val parse   : 'a p -> tokenstream -> ('a, locerr) either
  val parse'  : 'a p -> tokenstream -> ('a * tokenstream, locerr) either
  val accept  : 'a -> 'a p
  val next    : token p
  val reject  : string -> 'a p
  val delay   : ('a -> 'b p) -> 'a -> 'b p
  val eof     : unit p
  val ign     : 'a p -> unit p
  val eat     : token -> unit p
  val satisfy : (token -> bool) -> token p
  val many    : 'a p -> 'a list p
  val some    : 'a p -> 'a list p
  val enclose : 'a p -> 'b p -> 'c p -> 'c p
  val choice  : 'a p list -> 'a p

  val >>=     : 'a p * ('a -> 'b p) -> 'b p
  val <|>     : 'a p * 'a p -> 'a p
  val <*>     : ('a -> 'b) p * 'a p -> 'b p
  val <*      : 'a p * 'b p -> 'a p
  val *>      : 'a p * 'b p -> 'b p
  val <$>     : ('a -> 'b) * 'a p -> 'b p
  val <$$>    : ('a * Region.reg -> 'b) * 'a p  -> 'b p

  val >>> : 'a p * 'b p -> ('a*'b) p
  val ??  : 'a p * 'b p -> ('a * 'b -> 'a) -> 'a p
  val ??? : 'a p * 'b p -> ('a * 'b * Region.reg -> 'a) -> 'a p
  val ??* : 'a p * 'b p -> ('a * 'b -> 'a) -> 'a p
end

(**

[token] type of tokens.

['a p] type of parsers that parse values of type `'a`.

[p >>= f] returns a parser that first parses using `p` and then uses the result
of the parse as an argument to `f`, thereby producing a new parser.

[accept v] returns a parser that returns the value `v`.

[reject s] returns a parser that always fails with the error message `s`.

[a >>> b] returns a parser that first parses using `a`, then parses
using `b`, and finally returns the pair of the two parse results.

[a <*> b] returns a parser that first parses using `a`, then parses using `b`,
and finally returns the result obtained by applying the function returned by the
first parser to the value obtained by the second parser.

[a *> b] returns a parser that first parses using `a`, then parses using `b`,
and finally returns the second parse result.

[a <* b] returns a parser that first parses using `a`, then parses using `b`,
and finally returns the first parse result.

[(a ?? b) f] first parses using `a` and maybe continues using `b`, if both
succeeds, combines the parse results using `f`.

[(a ??? b) f] is defined as `??`, but allows for passing region information to
`f`.

[(a ??* b) f] first parses using `a` followed by parsing zero or more times
using `b`, while iteratively combining successful parse results using `f`.

[a <|> b] returns a parser that first attempts to parse using `a`. On success,
the result is returned. Otherwise, returns the result of parsing using `b`.

[f <$> p] returns a parser that parses using `p` and applies `f` to the result
on success.

[ign p] discard the result of a parser `p`.

[eat t ts] "eat" one token `t` from list `ts`.

[f <$$> p] is defined as `<$>`, but allows for passing region information to
`f`.

*)
