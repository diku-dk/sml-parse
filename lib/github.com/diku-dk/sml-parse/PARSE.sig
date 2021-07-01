(** Simple parser combinator library that keeps track of position
    information.
*)

signature PARSE = sig
  type token
  datatype ('a,'b) either = OK of 'a | NO of 'b
  type locerr = Region.loc * (unit -> string)
  type 'a p = (token*Region.reg)list -> ('a * Region.reg * (token*Region.reg)list, locerr) either

  val >>> : 'a p * 'b p -> ('a*'b)p
  val ->> : unit p * 'b p -> 'b p
  val >>- : 'a p * unit p -> 'a p
  val ??  : 'a p * 'b p -> ('a*'b -> 'a) -> 'a p
  val ??? : 'a p * 'b p -> ('a*'b*Region.reg -> 'a) -> 'a p
  val ??* : 'a p * 'b p -> ('a*'b -> 'a) -> 'a p
  val ||  : 'a p * 'a p -> 'a p
  val oo  : 'a p * ('a -> 'b) -> 'b p
  val ign : 'a p -> unit p
  val eat : token -> unit p
  val oor : 'a p * ('a*Region.reg -> 'b) -> 'b p
end

(**

[token] type of tokens.

['a p] type of parsers that parse values of type `'a`.

[a >>> b] returns a parser that first parses using `a`, then parses
using `b`, and finally returns the pair of the two parse results.

[a ->> b] returns a parser that first parses using `a`, then parses
using `b`, and finally returns the second parse result.

[a >>- b] returns a parser that first parses using `a`, then parses
using `b`, and finally returns the first parse result.

[(a ?? b) f] first parses using `a` and maybe continues using `b`, if
both succeeds, combines the parse results using `f`.

[(a ??? b) f] is defined as `??`, but allows for passing region
information to `f`.

[(a ??* b) f] first parses using `a` followed by parsing zero or more
times using `b`, while iteratively combining successful parse results
using `f`.

[a || b] returns a parser that first attempts to parse using `a`. On
success, the result is returned. Otherwise, returns the result of
parsing using `b`.

[p oo f] returns a parser that parses using `p` and applies `f` to the
result on success.

[ign p] discard the result of a parser `p`.

[eat t ts] "eat" one token `t` from list `ts`.

[p oor f] is defined as `oo`, but allows for passing region
information to `f`.

*)
