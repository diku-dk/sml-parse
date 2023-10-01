(** Utilities for basis library scan functionality.

This utility library features combinators for assembling scanners
(parsers) from more basic scanners. The library is designed to work
well together with the scanner functionality provided in the Standard
ML Basis Library (i.e., StringCvt.reader).

A scanner (or parser) of type `('a,'st) p` parses values of type
`'a`. Such a parser is a function that takes a char-reader (of type
`(char,'st) reader`, and returns a reader that reads values of type
`'a`.

When opening ScanUtil, include the following infix-declaration:

    infix >>> ->> >>- >>? || >>@ >>* ??

*)

signature SCAN_UTIL =
sig
  type ('a,'st) reader = ('a,'st) StringCvt.reader
  type ('a,'st) p = (char,'st) reader -> ('a,'st) reader

  val >>> : ('a,'st) p * ('b,'st) p -> ('a * 'b,'st) p
  val >>@ : ('a,'st) p * ('a -> 'b) -> ('b,'st) p
  val ||  : ('a,'st) p * ('a,'st) p -> ('a,'st) p
  val ->> : ('a,'st) p * ('b,'st) p -> ('b,'st) p
  val >>- : ('a,'st) p * ('b,'st) p -> ('a,'st) p
  val >>? : ('a,'st) p * ('b,'st) p -> ('a * 'b -> 'a) -> ('a,'st) p
  val >>* : ('a,'st) p * ('b,'st) p -> ('a * 'b -> 'a) -> ('a,'st) p
  val ??  : ('a,'st) p * ('a -> 'b option) -> ('b,'st) p

  val ign : ('a,'st) p -> (unit,'st) p
  val con : string * 'a -> ('a,'st) p
  val str : string -> (string,'st) p
  val eos : (unit,'st) p

  val scanChar  : (char -> bool) -> (char,'st) p
  val scanChars : (char -> bool) -> (string,'st) p

  val list      : ('a,'st) p -> ('a list,'st) p
  val option    : ('a,'st) p -> ('a option,'st) p

  val skipChars : (char -> bool) -> ('a,'st) p -> ('a,'st) p
  val skipWS    : ('a,'st) p -> ('a,'st) p
  val noSkipWS  : ('a,'st) p -> ('a,'st) p

  val remainder : (string,'st) p

  val scanId    : (string,'st) p
end

(**

[type ('a,'st) reader] The generic reader type, which is a type
abbreviation for the type `'st -> ('a * 'st) option`.

[a >>> b] returns a scanner that first scans using `a`, then scans
using `b`, and finally returns the pair of the two scan results.

[a ->> b] returns a scanner that first scans using `a`, then scans
using `b`, and finally returns the second scan result.

[a >>- b] returns a scanner that first scans using `a`, then scans
using `b`, and finally returns the first scan result.

[(a >>? b) f] first scans using `a` and maybe continues using `b`, if
both succeeds, combines the scan results using `f`.

[(a >>* b) f] first scans using `a` followed by scanning zero or more
times using `b`, while iteratively combining successful scan results
using `f`.

[a || b] returns a scanner that first attempts to scan using `a`. On
success, the result is returned. Otherwise, returns the result of
scanning using `b`.

[p >>@ f] returns a scanner that scans using `p` and applies `f` to the
result on success.

[ign p] returns a scanner that discards the result of scanning using
`p`.

[p ?? f] returns a scanner that scans using `p` and applies `f` on the
result, which will either succeed or fail.

[eos] a scanner that accepts only the empty stream.

[skipWS p] returns a scanner that will skip initial white space before
scanning using `p`.

[noSkipWS p] returns a scanner that does not skip initial white space
when scanning using `p`.

[str s] returns a scanner that will accept only the initial string `s`
and returns the string value `s`.

[con (s,v)] returns a scanner that will accept only the initial string
`s` and which returns the value `v`.

[scanChar P] returns a scanner that accepts a character if it
satisfies the predicate `P`.

[scanChars P] returns a scanner that accepts a string of characters
for which each character satisfies the predicate `P`.

[skipChars P p] returns a scanner identical to `p` but that skips
initial characters that satisfy the predicate `P`.

[list p] returns a scanner that constructs a list of values by
repeatedly scanning using p until p does not succeed.

[option p] returns a scanner that constructs an option value by trying
to scan using p.

[remainder] scans all remaining characters (until end-of-stream). For
large input streams, it may be more efficient to apply a
state-specific extractor.

[scanId] returns a scanner that scans identifiers consisting of an
initial alphabetic character followed by zero or more alpha-numerical
characters.

*)
