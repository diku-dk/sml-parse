
(** Source region information. *)

signature REGION = sig
  type srcname = string
  type loc = int * int * srcname
  type reg = loc * loc

  val botloc  : loc
  val loc0    : srcname -> loc (* line 1, char 1 *)
  val newline : loc -> loc
  val next    : loc -> loc
  val lt      : loc -> loc -> bool
  val wf      : reg -> bool
  val ppLoc   : loc -> string
  val pp      : reg -> string
  val plus    : string -> reg -> reg -> reg
end

(**

[botloc] represents "end of source".

[loc0 s] returns the first location of a source (line 1, char 1 of
source s).

[newline l] returns the location of the first position in the line
following that of `l`.

[next l] returns the location following `l` on the same line as `l`.

[lt a b] returns true iff location `a` is strictly before location
`b`.

[wf r] returns true if and only if the region `r` is well-formed.

[ppLoc l] returns a pretty-printed version of the location `l`.

[pp r] returns a pretty-printed version of the region `r`.

[plus s r1 r2] returns the region that is the result of merging region
`r1` and `r2`. The string `s` is used for error reporting.

*)
