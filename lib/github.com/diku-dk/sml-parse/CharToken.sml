(** Char tokenisation library.

The tokenisation library is very basic and considers every char a token. The
library does not treat white space except that it returns region information
that respects the location in the input (e.g., newline characters has influence
on the returned region information).

*)

structure CharToken :> CHAR_TOKEN = struct

type reg = Region.reg
type loc = Region.loc

type token = char

fun pp_token t = Char.toString t

fun tokenise {srcname:string, input:string} : (token*reg) list =
    let fun next (c, (l:loc,ts:(token*reg)list)) : loc * (token*reg)list =
            let val l' = if c = #"\n" then Region.newline l
                         else if Char.isPrint c then Region.next l
                         else l
            in (l', (c,(l,l))::ts)
            end
        val (_,ts) = CharVector.foldl next (Region.loc0 srcname, nil) input
    in rev ts
    end

end
