structure ScanUtil : SCAN_UTIL = struct

type ('a,'st) reader = 'st -> ('a * 'st) option
type ('a,'st) p = (char,'st) reader -> ('a,'st) reader

infix >>> ->> >>- >>? || >>@ >>* ??

fun (p1 >>> p2) get s =
    case p1 get s of
        SOME (a,s) => (case p2 get s of
                           SOME (b,s) => SOME((a,b),s)
                         | NONE => NONE)
      | NONE => NONE

fun (p >>@ f) get s =
    case p get s of
        SOME (a,s) => SOME (f a,s)
      | NONE => NONE

fun (p1 >>- p2) = (p1 >>> p2) >>@ (#1)
fun (p1 ->> p2) = (p1 >>> p2) >>@ (#2)

fun (p1 || p2) get s =
    case p1 get s of
        NONE => p2 get s
      | r => r

fun (p1 >>? p2) f get s =
    case p1 get s of
        NONE => NONE
      | SOME (a,s) => case p2 get s of
                          SOME (b,s) => SOME(f(a,b),s)
                        | NONE => SOME(a,s)

fun (p1 >>* p2) f get s =
    case p1 get s of
        SOME (a,s) =>
        let fun repeat (a,s) =
                case p2 get s of
                    SOME (b,s) => repeat (f(a,b), s)
                  | _ => SOME (a,s)
        in repeat (a,s)
        end
      | NONE => NONE

fun ign scan get s =
    case scan get s of
        SOME (_,s) => SOME((),s)
      | NONE => NONE

fun (p ?? f) get s =
    case p get s of
        NONE => NONE
      | SOME (v,s) => case f v of
                          SOME v' => SOME(v',s)
                        | NONE => NONE

fun con (str,con) get s0 =
    let fun loop (i,s) =
            if i >= size str then SOME(con,s)
            else case get s of
                     SOME (c,s) =>
                     if c = String.sub(str,i) then loop (i+1,s)
                     else NONE
                   | NONE => NONE
    in loop (0,s0)
    end

fun str s = con (s,s)

fun eos get s =
    case get s of
        NONE => SOME((),s)
      | SOME _ => NONE

fun skipChars P p get s0 =
    case get s0 of
        SOME(c,s) => if P c then skipChars P p get s
                     else p get s0
      | NONE => p get s0

fun scanChar P get s =
    case get s of
        SOME(c,s) => if P c then SOME(c,s)
                     else NONE
      | NONE => NONE

fun scanChars P =
    (scanChar P >>@ (fn c => [c]) >>* scanChar P) (fn (a,b) => b::a) >>@ (implode o rev)

fun noSkipWS p get s =
    case get s of
        SOME(c,_) => if Char.isSpace c then NONE
                     else p get s
      | NONE => NONE

fun skipWS p = skipChars Char.isSpace p

fun scanId get =
    (scanChar Char.isAlpha >>@ String.str >>?
     scanChars Char.isAlphaNum
    ) (op ^) get

end
