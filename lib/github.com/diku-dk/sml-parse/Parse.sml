functor Parse (eqtype token
               val pp_token : token -> string)
        :> PARSE where type token = token =
struct

type loc = Region.loc
type reg = Region.reg

type token = token

(* keep track of the max location - the longest parse *)
datatype ('a,'b) either = OK of 'a | NO of 'b

type locerr = loc * (unit -> string)

type tokenstream = (token*reg)list

type 'a p = tokenstream -> ('a * reg * tokenstream, locerr) either

fun parse' f ts =
    case f ts of
        OK(x,_,ts') => OK (x, ts')
      | NO l => NO l

fun parse f ts =
    case f ts of
        OK(x,_,_) => OK x
      | NO l => NO l

infixr >>= <|> *> <*
infix >>> ?? ??? ??* <*> <$> <$$>

fun p >>= f =
    fn ts =>
       case p ts of
           NO err => NO err
         | OK (v1,r1,ts1) =>
           case f v1 ts1 of
               NO err => NO err
             | OK (v2,r2,ts2) => OK(v2,Region.plus "bind" r1 r2,ts2)

val next : token p =
    fn ts =>
       case ts of
           (t,r)::ts' => OK (t,r,ts')
         | nil => NO (Region.botloc, fn () => "eos")

fun peek ((_,r)::_) = (case Region.unReg r of
                           SOME (l,_) => l
                         | NONE => Region.botloc)
  | peek nil = Region.botloc

fun accept x =
    fn ts => OK(x,Region.emp,ts)

fun reject (s:string) : 'a p =
    fn ts => NO(peek ts, fn () => s)

fun delay (fp: 'a -> 'b p) (x: 'a) : 'b p =
    fn ts => fp x ts

val eof : unit p =
    fn ts => if null ts then OK((),Region.emp,ts)
             else NO(peek ts, fn () => "expecting end of stream")

fun satisfy p =
    next >>= (fn c => if p c then accept c else reject "satisfy")

fun maxLocerr (l1:locerr) l2 =
    if Region.lt (#1 l1) (#1 l2) then l2
    else l1

fun p1 <*> p2 =
    p1 >>= (fn f => p2 >>= (fn x => accept (f x)))

fun p1 <* p2 =
    accept (fn x => fn _ => x) <*> p1 <*> p2

fun p1 *> p2 =
    accept (fn _ => fn x => x) <*> p1 <*> p2

fun p1 >>> p2 =
    p1 >>= (fn v1 => p2 >>= (fn v2 => accept (v1,v2)))

fun f <$> p =
    p >>= (fn x => accept (f x))

fun p1 ?? p2 = fn f => fn ts =>
    case p1 ts of
        OK(v1,r1,ts) =>
        (case p2 ts of
             OK(v2,r2,ts) => OK(f(v1,v2), Region.plus "??" r1 r2, ts)
           | _ => OK(v1,r1,ts))
      | NO l => NO l

fun p1 ??? p2 = fn f => fn ts =>
    case p1 ts of
        OK(v1,r1,ts) =>
        (case p2 ts of
             OK(v2,r2,ts) =>
             let val r = Region.plus "???" r1 r2
             in OK(f(v1,v2,r), r, ts)
             end
           | NO l => OK(v1,r1,ts))
      | NO l => NO l

fun p1 <|> p2 = fn ts =>
    case p1 ts of
        OK(v,r,ts) => OK(v,r,ts)
      | NO l1 => case p2 ts of
                    OK(v,r,ts) => OK(v,r,ts)
                  | NO l2 => NO (maxLocerr l1 l2)

fun p1 ??* p2 = fn f => fn ts =>
    case p1 ts of
        OK(v1,r1,ts) =>
        let fun repeat (v1,r1,ts) =
                case p2 ts of
                    OK(v2,r2,ts) => repeat (f(v1,v2), Region.plus "??*" r1 r2, ts)
                  | _ => OK(v1,r1,ts)
        in repeat (v1,r1,ts)
        end
      | NO l => NO l

fun f <$$> p = fn ts =>
    case p ts of
      OK(v,r,ts) => OK(f(v,r),r,ts)
    | NO l => NO l

fun ign (p:'a p) : unit p =
    (fn _ => ()) <$> p

fun many p =
    (p >>= (fn x => many p >>= (fn xs => accept (x :: xs)))) <|> delay accept []

fun some p =
    p >>= (fn x => many p >>= (fn xs => accept (x :: xs)))

fun enclose (lb:'a p) (rb:'b p) (p:'c p) : 'c p = lb *> p <* rb

fun choice [] = reject "choice"
  | choice (p :: ps) = p <|> delay choice ps

fun eat t ts =
    case ts of
        nil => NO (Region.botloc,fn() => ("expecting token " ^ pp_token t ^
                                          " but reached the end"))
      | (t',r:Region.reg)::ts' =>
        if t=t' then OK ((),r,ts')
        else NO (case Region.unReg r of
                     SOME (l,_) => l
                   | NONE => Region.botloc,
                 fn() => ("expecting token " ^ pp_token t ^
                          " but found token " ^ pp_token t'))
end
