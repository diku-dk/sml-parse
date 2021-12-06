
open ScanUtil

infix >>> ->> >>- >>? || >>@ >>* ??

fun curry f a b = f (a,b)

type st = CharVectorSlice.slice

val scanNum = scanChars Char.isDigit

fun isSymb c = CharVector.exists (curry (op =) c) "#|+-~^?*:!%/='<>@"

fun isSep c = CharVector.exists (curry (op =) c) "(){}[],.;"

val scanSymb = scanChar isSymb >>@ String.str

val scanSep = scanChar isSep >>@ String.str

fun scanErr get s =
    case get s of
        SOME _ => raise Fail ("Error at position " ^
                              Int.toString (#2 (CharVectorSlice.base s)) ^
                              ": expecting token")

      | NONE => NONE

(* Example: Lexing only *)

fun lexing s =
    let
      val scanIdOrSymbOrSep =
          scanId || scanSymb || scanSep || scanNum || scanErr

      val scanToken = skipChars Char.isSpace scanIdOrSymbOrSep

      val scanTokens : (string list,st) p =
          (scanToken >>@ (fn s => [s]) >>*
           scanToken
          ) (fn (a,b) => b :: a)
          >>@ List.rev
          >>- eos

      val sl = CharVectorSlice.full s

      val () = print ("Lexing string:\n \"" ^ s ^ "\"\n")

    in
      (case scanTokens CharVectorSlice.getItem sl of
           SOME(ts,_) =>
           print ("Tokens: \n [" ^
                  String.concatWith ", " ts ^
                  "]\n")
         | NONE => raise Fail "lex failure"
      ) handle Fail msg => print ("ERROR: " ^ msg ^ "\n")
    end

val ex1 = "let y = 82 + 2 in let x = 454 in y + x + y (* ok *)    + 23"

val () = lexing ex1

val ex2 = "let y = 82 + 2 in let x = & 454 in y + x + y (* ok *)   + 23"

val () = lexing ex2

(* Example: Lexing and parsing *)

datatype e = Int of int | Let of string * e * e | Add of e * e | Var of string

fun look nil x = raise Fail ("eval: non-bound variable: " ^ x)
  | look ((k,v)::E) x = if k = x then v else look E x

type env = (string*int)list

fun eval (E:env) (e:e) : int =
    case e of
        Int d => d
      | Let(x,e1,e2) => eval ((x,eval E e1)::E) e2
      | Var x => look E x
      | Add(e1,e2) => eval E e1 + eval E e2

(* some utilities *)

val scanComment =
    ign (str "(*") ->>
    skipChars (fn c => c <> #"*") (ign (str "*)"))

val skipWS = fn p =>
    skipWS scanComment ->> skipWS p
 || skipWS p

fun p_kw s : (unit,st) p =   (* parse a keyword *)
    skipWS(ign (str s))

val p_int : (int,st) p =
    skipWS scanNum ?? Int.fromString

val p_comment : (unit,st) p =
    skipWS scanComment

val keywords = ["in", "let"]
val p_var : (string, st) p =
    skipWS scanId ?? (fn id => if List.exists (fn s => id=s) keywords then NONE
                               else SOME id)
fun parse s =
    let
      val rec p_e : (e,st) p =
       fn g =>
          (    (((((p_kw "let" ->> p_var) >>> ((p_kw "=" ->> p_e) >>> (p_kw "in" ->> p_e))) >>@ (fn (v,(e1,e2)) => Let(v,e1,e2))) >>? p_next) (fn (e,f) => f e))
            || (((p_var >>@ Var) >>? p_next) (fn (e,f) => f e))
            || (((p_int >>@ Int) >>? p_next) (fn (e,f) => f e))
            || skipWS scanErr
          ) g

      and p_next : (e -> e,st) p =
       fn g =>
          (    ((p_kw "+" ->> p_e) >>@ (fn e2 => fn e1 => Add(e1,e2)))
          ) g

      val sl = CharVectorSlice.full s

      val () = print ("Lexing string:\n \"" ^ s ^ "\"\n")
    in
      (case (p_e >>- skipWS eos) CharVectorSlice.getItem sl of
           SOME(e,_) =>
           print ("Value: " ^ Int.toString(eval nil e) ^ "\n")
         | NONE => raise Fail "parse failure"
      ) handle Fail msg => print ("ERROR: " ^ msg ^ "\n")
    end

val () = parse ex1

val () = parse ex2
