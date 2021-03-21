
structure T = SimpleToken

fun is_id s =
    size s > 0 andalso
    let val c0 = String.sub(s,0)
    in Char.isAlpha c0 orelse c0 = #"_"
    end andalso CharVector.all (fn c => Char.isAlphaNum c orelse c = #"_") s

fun is_num s =
    size s > 0 andalso
    (s = "0" orelse
     let val c0 = String.sub(s,0)
     in (Char.isDigit c0 andalso c0 <> #"0")
     end andalso CharVector.all (fn c => Char.isDigit c) s)

fun tokens s =
    T.tokenise {sep_chars="(){}[],.;",
                symb_chars="#&|+-~^?*:!%/='<>@",
                is_id=is_id,
                is_num=is_num}
               {srcname="stdin",input=s}

fun lexing () =
    let val s = "let y = 82 + 2 in let x = 454\nin y + x + y (* ok *)\n+ 23"
        val ts = tokens s
        val () = print "Tokens:\n"
        val () = app (fn (t,r) => print (Region.pp r ^ ":" ^ T.pp_token t ^ ", ")) ts
        val () = print "\n\n"
    in ts
    end

val ts = lexing ()

structure P = Parse(type token = T.token
                    val pp_token = T.pp_token)

open P

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

fun locOfTs nil = Region.botloc
  | locOfTs ((_,(l,_))::_) = l

val p_int : int p =
 fn ts =>
    case ts of
        (T.Num n,r)::ts' =>
        (case Int.fromString n of
             SOME n => OK (n,r,ts')
           | NONE => NO(locOfTs ts, fn () => "int"))
      | _ => NO(locOfTs ts, fn () => "int")

val p_kw : string -> unit p =
 fn s => fn ts =>
    case ts of
        (T.Id k,r)::ts' =>
        if k = s then OK ((),r,ts')
        else NO(locOfTs ts, fn () => "kw")
      | _ => NO(locOfTs ts, fn () => "kw")

val p_var : string p =
 fn ts =>
    case ts of
        (T.Id k,r)::ts' => OK (k,r,ts')
      | _ => NO(locOfTs ts, fn () => "var")

val p_symb : string -> unit p =
 fn s => fn ts =>
    case ts of
        (T.Symb k,r)::ts' =>
        if k = s then OK ((),r,ts')
        else NO(locOfTs ts, fn () => "var")
      | _ => NO(locOfTs ts, fn () => "var")

infix >>> ->> >>- oo || ??

val rec p_e : e p =
 fn ts =>
  (    (((((p_kw "let" ->> p_var) >>> ((p_symb "=" ->> p_e) >>> (p_kw "in" ->> p_e))) oo (fn (v,(e1,e2)) => Let(v,e1,e2))) ?? p_next) (fn (e,f) => f e))
    || (((p_var oo Var) ?? p_next) (fn (e,f) => f e))
    || (((p_int oo Int) ?? p_next) (fn (e,f) => f e))
  ) ts

and p_next : (e -> e) p =
    fn ts =>
     (    ((p_symb "+" ->> p_e) oo (fn e2 => fn e1 => Add(e1,e2)))
     ) ts

val res = case p_e ts of
              OK(e,r,ts') =>
              (case ts' of
                   nil => Int.toString(eval nil e)
                 | _ => raise Fail ("Syntax error at location " ^ Region.ppLoc (#2 r)))
            | NO(r,f) => f()

val i = print ("Eval = " ^ res ^ "\n")
