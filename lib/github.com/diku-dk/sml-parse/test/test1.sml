
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

infix >>=

val p_int : int p =
    next >>= (fn T.Num n => (case Int.fromString n of
                                 SOME n => accept n
                               | NONE => reject "expecting int")
               | _ => reject "expecting int")

fun qq s = "'" ^ s ^ "'"

fun p_kw (s:string) : unit p =
    next >>= (fn T.Id k =>
                 if k = s then accept ()
                 else reject ("expecting keyword " ^ qq s)
               | _ => reject ("expecting keyword " ^ qq s))

val p_var : string p =
    next >>= (fn T.Id k => accept k | _ => reject "expecting variable")

fun p_symb (s:string) : unit p =
    next >>= (fn T.Symb k =>
                 if k = s then accept ()
                 else reject ("expecting symbol " ^ qq s)
               | _ => reject ("expecting symbol " ^ qq s))

infixr <|> *> <*
infix >>> ?? <*> <$> <$$>

fun delay0 p = delay p ()

fun p_e () : e p =
    (choice [(fn (v,(e1,e2)) => Let(v,e1,e2)) <$> ((p_kw "let" *> p_var) >>> ((p_symb "=" *> delay p_e ()) >>> (p_kw "in" *> delay0 p_e))),
             Var <$> p_var,
             Int <$> p_int] ?? delay0 p_next) (fn (e,f) => f e)

and p_next () : (e -> e) p =
    (fn e2 => fn e1 => Add(e1,e2)) <$> (p_symb "+" *> delay0 p_e)

val res = case parse (delay p_e ()) ts of
              OK e => Int.toString(eval nil e)
            | NO(r,f) => f()

val i = print ("Eval = " ^ res ^ "\n")
