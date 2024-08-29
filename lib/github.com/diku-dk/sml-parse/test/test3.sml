
structure T = CharToken

fun constituent c =
  Char.isAlphaNum c orelse c = #"'" orelse c = #"_"

fun isValidName s =
  List.all constituent (explode s)

fun checkValidName s =
  if isValidName s then ()
  else raise Fail ("\"" ^ s ^ "\" is an invalid SML identifier.")

fun intersperse y [] = []
  | intersperse y [x] = [x]
  | intersperse y (x :: xs) =
      x :: y :: intersperse y xs

fun punctuate c = concat o intersperse c

structure Parser = Parse(type token = T.token
                         val pp_token = T.pp_token)

local
  open Parser

  infixr >>= <|> *> <*
  infix <*> <$>

  fun char c =
      satisfy (fn c' => c = c')

  val space = many (satisfy (fn c => c = #" "))

  fun lexeme p = p <* space

  fun lChar c = lexeme (char c)

  fun lString s =
      let fun loop [] = accept ()
            | loop (c :: cs) = char c *> loop cs
      in lexeme (loop (explode s))
      end

  fun sepBy1 p sep =
      p >>=
        (fn x =>
            choice
                [ sep *> delay (sepBy1 p) sep >>= (fn xs => accept (x :: xs))
                , delay accept [x]
        ])

  fun sepBy p sep =
      choice [delay (sepBy1 p) sep, delay accept []]

  fun parens p =
      enclose (lChar #"(") (lChar #")") p

  fun braces p =
      enclose (lChar #"{") (lChar #"}") p

  fun constituent c = Char.isAlpha c

  local
    val firstChar = satisfy Char.isAlpha
    val secondChar = satisfy (fn c => Char.isAlphaNum c orelse c = #"'")
  in
    val lName = lexeme ((fn c => fn cs =>
                            implode (c :: cs)) <$> firstChar <*> many secondChar)
  end

  fun delay0 f = delay f ()

  (* Futhark type. *)
  datatype T =
    TVar of string
  | TArray of T
  | TTuple of T list
  | TRecord of (string * T) list

  fun pT () =
    choice
      [ TVar <$> lName
      , parens (delay0 pT)
      , TArray <$> (lString "[]" *> delay0 pT)
      , TTuple <$> parens (sepBy (delay0 pT) (lChar #","))
      , TRecord <$> braces (sepBy (delay0 pField) (lChar #","))
      ]
  and pField () =
    (fn v => fn t => (v, t)) <$> (lName <* lChar #":") <*> pT ()

  fun showT (TVar s) = s
    | showT (TArray t) = "[]" ^ showT t
    | showT (TTuple ts) =
        "(" ^ punctuate "," (map showT ts) ^ ")"
    | showT (TRecord ts) =
        "{" ^ punctuate "," (map (fn (v, t) => v ^ ":" ^ showT t) ts) ^ "}"

  (* Make a string a valid SML identifier, whatever it may presently be. *)
  fun escapeName name =
    let
      fun escape c =
        if constituent c then
          str c
        else
          case c of
            #"[" => "_LB_"
          | #"]" => "_RB_"
          | _ => "_"
      val name' = concat (map escape (explode name))
    in
      if name <> name' then "unrep_" ^ name' else name
    end

  fun showTSML (TVar s) = s
    | showTSML (TArray t) = "arr_" ^ showTSML t
    | showTSML (TTuple ts) =
        "tup" ^ Int.toString (length ts) ^ "_" ^ punctuate "_" (map showTSML ts)
    | showTSML (TRecord ts) =
        "rec" ^ Int.toString (length ts) ^ "_" ^ punctuate "_" (map #1 ts)

  fun println s = print (s ^ "\n")

  fun prTypeName s =
      let val ts = T.tokenise {srcname="<stdin>",input=s}
      in case parse ((space *> pT ()) <* eof) ts of
             OK x => println ("OK: " ^ showTSML x)
           | NO (loc,err) => println ("ERR at " ^ Region.ppLoc loc ^ ": " ^ err())
      end
in

val () = prTypeName "[][](i64,f32)"

val () = prTypeName "[][]i64"

(*val () = prTypeName (List.nth (CommandLine.arguments (), 0)) *)
end
