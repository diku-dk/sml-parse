structure SimpleToken :> SIMPLE_TOKEN = struct

type reg = Region.reg
type loc = Region.loc

datatype token = Symb of string
               | Id of string
               | Num of string

fun pp_token t =
    case t of
        Symb s => s
      | Id s => s
      | Num s => s

datatype state = IdS of loc * string
               | SymbS of loc * string
               | NumS of loc * string
               | CommentS of loc * string
               | BeginS

fun tokenise {sep_chars         : string,                  (* single-char symbols *)
              symb_chars        : string,                  (* multi-char symbols *)
              is_id             : string -> bool,          (* is a string an id? *)
              is_num            : string -> bool}          (* is a string a number? *)
             {srcname:string, input:string} : (token*reg) list =
    let
      fun isSymbChar c = CharVector.exists (fn c' => c=c') symb_chars
      fun isIdChar0 c = is_id(String.str c)
      fun isSepChar c = CharVector.exists (fn c' => c=c') sep_chars
      fun next (l:loc,l':loc,c:char,s:state,ts:(token*reg)list)
          : loc * state * (token*reg)list =
          case s of
              CommentS (l0,"*") =>
              if c = #")" then (l',BeginS,ts)
              else (l',CommentS(l0, ""),ts)
            | CommentS(l0,"(") =>
              if c = #"*" then (l',CommentS(l0,""),ts)
              else (l',BeginS,(Symb "(", (l0,l0))::ts)
            | CommentS (l0,"") =>
              if c = #"*" then (l',CommentS(l0,"*"),ts)
              else (l',CommentS(l0,""),ts)
            | BeginS =>
              if c = #"(" then (l',CommentS (l',"("),ts)
              else if isSepChar c then (l',BeginS,(Symb(String.str c),(l,l))::ts)
              else if isSymbChar c then (l',SymbS(l,String.str c),ts)
              else if isIdChar0 c then (l',IdS(l,String.str c),ts)
              else if Char.isDigit c then (l',NumS(l,String.str c),ts)
              else if Char.isSpace c then (l',BeginS,ts)
              else raise Fail ("lex error at location " ^ Region.ppLoc l')
            | SymbS (l0,s) =>
              if c = #"(" then (l',CommentS (l',"("),(Symb s,(l0,l))::ts)
              else if isSepChar c then (l',BeginS,(Symb(String.str c),(l',l'))::(Symb s,(l0,l))::ts)
              else if isSymbChar c then (l',SymbS(l0,s ^ String.str c),ts)
              else if Char.isDigit c then (l',NumS(l',String.str c),(Symb s,(l0,l))::ts)
              else if isIdChar0 c then (l',IdS(l',String.str c),(Symb s,(l0,l))::ts)
              else if Char.isSpace c then (l',BeginS,(Symb s,(l0,l))::ts)
              else raise Fail ("lex error at location " ^ Region.ppLoc l')
            | NumS (l0,s) =>
              if c = #"(" then (l',CommentS (l',"("),(Num s,(l0,l))::ts)
              else if isSepChar c then (l',BeginS,(Symb(String.str c),(l',l'))::(Num s,(l0,l))::ts)
              else if isSymbChar c then (l',SymbS(l',String.str c),(Num s,(l0,l))::ts)
              else if is_num(s ^ String.str c) then (l',NumS(l0,s ^ String.str c),ts)
              else if Char.isSpace c then (l',BeginS,(Num s,(l0,l))::ts)
              else raise Fail ("lex error at location " ^ Region.ppLoc l')
            | IdS (l0,s) =>
              if c = #"(" then (l',CommentS (l',"("),(Id s,(l0,l))::ts)
              else if isSepChar c then (l',BeginS,(Symb(String.str c),(l',l'))::(Id s,(l0,l))::ts)
              else if isSymbChar c then (l',SymbS(l',String.str c),(Id s,(l0,l))::ts)
              else if is_id (s ^ String.str c) then (l',IdS(l0,s ^ String.str c),ts)
              else if Char.isSpace c then (l',BeginS,(Id s,(l0,l))::ts)
              else raise Fail ("lex error at location " ^ Region.ppLoc l')
            | CommentS _ => raise Fail "lex: impossible"

      val s0 = (Region.loc0 srcname, BeginS, nil)
      val (l',s,ts) =
          CharVector.foldl (fn (c, (l,s,ts)) =>
                               let val l' = if c = #"\n" then Region.newline l
                                            else Region.next l
                               in next (l,l',c,s,ts)
                               end) s0 input
      val ts =
          case s of
              SymbS(l,s) => (Symb s,(l,l'))::ts
            | NumS(l,s) => (Num s,(l,l'))::ts
            | IdS(l,s) => (Id s,(l,l'))::ts
            | CommentS _ => raise Fail "lex error: non-closed comment"
            | BeginS => ts
    in rev ts
    end

end
