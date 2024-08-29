structure Region :> REGION = struct
  type srcname = string
  type loc = int * int * srcname
  val botloc = (0,0,"")
  fun loc0 f = (1,0,f)
  fun newline l =
      if l = botloc then
        raise Fail "Region.newline: botloc is not a real location"
      else (#1 l + 1,0,#3 l)
  fun next l =
      if l = botloc then
        raise Fail "Region.next: botloc is not a real location"
      else (#1 l, #2 l + 1, #3 l)
  fun lt (l1:loc) (l2:loc) =
      if l2 = botloc then false
      else l1 = botloc orelse
           #1 l1 < #1 l2 orelse (#1 l1 = #1 l2 andalso #2 l1 < #2 l2)
  fun ppLoc0 (a,b,_) = Int.toString a ^ "." ^ Int.toString b
  fun ppLoc (l:loc) = #3 l ^ ":" ^ ppLoc0 l

  type reg = (loc * loc) option

  val emp : reg = NONE

  fun wf NONE = true
    | wf (SOME (l1,l2)) = #3 l1 <> #3 l2 orelse l1 = l2 orelse lt l1 l2

  fun pp NONE = "nowhere"
    | pp (SOME(a,b)) =
      if a = b then ppLoc a
      else if #3 a = #3 b then #3 a ^ ":" ^ ppLoc0 a ^ "-" ^ ppLoc0 b
      else ppLoc a ^ "-" ^ ppLoc b

  fun plus s r1 r2 =
      case (r1,r2) of
          (NONE,_) => r2
        | (_,NONE) => r1
        | (SOME (l1,l1'), SOME(l2,l2')) =>
          if wf r1 andalso wf r2 andalso (lt l1' l2 orelse #3 l1' <> #3 l2) then
            SOME(l1, l2')
          else raise Fail ("Region " ^ pp r1 ^ " cannot be merged with region " ^ pp r2 ^ " at " ^ s)

  fun mkReg (l1,l2) = SOME (l1,l2)
  fun unReg x = x
end
