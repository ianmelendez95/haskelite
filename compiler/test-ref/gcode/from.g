Begin
PushGlobal $Prog
Eval
Print
End
GlobStart $from 1
Push 0
PushGlobal $succ
MkAp
PushGlobal $from
MkAp
Push 1
PushGlobal $CONS
MkAp
MkAp
Update 2
Pop 1
Unwind
GlobStart $succ 1
PushInt 1
Push 1
PushGlobal $+
MkAp
MkAp
Update 2
Pop 1
Unwind
GlobStart $Prog 0
PushInt 0
PushGlobal $succ
MkAp
PushGlobal $from
MkAp
Update 1
Pop 0
Unwind
GlobStart $NEG 1
Eval
Neg
Update 1
Return
GlobStart $+ 2
Push 1
Eval
Add
Update 3
Pop 2
Return
GlobStart $CONS 2
Cons
Update 1
Return
GlobStart $HEAD 1
Eval
Head
Eval
Update 1
Unwind
GlobStart $TAIL 1
Eval
Tail
Eval
Update 1
Unwind
