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