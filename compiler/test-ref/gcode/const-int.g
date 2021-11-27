Begin
PushGlobal $Prog
Eval
Print
End
GlobStart $Prog 0
PushInt 5
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
