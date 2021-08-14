# test: does application of a thunk work
# (\y. y 5) (\x. x)
# [ (\x. x) ] 5
# (\x. x) 5
# 5
VAR X
LAM X
INT 5
VAR Y
APP
LAM Y
APP