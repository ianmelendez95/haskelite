################################################################################
#
# factorial function implemented with Y combinator (for 20!)
#
#   NOTE: like y-fact.s but with factorial 20, the maximum value possible
#         for 64 bit nums
#
# see: y-fact.s
#
################################################################################

# ((Y (\fact. \n. if (= n 1) 1 (* n (fact (- n 1))))) 20)
INT 20
INT 1
VAR N
FUN -
APP
APP
VAR FACT
APP
VAR N
FUN *
APP
APP
INT 1
INT 1
VAR N
FUN =
APP
APP
FUN IF
APP
APP
APP
LAM N
LAM FACT
FUN Y
APP
APP
