################################################################################
#
# factorial function implemented with Y combinator
#
#     Y (\fact. \n. if (= n 1) 1 (* n (fact (- n 1))))
#
# which is roughly analogous to
#
#     fact n = if n == 1 then 1 else n * (fact (n - 1))
#
# [example eval]
#
# let fact_rec = (\fact. \n. if (= n 1) 1 (* n (fact (- n 1))))
#
# ((Y fact_rec) 2)    # analogous to fib 2
# ((fact_rec (Y fact_rec)) 2)
# (((\fact. \n. if (= n 1) 1 (* n (fact (- n 1)))) (Y fact_rec)) 2)
# ((\n. if (= n 1) 1 (* n ((Y fact_rec) (- n 1)))) 2)
# (if (= 2 1) 1 (* 2 ((Y fact_rec) (- 2 1))))
# (* 2 ((Y fact_rec) (- 2 1)))
# (* 2 ((Y fact_rec) 1))
#
# let's eval just ((Y fact_rec) 1) for readability
#
# ((Y fact_rec) 1)
# ((fact_rec (Y fact_rec)) 1)
# (((\fact. \n. if (= n 1) 1 (* n (fact (- n 1)))) (Y fact_rec)) 1)
# ((\n. if (= n 1) 1 (* n ((Y fact_rec) (- n 1)))) 1)
# (if (= 1 1) 1 (* 1 ((Y fact_rec) (- 1 1))))
# 1
#
# thus ((Y fact_rec) 1) => 1
# so
#
# (* 2 ((Y fact_rec) 1))
# (* 2 1)
# 2
#
################################################################################

# full expression:
#   ((Y (\fact. \n. if (= n 1) 1 (* n (fact (- n 1))))) 6)

INT 6
# [ 6 ]

INT 1
VAR N
FUN -
APP
APP
# [ (- n 1) 6 ]

VAR FACT
APP
# [ (fact (- n 1)) 6 ]

VAR N
FUN *
APP
APP
# [ (* n (fact (- n 1))) 6 ]

INT 1
# [ 1 (* n (fact (- n 1))) 6 ]

INT 1
VAR N
FUN =
APP
APP
# [ (= n 1) 1 (* n (fact (- n 1))) 6 ]

FUN IF
APP
APP
APP
# [ (if (= n 1) 1 (* n (fact (- n 1)))) 6 ]

LAM N
LAM FACT
# [ (\fact. \n. if (= n 1) 1 (* n (fact (- n 1)))) 6 ]

FUN Y
APP
# [ (Y (\fact. \n. if (= n 1) 1 (* n (fact (- n 1))))) 6 ]

APP
# [ ((Y (\fact. \n. if (= n 1) 1 (* n (fact (- n 1))))) 6) ]
