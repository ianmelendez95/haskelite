################################################################################
#
# let f_rec = (\f. \n. if (= n 0) 2 (f (- n 1)))
#
# Y (\f. \n. if (= n 0) 2 (f (- n 1))) 1
# is the same as: Y f_rec 1
# f_rec (Y f_rec) 1
# (\f. \n. if (= n 0) 2 (f (- n 1))) (Y f_rec) 1  # expanded first f_rec
# (\n. if (= n 0) 2 ((Y f_rec) (- n 1))) 1
# (if (= 1 0) 2 ((Y f_rec) (- 1 1)))
# ((Y f_rec) (- 1 1))
# ((Y f_rec) 0)   # eagerly eval (- 1 1) here for readability, would normally be lazily deferred
# (f_rec (Y f_rec) 0)
# (\f. \n. if (= n 0) 2 (f (- n 1))) (Y f_rec) 0  # expanded first f_rec
# (if (= 0 0) 2 ((Y f_rec) (- 0 1)))
# 2
#
# Analogous to
# f x = if x == 0 then 2 else f (x - 1)
#
################################################################################

# full expression: (Y (\f. \n. (if (= n 0) 2 (f (- n 1)))) 1)

INT 1
# [1] (1): 1

INT 1
VAR N
FUN -
APP
APP
VAR F
APP
# [2 1] (2): (f (- n 1))

INT 2
# [3 2 1] (3): 2

INT 0
VAR N
FUN =
APP
APP
# [4 3 2 1] (4): (= n 0)

FUN IF
APP
APP
APP
# [5 1] (5 <= 4 3 2): (if (= n 0) 2 (f (- n 1)))

LAM N
LAM F
# [6 1] (6 <= 5): (\f. \n. (if (= n 0) 2 (f (- n 1))))

FUN Y
APP
APP
# [7] (7 <= 6 1): (Y (\f. \n. (if (= n 0) 2 (f (- n 1)))) 1)