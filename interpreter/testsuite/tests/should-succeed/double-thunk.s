# test: do double thunks evaluate correctly (e.g. notation for double thunk is [[x]])
# (\x. (\y. y) x) 5
# (\y. y) [5]
# [[5]] (in reality it's optimized to just return [5], the [5] is not thunked again)
# 5
NAT 5
VAR X
LAM X
APP
VAR X
LAM X
APP