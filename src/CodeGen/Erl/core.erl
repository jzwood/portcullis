% PORTCULLIS INTERNAL

_plus_ = fun(A) -> fun(B) -> A + B end end.
_minus_ = fun(A) -> fun(B) -> A - B end end.
_mult_ = fun(A) -> fun(B) -> A * B end end.
_div_ = fun(A) -> fun(B) -> A / B end end.
_rem_ = fun(A) -> fun(B) -> A rem B end end.
_gt_ = fun(A) -> fun(B) -> A > B end end.
_gte_ = fun(A) -> fun(B) -> A >= B end end.
_lt_ = fun(A) -> fun(B) -> A < B end end.
_lte_ = fun(A) -> fun(B) -> A <= B end end.
_eq_ = fun(A) -> fun(B) -> A == B end end .
_cons_ = fun(A) -> fun(B) -> [A | B].

% USER CODE
