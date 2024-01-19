-compile(export_all).

% PORTCULLIS INTERNAL
plus_(A) -> fun(B) -> A + B end.
minus_(A) -> fun(B) -> A - B end.
mult_(A) -> fun(B) -> A * B end.
div_(A) -> fun(B) -> A / B end.
rem_(A) -> fun(B) -> A rem B end.
gt_(A) -> fun(B) -> A > B end.
gte_(A) -> fun(B) -> A >= B end.
lt_(A) -> fun(B) -> A < B end.
lte_(A) -> fun(B) -> A =< B end.
eq_(A) -> fun(B) -> A == B end.
cons_(A) -> fun(B) -> [A | B] end.

if_(true) -> fun(E) -> fun(_) -> E end end;
if_(false) -> fun(_) -> fun(E) -> E end end.

uncons_([]) -> fun(B) -> fun(_) -> B end end;
uncons_([H, T]) -> fun(_) -> fun(F) -> (F(H))(T) end end.

% USER CODE
