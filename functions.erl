
-module(functions).
-compile(export_all).

first([H|_]) -> H.

second([_,S|_]) -> S.

%% Pattern matching
%% first T variable is binded to a value. If the second parameter is similar,
%% the first function clause will be taken. Otherwise, it will fallback to the
%% second function clause
same(T, T) -> true;
same(_, _) -> false.

%% Use guards to define better pattern matching.
%% `andalso` similar to `,`
old_enough(X) when X >= 16 andalso X < 105 -> true;
old_enough(_) -> false.

%% Use guards to define better pattern matching,
%% `orelse` similar to `;`, even though `;` catches an exception if it happens in the first guard
%% and the second one will be evaluated
working_day(X) when X =:= 6 orelse X =:= 7 -> false;
working_day(_) -> true.

%% `andalso` and `orelse` clauses can be nested
valid_number(X) when (X >= 0 andalso X < 1000) orelse X =:= -1 -> true;
valid_number(_) -> false.

help_me(Animal) ->
    Talk = if Animal =:= cat -> "meow";
              Animal =:= beef -> "mooo";
              Animal =:= dog -> "bark";
              true -> "fadfsdf"
           end,
    {Animal, "says " ++ Talk ++ "!"}.

%% Using a function to check the parameter type as guard clause
say_hello(T) when is_list(T) -> io:format("Hello ~s!~n", [T]);
say_hello(T) when is_integer(T) -> io:format("Hello ~p!~n", [T]);
say_hello(_) -> io:format("Hello unknown!~n").

%% Using several functions as guard clauses
is_empty(L) when is_list(L) andalso length(L) =:= 0 -> true;
is_empty(_) -> false.

%% Recursion
fact(0) -> 1;
fact(N) when N >= 1 -> N * fact(N-1);
fact(_) -> 0.

len([]) -> 0;
len([_|T]) -> 1 + len(T).

%% Tail Recursion
tail_fact(N) -> tail_fact(N, 1).

tail_fact(0, Acc) -> Acc;
tail_fact(N, Acc) -> tail_fact(N-1, N*Acc).

tail_len(L) -> tail_len(L, 0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc+1).

duplicate(N, Term) -> tail_duplicate(N, Term, []).

tail_duplicate(0, _, Acc) -> Acc;
tail_duplicate(N, Term, Acc) -> tail_duplicate(N-1, Term, [Term|Acc]).

tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) -> tail_reverse(T, [H|Acc]).

sublist(T, N) when N >= length(T) -> T;
sublist(T, N) -> tail_sublist(T, N, []).

tail_sublist(_, 0, Acc) -> Acc;
tail_sublist([H|T], N, Acc) -> tail_sublist(T, N-1, Acc ++ [H]).
% Another option
%tail_sublist([H|T], N, Acc) -> tail_sublist(T, N-1, [H|Acc]);

zip(X, Y) -> tail_reverse(zip(X, Y, [])).

zip([], _, Acc) -> Acc;
zip(_, [], Acc) -> Acc;
zip([X|Xs], [Y|Ys], Acc) -> zip(Xs, Ys, [{X, Y}|Acc]).

map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].

filter(F, L) -> lists:reverse(filter(F, L, [])).
filter(_, [], Acc) -> Acc;
filter(F, [H|T], Acc) ->
    case F(H) of
        true -> filter(F, T, [H|Acc]);
        false -> filter(F, T, Acc)
    end.
%% using if
%% filter(F, [H|T], Acc) ->
%%     A = F(H),
%%     if A =:= true -> filter(F, T, [H|Acc]);
%%     true -> filter(F, T, Acc)
%%     end.


%% Reduce an array to a value
fold(_, []) -> [];
fold(F, [H|T]) -> fold(F, H, T).
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(Start, H), T).

max([H|T]) -> fold(fun(X,Y) when X > Y -> X; (_,Y) -> Y end, H, T).

reverse(L) ->
    fold(fun(Acc, X) -> [X|Acc] end, [], L).

