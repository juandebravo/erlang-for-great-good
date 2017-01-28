-module(exceptions).
-compile(export_all).

first_example(F) ->
    try F() of
        _ -> ok
    catch
        throw:Throw -> {throw, Throw};
        error:specific -> {error, "Changing the `specific` value to this sentence"};
        error:Error -> {error, Error};
        exit:Exit -> {exit, Exit};
        _:_ -> {unknown, "This will never happen, actually"} % Not a good idea though
    after
        io:format("This is always executed. You cannot return a value from this block! ~n")
    end.

%% You can include multiple expressions between try and catch
%% of is required only when you need pattern matching
second_example() -> second_example(fun () -> ok end).
second_example(F) ->
    try
        A = 1,
        B = 2,
        C = 3 = A + B,
        F(),
        C + 5
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.

third_example(X, Y) ->
    case catch X/Y of
        {'EXIT', {badarith,_}} -> "zero division!";
        N -> N
    end.
