%% Always include the filename as attribute of `module` info
-module(beginner).

%% Include as much metadata as you want
-author("Juan de Bravo").
-foo("Bar").

%% Key topic for hot loading code while running in production
-vsn("This should be an unique value that differentiates each version of th code, excluding comments. Important for hot-loading and release handling").

%% Define the functions that should be published outside the module
-export([add/2, hello/0, great_and_add_two/1, show_info/0, hello/2]).

%% Macros definition. They are substituted in compilation time
-define(HOUR, 3600). % in seconds
-define(sub(A, B), A-B).

%% Define a macro based on a compile option
-ifdef(DEBUGMODE).
-define(DEBUG(S), io:format("dbg: " ++ S ++ "~n")).
-else.
-define(DEBUG(_), ok).
-endif.

%% Function definition
add(A, B) ->
    ?DEBUG("Entering add function"),
    A + B.

hello() ->
    hello(male, "world").

%% 1 function declaration
%% 2 function clauses
hello(male, S) ->
    ?DEBUG(S),
    io:format("Hello, Mr ~s!~n", [S]);
hello(female, S) ->
    ?DEBUG(S),
    io:format("Hello, Ms ~s!~n", [S]).

great_and_add_two(X) ->
    %% Calling a function defined in the same module
    %% does not require including the module name
    hello(),
    add(X, 2).

show_info() ->
    io:format("This is the line ~~~p~~ in file `~s`, that defines the module `~s`~n", [?LINE, ?FILE, ?MODULE]).
