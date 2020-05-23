-module(palin).
-import(string,[to_lower/1]).
-export([palindrome/1, test/0]).

palindrome([]) ->
    false;
palindrome([_X]) ->
    true;
palindrome([X,X]) -> 
    true;
palindrome([X,_Y,X]) ->
    true;
palindrome([X|Xs]) ->
    Y = string:to_lower([X|Xs]),
    Y == lists:reverse(Y).

test() ->
    ok = test_palindrome(),
    ok.

test_palindrome() ->
    % {'EXIT', {function_clause, _}} = (catch palindrome(1)),
    false = palindrome([]),
    false = palindrome(""),
    true = palindrome("a"),
    true = palindrome("aa"),
    false = palindrome("ab"),
    true = palindrome("aaa"),
    false = palindrome("aab"),
    false = palindrome("abb"),
    false = palindrome("bba"),
    true = palindrome("aba"),
    true = palindrome("abba"),
    false = palindrome("abca"),
    false = palindrome("cbba"),
    false = palindrome("bbba"),
    true = palindrome("abcba"),
    false = palindrome("abcca"),
    false = palindrome("abbca"),
    false = palindrome("Madam I\'m Adam"),
    true = palindrome("MadamImAdam"),
    false = palindrome("madamixmadam"),
    ok. 

% eof
