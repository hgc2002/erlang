-module(indexfile).
-import(string,[len/1,lowercase/1,lexemes/2,replace/3]).
-import(maps,[keys/1,get/2,put/3]).
-import(lists,[reverse/1,sort/1]).
-import('index',[get_file_contents/1,show_file_contents/1]).
-export([create_index/1, test/0, test2/0]).

% create the requested index for the given file
create_index(Filename) ->
    Lines = get_file_contents(Filename),
    % show_file_contents(Lines).
    create_index2(Lines).

% useful to call it from test routines
create_index2([A|As]) ->
    lists:sort(fix_map(process_lines([A|As]))).

% some simple tests to run manually
test() -> 
    % Lines = ["aaa, bbb! ccc? bbB e'ee", "Aaa ddd --ccc `bbb' eee"],
    Lines = ["eee fff aaa, bbb! ccc? bbB e'e", "Aaa ddd --ccc `bbb' eee","aaa","bbb","aaa"],
    % Lines = ["aaa","aaa","bbb","aaa", "zz"],
    lists:sort(fix_map(process_lines(Lines))).

% specific file test
test2() ->
    create_index("gettysburg-address.txt").

% because words are stored in a map, the requested output is a list of tuples 
% this routines convert the word map in a list of tuples.
fix_map(Map) ->
    Keys = maps:keys(Map),
    fix_map(Keys,Map,[]).
fix_map([], _Map, Acum) ->
    Acum;
fix_map([Key|Ks],Map,Acum) ->
    Lines  = maps:get(Key,Map),
    Lines2 = maps:keys(Lines), 
    Lines3 = fix_lines(Lines2),
    fix_map(Ks,Map,[{Key,Lines3}|Acum]).

% because line occurrrence is stored as a map for each word, this routine convert
% every line maps in a list of lines, and then out it back again into the word map.
fix_lines([Line|Ls]) ->
    fix_lines(Ls, Line, Line, []).

fix_lines([], FirstLine, LastLine, []) -> 
    [{FirstLine, LastLine}];
fix_lines([], FirstLine, LastLine, [A|As]) -> 
    lists:reverse([{FirstLine, LastLine}, A | As]);
fix_lines([Line|Ls], FirstLine, LastLine, Acum) ->
    if 
        Line == LastLine+1 ->
            fix_lines(Ls, FirstLine, Line, Acum);
        true ->
            fix_lines(Ls, Line, Line, [{FirstLine, LastLine} | Acum])
    end.

% process all lines (list of lines) of the given text and return a word map
% where the line occurrence is in a map per word
process_lines(Lines) ->
    process_lines(Lines, 1, #{}).

process_lines([], _LineNumber, Map) ->
    Map;
process_lines([Line|Ls], LineNumber, Map) ->
    NewMap = single_line(Line, LineNumber, Map),
    process_lines(Ls, LineNumber+1, NewMap).

% process a single line and return a map of line occurrences per each word found in the line.
% lines are converted to lowercase before get indexed
single_line([], _LineNumber, Map) ->
    Map;
single_line(Line, LineNumber, Map) ->
    Line2 = string:replace(Line, "--", "", all),    % special case with double dash "--""
    Line3 = string:replace(Line2, "' ", " ", all),  % special case with apostrophe ans space"' ""
    Line4 = string:lowercase(Line3),                % uppercase same as lowercase
    Words = string:lexemes(Line4, " .,`:()[]?!\\"), % make a list of words
    Word2 = remove_short(Words),
    add_to_map(Word2, LineNumber, Map).

% remove short words (everything with 3 or less characters) is dismissed,
% returning a list of the accepted words only
remove_short(Words) ->
    remove_short(Words,[]).

remove_short([], Acum) ->
    Acum;
remove_short([Word|Ws],[]) ->
    if
        length(Word) >= 3 ->
            remove_short(Ws, [Word]);
        true ->
            remove_short(Ws, [])
    end;
remove_short([Word|Ws],[A|As]) ->
    if
        length(Word) >= 3 ->
            remove_short(Ws, [Word,A|As]);
        true ->
            remove_short(Ws, [A|As])
    end.

% add a line occurrence to the map of the given word, returning the final map
add_to_map([], _LineNumber, Map) ->
    Map;
add_to_map([Word|Ws], LineNumber, Map) ->
    Map1 = maps:get(Word, Map, #{}),        % every word is the key of its own map of lines
    Map2 = maps:put(LineNumber, ok, Map1),   % put the new line in the map of lines
    Map3 = maps:put(Word, Map2, Map),      % put the new map of lines in the word map
    add_to_map(Ws, LineNumber, Map3).       % adding the next word to word map

% EOF
