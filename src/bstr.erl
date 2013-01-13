-module(bstr).
-author('Artem Artemiev <art.art.v@gmail.com>').

-export([len/1
         ,join/2
         ,concat/2
         ,tokens/2
         ,str/2
         ,rstr/2
         ,substr/2
         ,substr/3
         ,copies/2
         ,strip/2
         ,strip/3
         ,reverse/1
         ,entry/2]).


len(BitString) ->
    byte_size(BitString).

concat(BitString0, BitString1) ->
    <<BitString0/bytes, BitString1/bytes>>.

join(BitStringList, Separator) ->
    Raw = << <<BitString/bytes, Separator/bytes>> || BitString <- BitStringList >>,
    Size = byte_size(Raw) - byte_size(Separator),
    <<Result:Size/bytes, _Rest/bytes>> = Raw,
    Result.

tokens(BitString, SeparatorList) ->
    [S || S <- binary:split(BitString, SeparatorList, [global, trim]), S =/= <<>>].

str(BitString, SubBitString) ->
    case binary:match(BitString, SubBitString, []) of
        {Start, _End} -> Start+1;
        nomatch -> 0
    end.

rstr(BitString, SubBitString) ->
    case binary:matches(BitString, SubBitString, []) of
        nomatch -> 0;
        List ->
            case catch lists:last(List) of
                {'EXIT', _Reason} -> 0;
                {Start, _End} -> Start+1
            end
    end.

substr(BitString, Start) ->
    binary:part(BitString, {Start-1, byte_size(BitString)-Start+1}).

substr(BitString, Start, Len) ->
    binary:part(BitString, {Start-1, Len}).

copies(BitString, Num) ->
    binary:copy(BitString, Num).

strip(BitString, left) ->
    lstrip(BitString, <<" ">>);
strip(BitString, right) ->
    rstrip(BitString, <<" ">>);
strip(BitString, both) ->
    bstrip(BitString, <<" ">>).

strip(BitString, left, Character) ->
    lstrip(BitString, Character);
strip(BitString, right, Character) ->
    rstrip(BitString, Character);
strip(BitString, both, Character) ->
    bstrip(BitString, Character).

reverse(BitString) ->
    rev(BitString, <<>>).

entry(BitString, SubBitString) ->
    case binary:match(BitString, SubBitString, []) of
        nomatch ->
            false;
        _Other ->
            true
    end.

%%%
%%%
%%%

lstrip(<<A:1/bytes, Rest/bytes>>, A) ->
    lstrip(Rest, A);
lstrip(Result, _A) ->
    Result.

rstrip(BitString0, A) ->
    RevBitString = rev(BitString0, <<>>),
    BitString1 = lstrip(RevBitString, A),
    rev(BitString1, <<>>).

bstrip(BitString0, A) ->
    BitString1 = lstrip(BitString0, A),
    rstrip(BitString1, A).

rev(<<>>, Reversed) ->
    Reversed;
rev(<<Letter:1/bytes, RestBitString/bytes>>, Reversed) ->
    rev(RestBitString, <<Letter/bytes, Reversed/bytes>>).

%%%
%%%
%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

bstr_test() ->
    ?assertMatch(<<"test test">>, bstr:strip(<<"   test test  ">>, both)),
    ?assertMatch(true, bstr:entry(<<"testsubstringtest">>, <<"substring">>)),
    ?assertMatch(false, bstr:entry(<<"testsubstringtest">>, <<"substring0">>)),
    ?assertMatch(<<"543210">>, bstr:reverse(<<"012345">>)),
    ?assertMatch(<<"testtesttest">>, bstr:copies(<<"test">>, 3)),
    ?assertMatch([<<"test0">>, <<"test1">>], bstr:tokens(<<"test0,,test1,,,">>, <<",">>)),
    ?assertMatch([], bstr:tokens(<<"      ">>, <<" ">>)),
    ?assertMatch(<<"t1;t2;t3">>, bstr:join([<<"t1">>, <<"t2">>, <<"t3">>], <<";">>)).

-endif.
