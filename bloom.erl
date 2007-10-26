-module(bloom).
-export([new/1, new/2, is_element/2, add_element/2]).

-record(bloom, {m=0, bitmap = <<>>, k=0, n=0, keys=0}).

new(N) -> new(N, 0.001).
new(N, E) when N > 0, is_float(E), E > 0, E =< 1 ->
    {M, K} = calc_least_bits(N, E),
    #bloom{m=M, bitmap = <<0:(M+8 - M band 7)>>, k=K, n=N}.
%new(M, K) when M > 0, is_integer(K), K > 0 -> throw(unimplemented).

is_element(Key, B) -> is_element(Key, B, calc_idxs(Key, B)).
is_element(_, _, []) -> true;
is_element(Key, B, [{BitIdx, ByteIdx} | T]) ->
    <<_:ByteIdx/binary, Byte, _/binary>> = B#bloom.bitmap,
    Mask = 1 bsl (BitIdx rem 8),
    case Byte band Mask =/= 0 of
        true -> is_element(Key, B, T);
        false -> false
    end.
    
add_element(Key, #bloom{keys=Keys, n=N, bitmap=Bitmap} = B) when Keys =< N ->
    Idxs = lists:keysort(1, calc_idxs(Key, B)),
    {Count, Bitmap0} = set_bits(Bitmap, Idxs),
    case Count of
        0 -> B;
        _ -> B#bloom{bitmap=Bitmap0, keys=Keys+1}
    end.

set_bits(Bin, Idxs) -> set_bits(Bin, binary_to_list(Bin), 0, Idxs, [], 0).
set_bits(Bin, [Byte | Bytes], I, [{BitIdx, ByteIdx} | Idxs], Acc, Count) ->
    if ByteIdx =:= I ->
        Mask = 1 bsl (BitIdx rem 8),
        Byte0 = Byte bor Mask,
        Count0 = if Byte =/= Byte0 -> Count+1; true -> Count end,
        set_bits(Bin, [Byte0 | Bytes], I, Idxs, Acc, Count0);
    true ->
        set_bits(Bin, Bytes, I+1, [{BitIdx, ByteIdx} | Idxs], [Byte | Acc],
                 Count)
    end;
set_bits(Bin, [Byte | Bytes], I, [], Acc, Count) -> 
    set_bits(Bin, Bytes, I, [], [Byte | Acc], Count);
set_bits(Bin, [], _, _, _, 0) -> {0, Bin};
set_bits(_, [], _, _, Acc, Count) ->
    {Count, list_to_binary(lists:reverse(Acc))}.

calc_least_bits(N, E) -> calc_least_bits(N, E, 1, 0, 0).
calc_least_bits(N, E, K, MinM, BestK) ->
    M = -1 * K * N / math:log(1 - math:pow(E, 1/K)),
    {CurM, CurK} = if M < MinM -> {M, K}; true -> {MinM, BestK} end,
    case K of
          1 -> calc_least_bits(N, E, K+1, M, K);
        100 -> {trunc(CurM)+1, CurK};
          _ -> calc_least_bits(N, E, K+1, CurM, CurK)
    end.

% Todo: handle case of m > 2^32.
calc_idxs(Key, #bloom{m=M, k=K}) ->
    [X, Y] = [erlang:phash2(X, M) || X <- [Key, {Key, "salt"}]],
    calc_idxs(M, K - 1, X, Y, [{X, X bsr 3}]).
calc_idxs(_, 0, _, _, Acc) -> Acc;
calc_idxs(M, I, X, Y, Acc) ->
    Xi = (X+Y) rem M,
    Yi = (Y+I) rem M,
    calc_idxs(M, I-1, Xi, Yi, [{Xi, Xi bsr 3} | Acc]).