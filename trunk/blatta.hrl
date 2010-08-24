-record(board, {p, b, n, r, q, k, s}).
-record(engine, {process}).
-record(game, {w, b, main_board, turn}).

-define(UCIOK, io:format("uciok~n",[])).
-define(WBOARD, 16#ffff).
-define(BBOARD, 16#ffff000000000000).
-define(DRAW_LINE, io:format("+-+-+-+-+-+-+-+-+~n", [])).
-define(EMPTY_GAME, #game{w=0, b=0, turn=2, main_board=#board{p=0, b=0, n=0, r=0, q=0, k=0, s=array:new([{size,64},{fixed,true},{default,0}])}}).
-define(START_POS, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").
-define(POWS, {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,
 262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,
 134217728,268435456,536870912,1073741824,2147483648,4294967296,8589934592,
 17179869184,34359738368,68719476736,137438953472,274877906944,549755813888,
 1099511627776,2199023255552,4398046511104,8796093022208,17592186044416,
 35184372088832,70368744177664,140737488355328,281474976710656,
 562949953421312,1125899906842624,2251799813685248,4503599627370496,
 9007199254740992,18014398509481984,36028797018963968,72057594037927936,
 144115188075855872,288230376151711744,576460752303423488,1152921504606846976,
 2305843009213693952,4611686018427387904,9223372036854775808,
 18446744073709551616}).

-define(FILES, {16#0101010101010101, 16#0202020202020202,  16#0404040404040404, 16#0808080808080808, 16#1010101010101010, 16#202020202020202 , 16#4040404040404040, 16#8080808080808080}).

-define(STRING_POS, {
		"a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1",
		"a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
		"a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
		"a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
		"a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
		"a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
		"a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
		"a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8"}).

-define(DB64, {
   63,  0, 58,  1, 59, 47, 53,  2,
   60, 39, 48, 27, 54, 33, 42,  3,
   61, 51, 37, 40, 49, 18, 28, 20,
   55, 30, 34, 11, 43, 14, 22,  4,
   62, 57, 46, 52, 38, 26, 32, 41,
   50, 36, 17, 19, 29, 10, 13, 21,
   56, 45, 25, 31, 35, 16,  9, 12,
   44, 24, 15,  8, 23,  7,  6,  5
}).
-define(KDB64, 16#07EDD5E59A4E28C2).
-define(KBMASK, 16#0FFFFFFFFFFFFFFFF).
lbpow(I) -> element(I+1, ?POWS).

bsf(BB) ->
	I = (((((BB band (-BB)) band ?KBMASK) * ?KDB64) band ?KBMASK) bsr 58) + 1,
        element(I, ?DB64).

get_info($p) -> {#game.b, #board.p};
get_info($r) -> {#game.b, #board.r};
get_info($n) -> {#game.b, #board.n};
get_info($b) -> {#game.b, #board.b};
get_info($q) -> {#game.b, #board.q};
get_info($k) -> {#game.b, #board.k};
get_info($P) -> {#game.w, #board.p};
get_info($R) -> {#game.w, #board.r};
get_info($N) -> {#game.w, #board.n};
get_info($B) -> {#game.w, #board.b};
get_info($Q) -> {#game.w, #board.q};
get_info($K) -> {#game.w, #board.k}.

get_pos(C, L) -> CV = 7 - (C - $a), LV = (L - $1) * 8, CV + LV.

%%%get_pawn_table_attack(#game.w, I) -> element(I+1, ?WHITE_PAWN_ATTACK);
%%%get_pawn_table_attack(#game.b, I) -> element(I+1, ?BLACK_PAWN_ATTACK);

print_moves(T, Game) -> print_moves(T, Game, 1).
print_moves([], _, _) -> ok;
print_moves([H|T], Game, I) ->
        {_, Pos, M1} = H,
	io:format("Move[~p] ~p -> ~p:~n", [I, Pos, M1]),
        print_game(process_move(Pos, M1, Game)),
        print_moves(T, Game, I + 1).

print_game(#game{main_board=#board{s=S}}) ->
        ?DRAW_LINE,
        print_game(S, 63).

print_game(_, -1) -> io:format("|~n", []), ?DRAW_LINE;
print_game(Array, N) ->
        X = (N+1) rem 8,
        case X == 0 andalso N < 63  of
                true -> io:format("|~n", []), ?DRAW_LINE;
                _ -> ok
        end,
        io:format("|~s", [get_char(array:get(N,Array))]),
        print_game(Array, N-1).

next_index(0) -> {-1, -1};
next_index(L) ->
        B=bsf(L),
        {B, (L - lbpow(B))}.

get_char(0) -> " ";
get_char({#game.b, Piece}) -> element(Piece - 1, {"p", "b", "n", "r", "q", "k"});
get_char({#game.w, Piece}) -> element(Piece - 1, {"P", "B", "N", "R", "Q", "K"}).

read_fen(S, #game{}=Game) -> read_fen(S, 63, Game).
read_fen([], _, #game{}=Game) -> Game;
read_fen([$/|T], I, #game{}=Game) -> read_fen(T, I, Game);
read_fen([N|T], I, #game{}=Game) when N >= $1, N =< $9 ->
        NN = (N - $1) + 1,
        read_fen(T, I - NN, Game);
read_fen([H|T], I, #game{}=Game) ->
        {Color, Piece} = get_info(H),
        read_fen(T, I-1, put_piece(Color, Piece, I, Game)).

get_file(I) -> element((I rem 8)+1, ?FILES).


