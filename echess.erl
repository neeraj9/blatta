-module(echess).
-compile(export_all).

-export([init/0]).

-define(UCIOK, io:format("uciok~n",[])).
-define(WBOARD, 16#ffff).
-define(BBOARD, 16#ffff000000000000).
-define(DRAW_LINE, io:format("+--+--+--+--+--+--+--+--+~n", [])).
-define(EMPTY_GAME, #game{w=0, b=0, main_board=#board{p=0, b=0, n=0, r=0, q=0, k=0, s=array:new([{size,64},{fixed,true},{default,0}])}}).
-define(START_POS, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").

-record(board, {p, b, n, r, q, k, s}).
-record(engine, {process}).
-record(game, {w, b, main_board, turn}).

init() -> 
	{ok, Engine} = new_engine(),
	loop(Engine).

loop(#engine{}=Engine) ->
	case io:get_line("") of
       		eof -> ok;
       		{error,Reason} -> io:format("read error:~p\n",[Reason]);
       		"quit" ++ _ ->
			stop(Engine), 
			quit();
		Data ->	
			case uci(Data, Engine) of
				{ok, NewEngine} ->
					loop(NewEngine);
				_ -> 
					io:format("Invalid Procedure: ~p on Engine:~p~n", [Data, Engine])
			end
    	end.

uci("ucinewgame" ++ _, E) ->
        new_engine(E);

uci("uci" ++ _, E) ->
	io:format("id name echess~n",[]),
	io:format("id auhtor Thiago~n",[]),
	?UCIOK,
	{ok, E};

uci("position startpos moves " ++ L, #engine{process=P}=E) ->
	P ! {position, L, E},
	{ok, E}; 

uci("print" ++ __, #engine{process=P}=E) ->
	P ! print,
	{ok, E};

uci(_, E) -> {ok, E}.

new_engine(#engine{}=Engine) ->
	stop(Engine),
	new_engine().
new_engine() ->
	Engine = #engine{process = spawn(fun () -> p(new_game()) end)},
	{ok, Engine}.

new_game() ->
	EMPTY = ?EMPTY_GAME,
	read_fen(?START_POS, EMPTY).

read_fen(S, #game{}=Game) -> read_fen(S, 63, Game).

read_fen([], _, #game{}=Game) -> Game;
read_fen([$/|T], I, #game{}=Game) -> read_fen(T, I, Game);
read_fen([N|T], I, #game{}=Game) when N >= $1, N =< $9 -> 
	NN = (N - $1) + 1,
	read_fen(T, I - NN, Game);
read_fen([H|T], I, #game{}=Game) -> 
	{Color, Piece} = get_info(H),
	read_fen(T, I-1, put_piece(Color, Piece, I, Game)). 

get_info($p) -> {#game.w, #board.p};
get_info($r) -> {#game.w, #board.r};	 
get_info($n) -> {#game.w, #board.n};
get_info($b) -> {#game.w, #board.b};
get_info($q) -> {#game.w, #board.q};
get_info($k) -> {#game.w, #board.k};
get_info($P) -> {#game.b, #board.p};
get_info($R) -> {#game.b, #board.r};
get_info($N) -> {#game.b, #board.n};
get_info($B) -> {#game.b, #board.b};
get_info($Q) -> {#game.b, #board.q};
get_info($K) -> {#game.b, #board.k}.

stop(#engine{}=Engine) ->
	Engine#engine.process ! stop,
	ok.

quit() -> ok.

p(#game{}=Game) ->
	receive
		stop -> ok;
		{move, _, _} -> p(Game);
		{position, L, E} -> 
			NewGame = setup_board(string:tokens(L, " "), Game),
			p(NewGame);
		print -> 
			print_game(Game),
			p(Game)
	end.

setup_board([], #game{}=Game) -> Game;
setup_board([H|T], #game{}=Game) ->
	NewGame = process_move(H, Game),
	setup_board(T, NewGame).

get_pos(C, L) ->
	CV = 7 - (C - $a),
        LV = (L - $1) * 8,
	CV + LV.	

process_move([C1,L1,C2,L2,_], Game) -> process_move([C1,L1,C2,L2], Game);
process_move([C1,L1,C2,L2], Game) ->
	P1  = get_pos(C1, L1),
	P2  = get_pos(C2, L2),
	io:format("M: ~p ~p~n", [P1, P2]),
	
	%%%RGame=remove_piece(P1, Game),
	%%%PGame=put_piece(P2, RGame),
	Game. 

remove_piece(Pos, #game{main_board=#board{s=S}}=Game) ->
	{Color, Piece}=array:get(Pos, S),
	remove_piece(Piece, Pos, Game).
remove_piece(Piece, Pos, #game{main_board=#board{s=S}=MB}=Game) ->
	PType=array:get(Piece, S),
	NewS=array:set(Piece, -1, S),
	%%%% MB#board
	Game.

put_piece(Color, Piece, Pos, #game{main_board=#board{s=S}=MB}=Game) ->
	BPos = 1 bsl Pos,
	io:format("Setting Array: ~p on Pos:~p Value:~p", [S, Pos, {Color,Piece}]),	
	S0 = array:set(Pos, {Color, Piece}, S),
	io:format("S[~p] -~p ~n", [S0, Piece]),
	P = element(Piece, MB),
	NP = P bor BPos,
	MB0 = setelement(Piece, MB, NP),
	io:format("P[~p][~p]~n", [P, NP]),
	C = element(Color, MB0),
	NC = C bor BPos,
	MB1 = setelement(Color, MB0, NC),
	io:format("C[~p][~p]~n", [C, NC]),
	Game#game{main_board= MB1#board{ s=S0 } }.

print_game(#game{main_board=#board{s=S}}) ->	
	?DRAW_LINE,
	print_game(S, 63).

print_game(Array, -1) -> io:format("|~n", []), ?DRAW_LINE;
print_game(Array, N) ->
	X = (N+1) rem 8,
        case X == 0 andalso N > 0  of
                true -> io:format("|~n", []), ?DRAW_LINE;
                _ -> ok
        end,
	io:format("|~p", [array:get(N,Array)]),
	print_game(Array, N-1).

