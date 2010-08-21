-module(echess).
-compile(export_all).
-include("blatta.hrl").

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

uci("ucinewgame" ++ _, E) -> new_engine(E);
uci("uci" ++ _, E) ->
	io:format("id name echess~n",[]),
	io:format("id auhtor Thiago~n",[]),
	?UCIOK,
	{ok, E};
uci("position startpos moves " ++ L, #engine{process=P}=E) -> P ! {position, L}, {ok, E}; 
uci("genmoves" ++ _, #engine{process=P}=E) -> P ! {genmoves}, {ok, E};
uci("print" ++ _, #engine{process=P}=E) -> P ! print, {ok, E};
uci(_, E) -> {ok, E}.

new_engine(#engine{}=Engine) -> stop(Engine), new_engine().
new_engine() -> Engine = #engine{process = spawn(fun () -> p(new_game()) end)}, {ok, Engine}.

new_game() -> EMPTY = ?EMPTY_GAME, read_fen(?START_POS, EMPTY).

read_fen(S, #game{}=Game) -> read_fen(S, 63, Game).

read_fen([], _, #game{}=Game) -> Game;
read_fen([$/|T], I, #game{}=Game) -> read_fen(T, I, Game);
read_fen([N|T], I, #game{}=Game) when N >= $1, N =< $9 -> 
	NN = (N - $1) + 1,
	read_fen(T, I - NN, Game);
read_fen([H|T], I, #game{}=Game) -> 
	{Color, Piece} = get_info(H),
	read_fen(T, I-1, put_piece(Color, Piece, I, Game)). 

stop(#engine{}=Engine) -> Engine#engine.process ! stop, ok.

quit() -> ok.

p(#game{turn=C}=Game) ->
	receive
		stop -> ok;
		{move, _, _} -> p(Game);
		{position, L} -> 
			NewGame = setup_board(string:tokens(L, " "), Game),
			p(NewGame);
		{genmoves} ->
			M=gen_moves(C, Game),
			print_moves(M, Game),
			p(Game);
		print -> 
			print_game(Game),
			p(Game)
	end.

setup_board([], #game{}=Game) -> Game;
setup_board([H|T], #game{}=Game) ->
	NewGame = process_move(H, Game),
	setup_board(T, NewGame).

process_move([C1,L1,C2,L2,_], Game) -> process_move([C1,L1,C2,L2], Game);
process_move([C1,L1,C2,L2], #game{}=Game) ->
	P1  = get_pos(C1, L1),
	P2  = get_pos(C2, L2),
	process_move(P1, P2, Game).
process_move(P1, P2, #game{}=Game) ->
        {{Color, Piece}, RGame}=remove_piece(P1, Game),
        put_piece(Color, Piece, P2, RGame).

remove_piece(Pos, #game{main_board=#board{s=S}}=Game) ->
	{Color, Piece}=array:get(Pos, S),
	remove_piece(Color, Piece, Pos, Game).
remove_piece(Color, Piece, Pos, #game{main_board=#board{s=S}=MB}=Game) ->
	BPos = 1 bsl Pos,
	S0 = array:set(Pos, 0, S),
	PV = element(Piece, MB) - BPos, 
	MB0 = setelement(Piece, MB, PV),
	CV = element(Color, Game) - BPos, 	
	G0 = setelement(Color, Game, CV),
	{{Color, Piece}, G0#game{main_board=MB0#board{s=S0}}}.

put_piece(Color, Piece, Pos, #game{main_board=#board{s=S}=MB}=Game) ->
	BPos = 1 bsl Pos,
	S0 = array:set(Pos, {Color, Piece}, S),
	P = element(Piece, MB),
	NP = P bor BPos,
	MB0 = setelement(Piece, MB, NP),
	C = element(Color, Game),
	NC = C bor BPos,
	G = setelement(Color, Game, NC),
	G#game{main_board=MB0#board{s=S0}}.

gen_moves(Color, #game{}=Game) -> 
	gen_pawn_moves(Color, Game#game{}).

gen_pawn_moves(Color, #game{main_board=#board{p=P}}=Game) ->
	CPieces = element(Color, Game),
	Pawns = CPieces band P,
	get_pawn_moves(Color, Game, Pawns, []).

get_pawn_moves(_, _, 0, T) -> T;
get_pawn_moves(Color, #game{main_board=#board{s=S}}=Game, Pawns, T) ->
	case next_index(Pawns) of
		{-1, _} -> T;
		{Pos, L} -> 
			M = get_pawn_range(Color, Pos),
			case M of
				{M1, M2} -> 
					T1 = gen_quiet(Pos, M1, S, T),
					TT = gen_quiet(Pos, M2, S, T1);
				_ ->
					TT = gen_quiet(Pos, M, S, T)
			end,
			get_pawn_moves(Color, Game, L, TT);
		X -> io:format("ERROR WRONG INDEX: ~p~n", [X])
	end.

gen_quiet(Pos, M, S, T) ->
	case array:get(M, S) of
        	0 -> [{quiet, Pos, M}|T];
        	_ -> T
	end.

