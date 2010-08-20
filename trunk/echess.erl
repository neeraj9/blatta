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

uci("ucinewgame" ++ _, E) ->
        new_engine(E);

uci("uci" ++ _, E) ->
	io:format("id name echess~n",[]),
	io:format("id auhtor Thiago~n",[]),
	?UCIOK,
	{ok, E};

uci("position startpos moves " ++ L, #engine{process=P}=E) ->
	P ! {position, L},
	{ok, E}; 

uci("genmoves" ++ _, #engine{process=P}=E) ->
	P ! {genmoves},
	{ok, E};

uci("print" ++ _, #engine{process=P}=E) ->
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

stop(#engine{}=Engine) ->
	Engine#engine.process ! stop,
	ok.

quit() -> ok.

p(#game{turn=C}=Game) ->
	receive
		stop -> ok;
		{move, _, _} -> p(Game);
		{position, L} -> 
			NewGame = setup_board(string:tokens(L, " "), Game),
			p(NewGame);
		{genmoves} ->
			gen_moves(C, Game);
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
process_move([C1,L1,C2,L2], #game{main_board=#board{s=S}}=Game) ->
	P1  = get_pos(C1, L1),
	P2  = get_pos(C2, L2),
	io:format("M: ~p ~p~n", [P1, P2]), 
	{{Color, Piece}=P, RGame}=remove_piece(P1, Game),
	put_piece(Color, Piece, P2, RGame). 

remove_piece(Pos, #game{main_board=#board{s=S}}=Game) ->
	{Color, Piece}=array:get(Pos, S),
	io:format("P to Rem: ~p~n", [{Color, Piece}]),
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

print_game(#game{main_board=#board{s=S}}=Game) ->	
	?DRAW_LINE,
	print_game(S, 63). %%%	get_pawn_moves(#game.w, 

print_game(Array, -1) -> io:format("|~n", []), ?DRAW_LINE;
print_game(Array, N) ->
	X = (N+1) rem 8,
        case X == 0 andalso N < 63  of
                true -> io:format("|~n", []), ?DRAW_LINE;
                _ -> ok
        end,
	io:format("|~p", [array:get(N,Array)]),
	print_game(Array, N-1).

gen_moves(Color, #game{}=Game) ->
	io:format("gn: [~p:~p]", [Color, Game]),
	gen_pawn_moves(Color, Game#game{}).

gen_pawn_moves(Color, #game{main_board=#board{p=P}}=Game) ->
	io:format("gnm: [~p:~p]", [Color, Game]),
	CPieces = element(Color, Game),
	io:format("gnm2: [~p:~p]", [CPieces, P]),
	Pawns = CPieces band P,
	io:format("gnm3: [~p]~n", [Pawns]),
	get_pawn_moves(Color, Game, Pawns, []).

get_pawn_moves(_, _, 0, T) -> T;
get_pawn_moves(Color, #game{main_board=#board{s=S}}=Game, Pawns, T) ->
	case next_index(Pawns) of
		{-1, _} -> T;
		{Pos, L} -> 
			case Color of
				#game.w ->
					M = Pos bsl 8;
				_ ->
					M = Pos bsr 8
			end,
			io:format("TEST M: ~p S:~p~n", [M, S]),
			case M band S of
				0 ->
					Move = {quiet, Pos, M},
					get_pawn_moves(Color, Game, L, [Move | T]);
				_ ->	get_pawn_moves(Color, Game, L, T)
			end;
		X -> io:format("ERROR WRONG INDEX: ~p", [X])
	end.
		 
next_index(0) -> {-1, 0};
next_index(L) ->
	B=bsf(L),
	{B, L - B}.
