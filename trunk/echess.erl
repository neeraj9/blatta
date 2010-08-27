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
	gen_pawn_moves(Color, Game#game{}) ++ gen_knight_moves(Color, Game#game{}).

%%    __
%%   (  )
%%    ||
%%   /__\
%%  (____)

gen_pawn_moves(Color, #game{main_board=#board{p=P}}=Game) ->
	CPieces = element(Color, Game),
	Pawns = CPieces band P,
	get_pawn_moves(Color, Game, Pawns).

get_pawn_moves(_, _, 0) -> [];
get_pawn_moves(Color, #game{main_board=#board{s=S}}=Game, Pawns) ->
	case next_index(Pawns) of
		{-1, _} -> [];
		{Pos, L} -> 
			TT = get_pawn_moves_on_pos(Color, S, Pos),
			TT ++ get_pawn_moves(Color, Game, L);
		X -> io:format("ERROR WRONG INDEX: ~p~n", [X])
	end.

get_pawn_moves_on_pos(Color, S, Pos) ->
    QM = get_pawn_range(Color, Pos),    
    AM = get_pawn_range_a(Color, Pos),
    remove_colisions(Pos, QM, S) ++ only_captures(Pos, AM, S, Color).

remove_colisions(_, [], _) -> [];
remove_colisions(Pos, [H|T], S) -> 
    case array:get(H, S) == 0 of
	true -> 
	    [ {quiet, Pos, H} | remove_colisions(Pos, T, S)];
	_ ->
	    []
    end.

is_capture(Pos, S, Color) ->
    case array:get(Pos, S) of
			     {#game.w, _} -> Color == #game.b;
			     {#game.b, _} -> Color == #game.w;
			     _ -> false
    end.

only_captures(Pos, L, S, Color) -> 
    lists:map(fun(Y) -> {attack, Pos, Y} end, lists:filter(fun(X) -> is_capture(X, S, Color) end, L)). 

get_pawn_range_a(#game.w, Pos) when Pos rem 8 == 0 -> [Pos + 9];
get_pawn_range_a(#game.w, Pos) when Pos rem 8 == 7 -> [Pos + 7];
get_pawn_range_a(#game.w, Pos) -> [Pos + 9, Pos + 7];
get_pawn_range_a(#game.b, Pos) when Pos rem 8 == 7 -> [Pos - 9];
get_pawn_range_a(#game.b, Pos) when Pos rem 8 == 0 -> [Pos - 7];
get_pawn_range_a(#game.b, Pos) -> [Pos - 9, Pos - 7].

get_pawn_range(#game.w, Pos) when Pos < 16#10000 -> [Pos + 8, Pos + 16];
get_pawn_range(#game.b, Pos) when Pos > 16#800000000000 -> [Pos - 8, Pos - 16];
get_pawn_range(#game.w, Pos) -> [Pos + 8];
get_pawn_range(#game.b, Pos) -> [Pos - 8]. 

%%     __/|
%%  __/o   \
%% {==      |
%%    \    /
%%    /__<
%%   /____\
%%  (______)

gen_knight_moves(Color, #game{main_board=#board{n=N}}=Game) ->
	CPieces = element(Color, Game),
	Knights = CPieces band N,
	get_knight_moves(Color, Game, Knights).

get_knight_moves(Color, #game{main_board=#board{s=S}}=Game, Knights) ->
    	get_knight_moves(Color, 
get_knight_moves(Color, S, L, BoardColor, EnemyBoard) ->
    case next_index(Knights) of
		{-1, _} -> [];
		{Pos, L} -> 
			TT = get_knight_moves_on_pos(Color, S, Pos, CBoard),
			TT ++ get_knight_moves(Color, Game, L);
		X -> io:format("ERROR WRONG INDEX: ~p~n", [X])
	end.

get_knight_moves_on_pos(Color, S, Pos) ->
    lists:map(fun(X) -> {quiet, Pos, X} end, get_indexes(element(Pos + 1, ?KNIGHT_MOVES)) band (bnot element(Color, )).

get_indexes(I) ->
	case next_index(I) of
		{-1, _} -> [];
		{Pos, L} -> [ Pos | get_indexes(L) ]
				end.
