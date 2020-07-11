-module(tetris).
-import(tetrominoes,[fetchTetromino/2]).
-export([start/0, gameLoop/4, board/2, output/1]).

-define(WIDTH, 10).
-define(HEIGHT, 15).

%printRow([],_,_) ->
%    io:fwrite("~n", []);

printRow(BoardRow, []) ->
    io:fwrite("~s", [[BoardRow]]),
    io:fwrite("~s~n", [[$|]]);

printRow([BoardCell | BoardTail], [TetX | TetTail]) ->
    PrintX = 1,
    printRow([BoardCell | BoardTail], [TetX | TetTail], PrintX).

printRow(BoardRow, [], _) ->
    printRow(BoardRow, []);

printRow([BoardCell | BoardTail], [TetX | TetTail], PrintX) ->
    if
        TetX == PrintX ->
            io:fwrite("~s", [[$B]]),
            printRow(BoardTail, TetTail, PrintX + 1);
        true ->
            io:fwrite("~s", [[BoardCell]]),
            printRow(BoardTail, [TetX | TetTail], PrintX + 1)
    end.

displayBoard([BoardRow|BoardTail], {Tetromino, [PosX, PosY]}) ->
    PrintY = 1,
    displayBoard([BoardRow|BoardTail], {Tetromino, [PosX, PosY]}, PrintY).

displayBoard(_, _, ?HEIGHT+1) ->
    io:fwrite("~s~n", [[$|,$_,$_,$_,$_,$_,$_,$_,$_,$_,$_,$|]]);

displayBoard([BoardRow|BoardTail], {Tetromino, [PosX, PosY]}, PrintY) ->
    io:fwrite("~s", [[$|]]),
    printRow(BoardRow, [PosX + TetX || [TetX, TetY] <- Tetromino, PosY + TetY == PrintY]),
    displayBoard(BoardTail, {Tetromino, [PosX, PosY]}, PrintY + 1).

descendMoveGenerator(MoveQueuePID) ->
    timer:send_after(1000, tick),
    receive
        tick ->
            MoveQueuePID ! {move, [0,-1]},
            descendMoveGenerator(MoveQueuePID)
    end.

moveQueue({0, 0}) ->
    receive
        {move, MoveVec} ->
            moveQueue(MoveVec);
        {getMove, ReturnPID} ->
            ReturnPID ! {currMove, {0, 0}},
            moveQueue({0, 0})
    end;

moveQueue(AccMove) ->
    receive
        {move, MoveVec} ->
            moveQueue(addMoves(AccMove, MoveVec));
        {getMove, ReturnPID} ->
            ReturnPID ! {currMove, AccMove},
            moveQueue({0, 0})
    end.

addMoves({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.

start() ->
    Board = lists:duplicate(?HEIGHT,[160,160,160,160,160,160,160,160,160,160]),
    MoveQueue            = spawn(?MODULE, moveQueue, [{0,0}]),
    DescendMoveGenerator = spawn(?MODULE, descendMoveGenerator, [MoveQueue]),
    BoardPID             = spawn(?MODULE, board, [Board, placeholder]),
    GameLoop             = spawn(?MODULE, gameLoop, [tetrominoLanded, Board, placeholder, BoardPID]),
    Output               = spawn(?MODULE, output, [BoardPID]).

lookUpBoardPos(X, Y, Board) ->
    if
        X > ?WIDTH ->
            $B;
        Y > ?HEIGHT ->
            $B;
        true ->
            list:nth(X, list:nth(Y, Board))
    end.


%validTetrominoPos([], _, _) ->
%    true;
%
%validTetrominoPos([[TetX, TetY] | TetTail], Board, [PosX, PosY]) ->
%    case lookUpBoardPos(PosX + TetX, PosY + TetY, Board) of
%        $B ->
%            false;
%        160 ->
%            validTetrominoPos(TetTail, Board, [PosX, PosY])
%    end.
%
%getHorizontalPos(_, Board, [?WIDTH+1,_]) ->
%    [];
%
%getHorizontalPos(Tetromino, Board, [X, Y]) ->
%    case validTetrominoPos(Tetromino, Board, [X, Y]) of
%        true ->
%            [[X, Y]] ++ getHorizontalPos(Tetromino, Board, [X+1, Y]);
%        false ->
%            getHorizontalPos(Tetromino, Board, [X+1, Y])
%    end.

randTetromino() ->
    Tetrominoes = [i,j,l,o,s,t,z],
    RandIndex = 1,
    RandRotation = rand:uniform(4),
    {lists:nth(RandIndex, Tetrominoes), RandRotation, [4,0]}.


gameLoop(tetrominoDescending, Board, {Tetromino, Rotation, Pos}) ->
    %fetch moves bro

gameLoop(tetrominoLanded, Board, _, BoardPID) ->
    {Tetromino, Rotation, Pos} = randTetromino(),
    BoardPID ! {sendBoard, Board, {Tetromino, Rotation, Pos}},
    gameLoop(tetrominoDescending, Board, {Tetromino, Rotation, Pos}, BoardPID).

board(Board, TetrominoInfo) ->
    receive
        {sendBoard, NewBoard, NewTetrominoInfo} ->
            board(NewBoard, NewTetrominoInfo);
        {getBoard, ReturnPID} ->
            ReturnPID ! {sendBoard, Board, TetrominoInfo},
            board(Board, TetrominoInfo)
    end.

output(BoardPID) ->
    timer:send_after(17, tick),
    receive
        tick ->
            BoardPID ! {getBoard, self()},
            receive
                {sendBoard, Board, {Tetromino, Rotation, Pos}} ->
                    displayBoard(Board, {tetrominoes:fetchTetromino(Tetromino, Rotation), Pos}),
                    output(BoardPID)
            end
    end.