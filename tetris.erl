-module(tetris).
-import(tetrominoes,[fetchTetromino/2]).
-export([start/0, gameLoop/5, board/2, output/1, moveQueue/1, descendMoveGenerator/1, printFunction/2]).

-define(WIDTH, 10).
-define(HEIGHT, 15).
-define(TETROBLOCK, $B).
-define(DESCENDPERIOD, 250).
-define(TETROSTARTPOS, {4,1}).

%=============================================================================
%                                 START
%=============================================================================
%
% Configures all the required processes to run the core tetris game loop.
%
start() ->
    Board = lists:duplicate(?HEIGHT,[160,160,160,160,160,160,160,160,160,160]),
    MoveQueuePID         = spawn(?MODULE, moveQueue, [{0,0,0}]),
    DescendMoveGenerator = spawn(?MODULE, descendMoveGenerator, [MoveQueuePID]),
    BoardPID             = spawn(?MODULE, board, [Board, placeholder]),
    GameLoop             = spawn(?MODULE, gameLoop, [tetrominoLanded, Board, placeholder, BoardPID, MoveQueuePID]),
    Output               = spawn(?MODULE, output, [BoardPID]).


%=============================================================================
%                        DESCEND MOVE GENERATOR
%=============================================================================
%
% Sends the descend move to the move queue every ?DESCENDPERIOD mil-seconds,
% Basically determines how quickly the tetrominoes will descend.
%
descendMoveGenerator(MoveQueuePID) ->
    timer:send_after(?DESCENDPERIOD, tick),
    receive
        tick ->
            MoveQueuePID ! {move, {0, 1, 0}},
            descendMoveGenerator(MoveQueuePID)
    end.

%=============================================================================
%                              MOVE QUEUE
%=============================================================================
%
% Stores all the moves either sent by the AI or the descend move generator.
% Ready to send the accumulated moves to the core game loop when requested.
% {move in x dir, move in y dir, num of times to rotate 90 deg clockwise}
%
moveQueue({0, 0, 0}) ->
    receive
        {move, MoveVec} ->
            moveQueue(MoveVec);
        {getMove, ReturnPID} ->
            ReturnPID ! {sendMove, {0, 0, 0}},
            moveQueue({0, 0, 0})
    end;

moveQueue(AccMove) ->
    receive
        {move, MoveVec} ->
            moveQueue(addMoves(AccMove, MoveVec));
        {getMove, ReturnPID} ->
            ReturnPID ! {sendMove, AccMove},
            moveQueue({0, 0, 0})
    end.

%=============================================================================
%                              ADD MOVES
%=============================================================================
%
% Simple function to add together two moves.
%
addMoves({X1, Y1, R1}, {X2, Y2, R2}) ->
    {X1 + X2, Y1 + Y2, R1 + R2}.

%=============================================================================
%                       GEN RANDOM TETROMINO
%=============================================================================
%
% Generates a random tetromino.
%
genTetromino(Board) ->
    Tetrominoes = [i,j,l,o,s,t,z],
    RandIndex = 1,
    RandRotation = rand:uniform(4),
    Tetromino = lists:nth(RandIndex, Tetrominoes),
    case validTetrominoPos(tetrominoes:fetchTetromino(Tetromino, RandRotation), Board, ?TETROSTARTPOS) of
        true ->
            {lists:nth(RandIndex, Tetrominoes), RandRotation, ?TETROSTARTPOS};
        false ->
            gameOver
    end.

%=============================================================================
%                          LOOK UP BOARD POS
%=============================================================================
%
% Function returns contents of board cell in relation to the given coords.
% Returns the value of the tetromino cell value if coords are out of range/invalid.
%
lookUpBoardPos(X, Y, Board) ->
    if
        X > ?WIDTH ->
            ?TETROBLOCK;
        Y > ?HEIGHT ->
            ?TETROBLOCK;
        true ->
            lists:nth(X, lists:nth(Y, Board))
    end.

%=============================================================================
%                              VALID TETROMINO POS
%=============================================================================
%
% Function when given tetromino, its position and the board will return true/false
% atom on weather the tetromino and position pair is a valid state for the board.
%
validTetrominoPos([], _, _) ->
    true;

validTetrominoPos([[TetX, TetY] | TetTail], Board, {PosX, PosY}) ->
    case lookUpBoardPos(PosX + TetX, PosY + TetY, Board) of
        ?TETROBLOCK ->
            false;
        160 ->
            validTetrominoPos(TetTail, Board, {PosX, PosY})
    end.

%=============================================================================
%                         APPLY FUNC TO BOARD/ROW
%=============================================================================
%
% These pair of functions apply a function accross the board and current tetromino
% cells. For example printing the board or writing a tetromino to the board.
%

applyFuncToRow(BoardRow, Func, []) ->
    Func(restOfBoardRow, BoardRow);

applyFuncToRow(BoardRow, Func, TetrominoRowPos) ->
    PrintX = 1,
    applyFuncToRow(BoardRow, Func, TetrominoRowPos, PrintX).

applyFuncToRow(BoardRow, Func, [], _) ->
    applyFuncToRow(BoardRow, Func, []);

applyFuncToRow([BoardCell | BoardTail], Func, [TetX | TetTail], PrintX) ->
    if
        TetX == PrintX ->
            [Func(tetroCell, ?TETROBLOCK)] ++ applyFuncToRow(BoardTail, Func, TetTail, PrintX + 1);
        true ->
            [Func(boardCell, BoardCell)] ++ applyFuncToRow(BoardTail, Func, [TetX | TetTail], PrintX + 1)
    end.

applyFuncToBoard(Board, Func, {Tetromino, {PosX, PosY}}) ->
    YCoord = 1,
    applyFuncToBoard(Board, Func, {Tetromino, {PosX, PosY}}, YCoord).

applyFuncToBoard([], _, _, _) ->
    [];

applyFuncToBoard([BoardRow|BoardTail], Func, {Tetromino, {PosX, PosY}}, YCoord) ->
    [applyFuncToRow(BoardRow, Func, [PosX + TetX || [TetX, TetY] <- Tetromino, PosY + TetY == YCoord])] 
        ++ applyFuncToBoard(BoardTail, Func, {Tetromino, {PosX, PosY}}, YCoord + 1).

%=============================================================================
%                              PRINT FUNCTION
%=============================================================================
%
% Designed to be used with the applyFuncToBoard function, this function is used
% to print the board to the console.
%
printFunction(restOfBoardRow, BoardRow) ->
    io:fwrite("~s", [[BoardRow]]),
    io:fwrite("~s~n", [[$|]]);

printFunction(boardCell, BoardCell) ->
    io:fwrite("~s", [[BoardCell]]);

printFunction(tetroCell, TetroBlock) ->
    io:fwrite("~s", [[TetroBlock]]).

%=============================================================================
%                       WRITE TETROMINO TO BOARD
%=============================================================================
%
% 
%
writeTetrominoToBoard(restOfBoardRow, BoardRow) ->
    BoardRow;

writeTetrominoToBoard(boardCell, BoardCell) ->
    BoardCell;

writeTetrominoToBoard(tetroCell, TetroBlock) ->
    TetroBlock.

%=============================================================================
%                          UPDATE GAME STATE
%=============================================================================
%
% 
%
updateGameState(Board, {_, Pos}, {0,0}) ->
    {tetrominoDescending, Board, Pos};

updateGameState(Board, {Tetromino, {PosX, PosY}}, {MoveX, MoveY}) ->
    NewPos = {PosX + MoveX, PosY + MoveY},
    case validTetrominoPos(Tetromino, Board, NewPos) of
        true ->
            {tetrominoDescending, Board, NewPos};
        false ->
            if 
                MoveY == 1 ->
                    {tetrominoLanded, 
                     applyFuncToBoard(Board, fun writeTetrominoToBoard/2, {Tetromino, {PosX, PosY}}), 
                     {PosX, PosY}};
                true ->
                    {tetrominoDescending, Board, {PosX, PosY}}
            end
    end.

%=============================================================================
%                           GET ROTATE VALUE
%=============================================================================
%
% Calculate new rotation value using existin rotation value and the rotation from
% the move.
%
getNewRotation(Rotation, 0) ->
    Rotation;
getNewRotation(_, NewRotation) ->
    NewRotation.

%=============================================================================
%                              GAME LOOP
%=============================================================================
%
% 
%
gameLoop(tetrominoDescending, Board, {Tetromino, Rotation, Pos}, BoardPID, MoveQueuePID) ->
    %fetch moves bro
    MoveQueuePID ! {getMove, self()},
    receive
        {sendMove, {MoveX, MoveY, MoveRotate}} ->
            NewRotation = getNewRotation(Rotation, MoveRotate),
            {GameState, UpdatedBoard, UpdatedTetPos} = updateGameState(Board, 
                                                                       {tetrominoes:fetchTetromino(Tetromino, NewRotation), Pos},
                                                                       {MoveX, MoveY}),
            BoardPID ! {sendBoard, UpdatedBoard, {Tetromino, NewRotation, UpdatedTetPos}},
            gameLoop(GameState, UpdatedBoard, {Tetromino, NewRotation, UpdatedTetPos}, BoardPID, MoveQueuePID)
    end;

gameLoop(tetrominoLanded, Board, _, BoardPID, MoveQueuePID) ->
    case genTetromino(Board) of
        gameOver ->
            io:format("GAME OVER :( ~n",[]);
        {Tetromino, Rotation, Pos} ->
            BoardPID ! {sendBoard, Board, {Tetromino, Rotation, Pos}},
            gameLoop(tetrominoDescending, Board, {Tetromino, Rotation, Pos}, BoardPID, MoveQueuePID)
    end.

%=============================================================================
%                               BOARD
%=============================================================================
%
% 
%
board(Board, TetrominoInfo) ->
    receive
        {sendBoard, NewBoard, NewTetrominoInfo} ->
            board(NewBoard, NewTetrominoInfo);
        {getBoard, ReturnPID} ->
            ReturnPID ! {sendBoard, Board, TetrominoInfo},
            board(Board, TetrominoInfo)
    end.

%=============================================================================
%                               OUTPUT
%=============================================================================
%
% 
%
output(BoardPID) ->
    timer:send_after(?DESCENDPERIOD, display),
    receive
        display ->
            BoardPID ! {getBoard, self()},
            receive
                {sendBoard, Board, {Tetromino, Rotation, Pos}} ->
                    _ = applyFuncToBoard(Board, fun printFunction/2, {tetrominoes:fetchTetromino(Tetromino, Rotation), Pos}),
                    io:fwrite("~s~n", [[$-,$-,$-,$-,$-,$-,$-,$-,$-,$-,$|]]),
                    output(BoardPID)
            end
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
