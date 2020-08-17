-module(tetris).
-import(tetrominoes,[fetchTetromino/2]).
-import(ai, [aiCoreLoop/2]).
-export([start/0, gameLoop/7, board/4, output/1, moveQueue/1, descendMoveGenerator/2, printFunction/2, validTetrominoPos/3, applyFuncToBoard/3, updateBoard/2]).

-define(WIDTH, 10).
-define(HEIGHT, 15).
-define(TETROBLOCK, $B).
-define(DESCENDPERIOD, 250).
-define(TETROSTARTPOS, {4,1}).
-define(STARTLEVEL, 0).
-define(CLEAREDROWSFORNEXTLEVEL, 10).


%=============================================================================
%                              MISC FUNCS
%=============================================================================
%
% Some misc func to aid in code clarity
%
emptyRow() ->
    [160,160,160,160,160,160,160,160,160,160].

fullRow() ->
    [?TETROBLOCK,?TETROBLOCK,?TETROBLOCK,?TETROBLOCK,?TETROBLOCK,
     ?TETROBLOCK,?TETROBLOCK,?TETROBLOCK,?TETROBLOCK,?TETROBLOCK].

%=============================================================================
%                                 START
%=============================================================================
%
% Configures all the required processes to run the core tetris game loop.
%
start() ->
    Board = lists:duplicate(?HEIGHT,emptyRow()),
    MoveQueuePID         = spawn(?MODULE, moveQueue, [{0,0,0}]),
    BoardPID             = spawn(?MODULE, board, [Board, placeholder, [], {?STARTLEVEL,0,0}]),
    DescendMoveGenerator = spawn(?MODULE, descendMoveGenerator, [MoveQueuePID, BoardPID]),
    GameLoop             = spawn(?MODULE, gameLoop, [tetrominoLanded, Board, placeholder, {?STARTLEVEL,0,0}, BoardPID, MoveQueuePID, self()]),
    AIPID                = spawn(ai,      aiCoreLoop, [BoardPID, MoveQueuePID]),
    Output               = spawn(?MODULE, output, [BoardPID]),
    receive
        gameOver ->
            exit(AIPID, kill),
            exit(GameLoop, kill),
            exit(DescendMoveGenerator, kill),
            exit(Output, kill),
            exit(BoardPID, kill),
            exit(MoveQueuePID, kill)   
    end.


%=============================================================================
%                        DESCEND MOVE GENERATOR
%=============================================================================
%
% Sends the descend move to the move queue at an interval dependant on the current
% level of the tetris game.
% Basically determines how quickly the tetrominoes will descend.
%
descendMoveGenerator(MoveQueuePID, BoardPID) ->
    BoardPID ! {getLevel, self()},
    receive
        {sendLevel, {Level,_,_}} ->
            timer:send_after(tetrominoes:getSpeed(Level), tick),
            receive
                tick ->
                    MoveQueuePID ! {move, {0, 1, 0}},
                    descendMoveGenerator(MoveQueuePID, BoardPID)
            end
    end.

%=============================================================================
%                              MOVE QUEUE
%=============================================================================
%
% Stores all the moves either sent by the AI or the descend move generator.
% Sends the accumulated moves to the core game loop when requested.
%
% A move is represented as a 3-turple below : 
% {move in x dir, move in y dir, tetromino rotation}
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
% Generates a random tetromino, either returns a random valid tetromino or
% an atom expressing that the game over state has been reached (no valid tetromino
% can be generated). 
%
genTetromino(Board) ->
    Tetrominoes = [i,j,l,o,s,t,z],
    RandIndex = rand:uniform(length(Tetrominoes)),
    RandRotation = rand:uniform(tetrominoes:numOfUniqueRotations(lists:nth(RandIndex, Tetrominoes))),
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
% Returns the value representing the tetromino cell value if coords are out 
% of range/invalid.
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
% Function when given tetromino, its position and the board; will return true/false
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
%                         APPLY FUNC TO BOARD
%=============================================================================
%
% This function allows for a function to be applied to each row of the board.
%
applyFuncToBoard(Board, Func, {Tetromino, {PosX, PosY}}) ->
    YCoord = 1,
    applyFuncToBoard(Board, Func, {Tetromino, {PosX, PosY}}, YCoord).

applyFuncToBoard([], _, _, _) ->
    [];

applyFuncToBoard([BoardRow|BoardTail], Func, {Tetromino, {PosX, PosY}}, YCoord) ->
    [Func(BoardRow, [PosX + TetX || [TetX, TetY] <- Tetromino, PosY + TetY == YCoord])] 
        ++ applyFuncToBoard(BoardTail, Func, {Tetromino, {PosX, PosY}}, YCoord + 1).

%=============================================================================
%                              PRINT FUNCTION
%=============================================================================
%
% Designed to be used with the applyFuncToBoard function, this function is used
% to print the board to the console.
%

printFunction(BoardRow, [])->
    io:fwrite("~s", [[BoardRow]]),
    io:fwrite("~s~n", [[$|]]);

printFunction(BoardRow, TetrominoRowPos) ->
    PrintX = 1,
    printFunction(BoardRow, TetrominoRowPos, PrintX).

printFunction(BoardRow, [], _) ->
    printFunction(BoardRow, []);

printFunction([BoardCell | BoardTail], [TetX | TetTail], PrintX) ->
    if
        TetX == PrintX ->
            io:fwrite("~s", [[?TETROBLOCK]]),
            printFunction(BoardTail, TetTail, PrintX + 1);
        true ->
            io:fwrite("~s", [[BoardCell]]),
            printFunction(BoardTail, [TetX | TetTail], PrintX + 1)
    end.

%=============================================================================
%                  UPDATE BOARD/ WRITE TETROMINO TO BOARD
%=============================================================================
%
% Following updateBoard() and writeTetrominoToBoard() funcs write a tetromino
% to the board and clears completed rows on the board.
% Returns the number of rows cleared and the updated board.
%

updateBoard(Board, {Tetromino, {PosX, PosY}}) ->
    YCoord = 1,
    ClearRowCount = 0,
    NewBoardInfo = updateBoard(Board, {Tetromino, {PosX, PosY}}, YCoord, ClearRowCount),
    RowsCleared = lists:last(NewBoardInfo),
    {RowsCleared, lists:duplicate(RowsCleared,emptyRow()) ++ lists:droplast(NewBoardInfo)}.

updateBoard([], _, _, ClearRowCount) ->
    [ClearRowCount];

updateBoard([BoardRow|BoardTail], {Tetromino, {PosX, PosY}}, YCoord, ClearRowCount) ->
    NewBoardRow = writeTetrominoToBoard(BoardRow, [PosX + TetX || [TetX, TetY] <- Tetromino, PosY + TetY == YCoord]),
    if 
        NewBoardRow == [?TETROBLOCK,?TETROBLOCK,?TETROBLOCK,?TETROBLOCK,?TETROBLOCK,
                     ?TETROBLOCK,?TETROBLOCK,?TETROBLOCK,?TETROBLOCK,?TETROBLOCK] ->
            updateBoard(BoardTail, {Tetromino, {PosX, PosY}}, YCoord + 1, ClearRowCount + 1);
        true ->
            [NewBoardRow] ++ updateBoard(BoardTail, {Tetromino, {PosX, PosY}}, YCoord + 1, ClearRowCount)
    end.

writeTetrominoToBoard(BoardRow, []) ->
    BoardRow;
writeTetrominoToBoard(BoardRow, TetrominoRowPos) ->
    PrintX = 1,
    writeTetrominoToBoard(BoardRow, TetrominoRowPos, PrintX).

writeTetrominoToBoard(BoardRow, [], _) ->
    writeTetrominoToBoard(BoardRow, []);

writeTetrominoToBoard([BoardCell | BoardTail], [TetX | TetTail], PrintX) ->
    if
        TetX == PrintX ->
            [?TETROBLOCK] ++ writeTetrominoToBoard(BoardTail, TetTail, PrintX + 1);
        true ->
            [BoardCell] ++ writeTetrominoToBoard(BoardTail, [TetX | TetTail], PrintX + 1)
    end.

%=============================================================================
%                         UPDATE GAME ATTRIBUTES
%=============================================================================
%
% Game Attributes is turple of length 3 containing the following infomation :
% {Current level of the game, 
%  Current Score, 
%  Rows cleared since the last level was achieved}
%
% This updates the Game attributes using the rows cleared this turn.
%

updateGameAttributes(GameAttributes, 0) ->
    GameAttributes;

updateGameAttributes({?STARTLEVEL, Score, RowsClearedThisLevel}, RowsClearedThisTurn) ->
    RowsClearedForNextLevel = min(?STARTLEVEL*10 - 10, max(100, ?STARTLEVEL*10 - 50)),
    RowsClearedTotal = RowsClearedThisLevel + RowsClearedThisTurn,
    UpdatedScore = Score + (?STARTLEVEL + 1) * tetrominoes:getScore(RowsClearedThisTurn),
    if
        RowsClearedForNextLevel == RowsClearedTotal ->
            {?STARTLEVEL + 1, UpdatedScore, 0};
        RowsClearedForNextLevel < RowsClearedTotal ->
            {?STARTLEVEL + 1, UpdatedScore, RowsClearedTotal - RowsClearedForNextLevel};
        true ->
            {?STARTLEVEL, UpdatedScore, RowsClearedTotal}
    end;

updateGameAttributes({Level, Score, RowsClearedThisLevel}, RowsClearedThisTurn) ->
    UpdatedScore = Score + (?STARTLEVEL + 1) * tetrominoes:getScore(RowsClearedThisTurn),
    RowsClearedTotal = RowsClearedThisLevel + RowsClearedThisTurn,
    if
        ?CLEAREDROWSFORNEXTLEVEL == RowsClearedTotal ->
            {Level + 1, UpdatedScore, 0};
        ?CLEAREDROWSFORNEXTLEVEL < RowsClearedTotal ->
            {Level, UpdatedScore, RowsClearedTotal - ?CLEAREDROWSFORNEXTLEVEL};
        true ->
            {Level, UpdatedScore, RowsClearedTotal}
    end.




%=============================================================================
%                          UPDATE GAME STATE
%=============================================================================
%
% Updates the games data structures, i.e. the board and game attributes,
% using a given move.
%
updateGameState(Board, {_, Pos}, {0,0}, GameAttributes) ->
    {tetrominoDescending, Board, Pos, GameAttributes};

updateGameState(Board, {Tetromino, {PosX, PosY}}, {MoveX, MoveY}, GameAttributes) ->
    NewPos = {PosX + MoveX, PosY + MoveY},
    case validTetrominoPos(Tetromino, Board, NewPos) of
        true ->
            {tetrominoDescending, Board, NewPos, GameAttributes};
        false ->
            if 
                MoveY == 1 ->
                    {RowsCleared, NewBoard} = updateBoard(Board, {Tetromino, {PosX, PosY}}),
                    UpdatedGameAttributes = updateGameAttributes(GameAttributes, RowsCleared),
                    {tetrominoLanded, NewBoard, {PosX, PosY}, UpdatedGameAttributes};
                true ->
                    {tetrominoDescending, Board, {PosX, PosY}, GameAttributes}
            end
    end.

%=============================================================================
%                           GET ROTATE VALUE
%=============================================================================
%
% Returns the new rotation if it is non-zero, else just returns the existing
% rotation.
%
getNewRotation(Rotation, 0) ->
    Rotation;
getNewRotation(_, NewRotation) ->
    NewRotation.

%=============================================================================
%                              GAME LOOP
%=============================================================================
%
% The main loop of the tetris game, deals with fetching moves, generating new 
% tetrominoes and updating the game state.
%
gameLoop(tetrominoDescending, Board, {Tetromino, Rotation, Pos}, GameAttributes, BoardPID, MoveQueuePID, KillPID) ->
    %fetch moves bro
    MoveQueuePID ! {getMove, self()},
    receive
        {sendMove, {MoveX, MoveY, MoveRotate}} ->
            NewRotation = getNewRotation(Rotation, MoveRotate),
            {GameState, UpdatedBoard, UpdatedTetPos, UpdatedGameAttributes} = updateGameState(Board, 
                                                                                              {tetrominoes:fetchTetromino(Tetromino, NewRotation), Pos},
                                                                                              {MoveX, MoveY},
                                                                                              GameAttributes),
            BoardPID ! {updateBoard, UpdatedBoard, {Tetromino, NewRotation, UpdatedTetPos}, UpdatedGameAttributes},
            gameLoop(GameState, UpdatedBoard, {Tetromino, NewRotation, UpdatedTetPos}, UpdatedGameAttributes, BoardPID, MoveQueuePID, KillPID)
    end;

gameLoop(tetrominoLanded, Board, _, GameAttributes, BoardPID, MoveQueuePID, KillPID) ->
    case genTetromino(Board) of
        gameOver ->
            io:format("GAME OVER :( ~n",[]),
            KillPID ! gameOver;
        {Tetromino, Rotation, Pos} ->
            BoardPID ! {updateBoard, Board, {Tetromino, Rotation, Pos}, GameAttributes},
            gameLoop(tetrominoDescending, Board, {Tetromino, Rotation, Pos}, GameAttributes, BoardPID, MoveQueuePID, KillPID)
    end.

%=============================================================================
%                               BOARD
%=============================================================================
%
% Process representing the current state of the board. Infomation can be
% queried from it or it can be updated with new values.
%
board(Board, TetrominoInfo, FutureTetrominoes, GameAttributes) ->
    receive
        {updateBoard, UpdatedBoard, UpdatedTetrominoInfo, UpdatedGameAttributes} ->
            board(UpdatedBoard, UpdatedTetrominoInfo, FutureTetrominoes, UpdatedGameAttributes);

        {getBoard, ReturnPID} ->
            ReturnPID ! {sendBoard, Board, TetrominoInfo, FutureTetrominoes, GameAttributes},
            board(Board, TetrominoInfo, FutureTetrominoes, GameAttributes);

        {getLevel, ReturnPID} ->
            ReturnPID ! {sendLevel, GameAttributes},
            board(Board, TetrominoInfo, FutureTetrominoes, GameAttributes)
    end.

%=============================================================================
%                               OUTPUT
%=============================================================================
%
% Outputs the current state of the game to the console at a fix time interval
%
output(BoardPID) ->
    timer:send_after(?DESCENDPERIOD, display),
    receive
        display ->
            BoardPID ! {getBoard, self()},
            receive
                {sendBoard, Board, {Tetromino, Rotation, Pos}, FutureTetrominoes, {Level, Score, _}} ->
                    _ = applyFuncToBoard(Board, fun printFunction/2, {tetrominoes:fetchTetromino(Tetromino, Rotation), Pos}),
                    io:fwrite("~s~n", [[$-,$-,$-,$-,$-,$-,$-,$-,$-,$-,$|]]),
                    io:format("LEVEL : ~p", [Level]),
                    io:format("        SCORE : ~p~n", [Score]),
                    output(BoardPID)
            end
    end.
