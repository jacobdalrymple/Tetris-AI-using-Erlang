-module(ai).
-import(tetrominoes,[fetchTetromino/2, fetchTetrominoPos/2]).

%defines how long the ai will wait after submitting move to retrieve new game state.
-define(AIWAIT, 50).

evaluateMoves(Board, Tetrominoe, MoveList) ->


genValidMoves(Board, {Tetromino, Rotation, Pos}) ->
    MoveList = tetrominoes:fetchTetrominoPos(Tetromino, Rotation),
    MoveScores = evaluateMoves(Board, tetrominoes:fetchTetromino(Tetromino, Rotation), MoveList),

calculateBestMove(Board, TetrominoInfo) ->
    ValidMoves = genValidMoves(Board, TetrominoInfo),
    

aiCoreLoop(BoardPID, MoveQueuePID) ->
    BoardPID ! {getBoard, self()},
    receive
        {sendBoard, Board, TetrominoInfo} ->
            Move = calculateBestMove(Board, TetrominoInfo),
            MoveQueuePID ! {move, Move},
            timer:send_after(?AIWAIT, tick),
            receive
                tick ->
                    aiCoreLoop(BoardPID, MoveQueuePID)
            end
    end.