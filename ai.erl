-module(ai).
-import(tetrominoes,[fetchTetromino/2]).

%defines how long the ai will wait after submitting move to retrieve new game state.
-define(AIWAIT, 50).

genValidMoves(Board, TetrominoInfo) ->
    

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