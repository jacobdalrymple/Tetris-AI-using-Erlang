-module(ai).
-import(tetrominoes,[fetchTetromino/2, fetchTetrominoPos/2]).
-import(tetris, [validTetrominoePos/3]).

-export([aiCoreLoop/2]).

%defines how long the ai will wait after submitting move to retrieve new game state.
-define(AIWAIT, 50).

%evaluateMoves(Board, Tetrominoe, MoveList) ->
    

genValidMoves(Board, Tetromino, CurrPos, PotentialXPos) ->
    genValidMoves(Board, Tetromino, CurrPos, PotentialXPos, 1, []).

genValidMoves(_, _, _, [], _, _) ->
    [];

genValidMoves(Board, Tetromino, {CurrPosX, CurrPosY}, [PotentialXPos | PotentialXPosTail], PotentialYPos, CurrMoveList) ->
    case tetris:validTetrominoPos(Tetromino, Board, {PotentialXPos, PotentialYPos}) of
        true ->
            genValidMoves(Board, Tetromino, {CurrPosX, CurrPosY}, [PotentialXPos | PotentialXPosTail], PotentialYPos + 1, CurrMoveList);
        false ->
            if 
                PotentialYPos == 1 ->
                    genValidMoves(Board, Tetromino, {CurrPosX, CurrPosY}, PotentialXPosTail, 1, CurrMoveList);
                true ->
                    [{PotentialXPos - CurrPosX, PotentialYPos - 1 - CurrPosY, 0}] 
                        ++ genValidMoves(Board, Tetromino, {CurrPosX, CurrPosY}, PotentialXPosTail, 1, CurrMoveList)
            end
    end.

%need to stop tetromino going higher!!
calculateBestMove(Board, {Tetromino, Rotation, Pos}) ->
    ValidMoves = genValidMoves(Board,
                               tetrominoes:fetchTetromino(Tetromino, Rotation),
                               Pos,
                               tetrominoes:fetchTetrominoXPos(Tetromino, Rotation)),
    RandMoveIndex = rand:uniform(length(ValidMoves)),
    lists:nth(RandMoveIndex, ValidMoves).
    

aiCoreLoop(BoardPID, MoveQueuePID) ->
    BoardPID ! {getBoard, self()},
    receive
        {sendBoard, Board, placeholder} ->
            timer:send_after(?AIWAIT, tick),
            receive
                tick ->
                    aiCoreLoop(BoardPID, MoveQueuePID)
            end;
        {sendBoard, Board, TetrominoInfo} ->
            Move = calculateBestMove(Board, TetrominoInfo),
            MoveQueuePID ! {move, Move},
            timer:send_after(?AIWAIT, tick),
            receive
                tick ->
                    aiCoreLoop(BoardPID, MoveQueuePID)
            end
    end.