-module(ai).
-import(tetrominoes,[fetchTetromino/2, potentialXPos/2, numOfUniqueRotations/1]).
-import(tetris, [validTetrominoePos/3]).

-export([aiCoreLoop/2]).

%defines how long the ai will wait after submitting move to retrieve new game state.
-define(AIWAIT, 50).

%evaluateMoves(Board, Tetrominoe, MoveList) ->

fetchValidMoves(Board, {Tetromino, Rotation, Pos}) ->
    fetchValidMoves(Board, {Tetromino, Rotation, Pos}, tetrominoes:numOfUniqueRotations(Tetromino)).

fetchValidMoves(_, _, 0) ->
    [];
    
fetchValidMoves(Board, {Tetromino, Rotation, Pos}, PotentialRotation) ->
    Moves = genValidMoves(Board,
                            tetrominoes:fetchTetromino(Tetromino, PotentialRotation),
                            Pos,
                            tetrominoes:potentialXPos(Tetromino, PotentialRotation)),
    [{X, Y , PotentialRotation} || {X, Y} <- Moves]
        ++ fetchValidMoves(Board, {Tetromino, Rotation, Pos}, PotentialRotation - 1).



genValidMoves(Board, Tetromino, CurrPos, PotentialXPos) ->
    genValidMoves(Board, Tetromino, CurrPos, PotentialXPos, 1, []).

genValidMoves(_, _, _, [], _, _) ->
    [];

genValidMoves(Board, Tetromino, {CurrXPos, CurrYPos}, [PotentialXPos | PotentialXPosTail], PotentialYPos, CurrMoveList) ->
    case tetris:validTetrominoPos(Tetromino, Board, {PotentialXPos, PotentialYPos}) of
        true ->
            genValidMoves(Board, Tetromino, {CurrXPos, CurrYPos}, [PotentialXPos | PotentialXPosTail], PotentialYPos + 1, CurrMoveList);
        false ->
            if 
                PotentialYPos == 1 ->
                    genValidMoves(Board, Tetromino, {CurrXPos, CurrYPos}, PotentialXPosTail, 1, CurrMoveList);
                PotentialYPos < CurrYPos ->
                    genValidMoves(Board, Tetromino, {CurrXPos, CurrYPos}, PotentialXPosTail, 1, CurrMoveList);
                true ->
                    [{PotentialXPos - CurrXPos, PotentialYPos - 1 - CurrYPos}] 
                        ++ genValidMoves(Board, Tetromino, {CurrXPos, CurrYPos}, PotentialXPosTail, 1, CurrMoveList)
            end
    end.



%need to stop tetromino going higher!!
calculateBestMove(Board, TetrominoInfo) ->
    ValidMoves = fetchValidMoves(Board, TetrominoInfo),
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