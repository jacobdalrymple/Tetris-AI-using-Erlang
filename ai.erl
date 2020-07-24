-module(ai).
-import(tetrominoes,[fetchTetromino/2, potentialXPos/2, numOfUniqueRotations/1, applyFuncToBoard/3, writeTetrominoToBoard/2]).
-import(tetris, [validTetrominoePos/3]).

-export([aiCoreLoop/2]).

%defines how long the ai will wait after submitting move to retrieve new game state.
-define(AIWAIT, 250).
-define(TETROBLOCK, $B).

% ColInfo = {Height of cell's column, current contentes of cell}
updateRowTracker([], [], _) ->
    [];
updateRowTracker([?TETROBLOCK | BoardRowT], [{0, empty} | ColInfoT], YPos) ->
    [{YPos, block}] ++ updateRowTracker(BoardRowT, ColInfoT, YPos);
updateRowTracker([?TETROBLOCK | BoardRowT], [{Height, _} | ColInfoT], YPos) ->
    [{Height, block}] ++ updateRowTracker(BoardRowT, ColInfoT, YPos);


updateRowTracker([160 | BoardRowT], [{0, empty} | ColInfoT], YPos) ->
    [{0, empty}] ++ updateRowTracker(BoardRowT, ColInfoT, YPos);
updateRowTracker([160 | BoardRowT], [{Height, _} | ColInfoT], YPos) ->
    [{Height, empty}] ++ updateRowTracker(BoardRowT, ColInfoT, YPos).

getBoardCost([BoardRow | BoardT]) ->
    getBoardCost([BoardRow | BoardT], [{0, empty} || _ <- BoardRow], 1).


%here the heights are used in the cost function
getBoardCost([], RowTracker, _) ->
    lists:sum([1/math:pow(2, H)|| {H, _} <- RowTracker]);
%here the "holes" are counted via the list comphresion
getBoardCost([BoardRow | BoardTail], RowTracker, YPos) ->
    UpdatedRowTracker = updateRowTracker(BoardRow, RowTracker, YPos),
    length([H || {H, State} <- UpdatedRowTracker, State =:= empty, H =/= 0])
        + getBoardCost(BoardTail, UpdatedRowTracker, YPos + 1).

evalMove(Board, Tetromino, Rotation, {PosX, PosY}, {MoveX, MoveY}) ->
    NewPos = {PosX + MoveX, PosY + MoveY},
    getBoardCost(tetris:applyFuncToBoard(Board, 
                                          fun tetris:writeTetrominoToBoard/2,
                                          {tetrominoes:fetchTetromino(Tetromino, Rotation), 
                                              NewPos})).


fetchValidMoves(Board, {Tetromino, Rotation, Pos}) ->
    fetchValidMoves(Board, {Tetromino, Rotation, Pos}, tetrominoes:numOfUniqueRotations(Tetromino)).

fetchValidMoves(_, _, 0) ->
    [];

%get rid of rotation variable
fetchValidMoves(Board, {Tetromino, Rotation, Pos}, PotentialRotation) ->
    Moves = genValidMoves(Board,
                            tetrominoes:fetchTetromino(Tetromino, PotentialRotation),
                            Pos,
                            tetrominoes:potentialXPos(Tetromino, PotentialRotation)),
    [{evalMove(Board, Tetromino, PotentialRotation, Pos, {X,Y}), {X, Y , PotentialRotation}} || {X, Y} <- Moves]
        ++ fetchValidMoves(Board, {Tetromino, Rotation, Pos}, PotentialRotation - 1).



genValidMoves(Board, Tetromino, CurrPos, PotentialXPos) ->
    %io:format("POTENTIAL X POS : ~p~n", [PotentialXPos]),
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

selectBestMove([{Cost, Move} | MoveListT]) ->
    selectBestMove([{Cost, Move} | MoveListT], [{10000000, Move}]).

selectBestMove([], BestMoveList) ->
    {_, Move} = lists:nth( rand:uniform(length(BestMoveList)), BestMoveList),
    Move;
selectBestMove([{Cost, Move} | MoveListT], [{BestCost, BestMove} | BestMoveListT]) ->
    if
        Cost < BestCost ->
            selectBestMove(MoveListT, [{Cost, Move}]);
        true ->
            selectBestMove(MoveListT, [{BestCost, BestMove} | BestMoveListT])
    end.


%need to stop tetromino going higher!!
calculateBestMove(Board, TetrominoInfo) ->
    MoveList = fetchValidMoves(Board, TetrominoInfo),
    %io:format("MOVE LIST : ~p~n", [MoveList]),
    %io:format("SENT MOVE : ~p~n", [selectBestMove(MoveList)]),
    selectBestMove(MoveList).
    

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