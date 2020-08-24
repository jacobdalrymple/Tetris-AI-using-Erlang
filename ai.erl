-module(ai).
-import(tetrominoes,[fetchTetromino/2, potentialXPos/2, numOfUniqueRotations/1, applyFuncToBoard/3, writeTetrominoToBoard/2]).
-import(tetris, [validTetrominoePos/3]).

-export([aiCoreLoop/2]).

%defines how long the ai will wait after submitting move to retrieve new game state.
-define(AIWAIT, 10).
-define(TETROBLOCK, $B).
-define(BUMPSCOREWEIGHT, 1).
-define(HEIGHTSCOREWEIGHT, 1).
-define(HOLESCOREWEIGHT, 10).
-define(CLEARROWSCOREWEIGHT, -5).

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

getBumpAndHeightCost([{H, _}]) ->
    ?HEIGHTSCOREWEIGHT * 1/math:pow(2, H);
getBumpAndHeightCost([{H1, _} | [{H2, A} | T]]) ->
   ?HEIGHTSCOREWEIGHT * (1/math:pow(2, H1)) + ?BUMPSCOREWEIGHT * abs(H1 - H2) + getBumpAndHeightCost([{H2, A} | T]).


%here the heights are used in the cost function
getBoardCost([], RowTracker, _) ->
    getBumpAndHeightCost(RowTracker);
    %lists:sum([1/math:pow(2, H)|| {H, _} <- RowTracker])
    %    + ;
%here the "holes" are counted via the list comphresion
getBoardCost([BoardRow | BoardTail], RowTracker, YPos) ->
    UpdatedRowTracker = updateRowTracker(BoardRow, RowTracker, YPos),
    %num of holes + cleared lines + arrgate height
    ?HOLESCOREWEIGHT * length([H || {H, State} <- UpdatedRowTracker, State =:= empty, H =/= 0])
        + ?CLEARROWSCOREWEIGHT * trunc( length([1 || {_, State} <- UpdatedRowTracker, State == block]) / 10)
        + getBoardCost(BoardTail, UpdatedRowTracker, YPos + 1).

evalMove(Board, Tetromino, Rotation, {PosX, PosY}, {MoveX, MoveY}) ->
    NewPos = {PosX + MoveX, PosY + MoveY},
    {_, UpatedBoard} = tetris:updateBoard(Board, {tetrominoes:fetchTetromino(Tetromino, Rotation), NewPos}),
    getBoardCost(UpatedBoard).


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
                PotentialYPos =< 1 ->
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
        {sendBoard, Board, placeholder, _} ->
            timer:send_after(?AIWAIT, tick),
            receive
                tick ->
                    aiCoreLoop(BoardPID, MoveQueuePID)
            end;
        {sendBoard, Board, TetrominoInfo, GameAttributes} ->
            Move = calculateBestMove(Board, TetrominoInfo),
            MoveQueuePID ! {move, Move},
            timer:send_after(?AIWAIT, tick),
            receive
                tick ->
                    aiCoreLoop(BoardPID, MoveQueuePID)
            end
    end.