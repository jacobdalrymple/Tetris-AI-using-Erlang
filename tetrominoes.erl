-module(tetrominoes).
-export([fetchTetromino/2, potentialXPos/2, numOfUniqueRotations/1]).

-define(WIDTH, 10).
-define(HEIGHT, 15).

fetchTetromino(i, 1) ->
    [[0,1],[1,1],[2,1],[3,1]];
fetchTetromino(i, 2) ->
    [[3,0],[3,1],[3,2],[3,3]];

fetchTetromino(j, 1) ->
    [[0,0],[0,1],[1,1],[2,1]];
fetchTetromino(j, 2) ->
    [[1,0],[2,0],[1,1],[1,2]];
fetchTetromino(j, 3) ->
    [[0,1],[1,1],[2,1],[2,2]];
fetchTetromino(j, 4) ->
    [[1,0],[1,1],[0,2],[1,2]];

fetchTetromino(l, 1) ->
    [[2,0],[0,1],[1,1],[2,1]];
fetchTetromino(l, 2) ->
    [[1,0],[1,1],[1,2],[2,2]];
fetchTetromino(l, 3) ->
    [[0,1],[1,1],[2,1],[0,2]];
fetchTetromino(l, 4) ->
    [[0,0],[1,0],[1,1],[1,2]];

fetchTetromino(o,_) ->
    [[1,0],[2,0],[1,1],[2,1]];

fetchTetromino(s, 1) ->
    [[1,0],[2,0],[0,1],[1,1]];
fetchTetromino(s, 2) ->
    [[1,0],[1,1],[2,1],[2,2]];
fetchTetromino(s, 3) ->
    [[1,1],[2,1],[0,2],[1,2]];
fetchTetromino(s, 4) ->
    [[0,0],[0,1],[1,1],[1,2]];

fetchTetromino(t, 1) ->
    [[1,0],[0,1],[1,1],[2,1]];
fetchTetromino(t, 2) ->
    [[1,0],[1,1],[2,1],[1,2]];
fetchTetromino(t, 3) ->
    [[0,1],[1,1],[2,1],[1,2]];
fetchTetromino(t, 4) ->
    [[1,0],[0,1],[1,1],[1,2]];

fetchTetromino(z, 1) ->
    [[0,0],[1,0],[1,1],[2,1]];
fetchTetromino(z, 2) ->
    [[2,0],[1,1],[2,1],[1,2]];
fetchTetromino(z, 3) ->
    [[0,1],[1,1],[1,2],[2,2]];
fetchTetromino(z, 4) ->
    [[1,0],[0,1],[1,1],[0,2]].



potentialXPos(i, 1) ->
    [1, 2, 3, 4, 5, 6, 7];
potentialXPos(i, 2) ->
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

potentialXPos(o,_) ->
    [1, 2, 3, 4, 5, 6, 7, 8, 9];

potentialXPos(_, 1) ->
    [1, 2, 3, 4, 5, 6, 7, 8];
potentialXPos(_, 2) ->
    [1, 2, 3, 4, 5, 6, 7, 8, 9];
potentialXPos(_, 3) ->
    potentialXPos(j, 1);
potentialXPos(_, 4) ->
    potentialXPos(j, 2).

numOfUniqueRotations(i) ->
    2;
numOfUniqueRotations(o) ->
    1;
numOfUniqueRotations(_) ->
    4.

%fetchTetrominoPos(i,1) ->
%    [{1, ?HEIGHT-1}, {2, ?HEIGHT-1}, {3, ?HEIGHT-1}, {4, ?HEIGHT-1}, 
%     {5, ?HEIGHT-1}, {6, ?HEIGHT-1}, {7, ?HEIGHT-1}];
%fetchTetrominoPos(i,2) ->
%    [{1, ?HEIGHT-1}, {2, ?HEIGHT-1}, {3, ?HEIGHT-1}, {4, ?HEIGHT-1}, {5, ?HEIGHT-1},
%     {6, ?HEIGHT-1}, {7, ?HEIGHT-1}, {8, ?HEIGHT-1}, {9, ?HEIGHT-1}, {10, ?HEIGHT-1}];
%fetchTetrominoPos(i,3) ->
%    fetchTetrominoPos(i,1);
%fetchTetrominoPos(i,4) ->
%    fetchTetrominoPos(i,2).
%
%
%fetchTetromino(l,1) ->
%    [[0,1],[1,0],[1,0],[1,0],[1,0]];
%fetchTetromino(l,2) ->
%    [[3,0],[0,1],[0,1],[0,1],[0,1]];
%fetchTetromino(l,3) ->
%    [[0,3],[1,0],[1,0],[1,0],[1,0]];
%fetchTetromino(l,4) ->
%    [[1,0],[0,1],[0,1],[0,1],[0,1]].