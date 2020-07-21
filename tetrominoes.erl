-module(tetrominoes).
-export([fetchTetromino/2, fetchTetrominoXPos/2]).

-define(WIDTH, 10).
-define(HEIGHT, 15).

fetchTetromino(i,1) ->
    [[0,1],[1,1],[2,1],[3,1]];
fetchTetromino(i,2) ->
    [[3,0],[3,1],[3,2],[3,3]];
fetchTetromino(i,3) ->
    [[0,3],[1,3],[2,3],[3,3]];
fetchTetromino(i,4) ->
    [[1,0],[1,1],[1,2],[1,3]].

fetchTetrominoXPos(i,1) ->
    [1, 2, 3, 4, 5, 6, 7];
fetchTetrominoXPos(i,2) ->
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
fetchTetrominoXPos(i,3) ->
    fetchTetrominoXPos(i,1);
fetchTetrominoXPos(i,4) ->
    fetchTetrominoXPos(i,2).

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