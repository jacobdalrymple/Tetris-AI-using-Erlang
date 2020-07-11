-module(tetrominoes).
-export([fetchTetromino/2]).

fetchTetromino(i,1) ->
    [[0,1],[1,1],[2,1],[3,1],[4,1]];
fetchTetromino(i,2) ->
    [[3,0],[3,1],[3,2],[3,3],[3,4]];
fetchTetromino(i,3) ->
    [[0,3],[1,3],[2,3],[3,3],[4,3]];
fetchTetromino(i,4) ->
    [[1,0],[1,1],[1,2],[1,3],[1,4]].


%fetchTetromino(l,1) ->
%    [[0,1],[1,0],[1,0],[1,0],[1,0]];
%fetchTetromino(l,2) ->
%    [[3,0],[0,1],[0,1],[0,1],[0,1]];
%fetchTetromino(l,3) ->
%    [[0,3],[1,0],[1,0],[1,0],[1,0]];
%fetchTetromino(l,4) ->
%    [[1,0],[0,1],[0,1],[0,1],[0,1]].