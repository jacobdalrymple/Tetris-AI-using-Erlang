# Tetris AI Programmed with Erlang

This project is an implementation of a tetris AI with the functional programming
language Erlang, using concurrency design principles.

---------------------------------
Overview of Concurrent Processes:
---------------------------------

The project has 6 independent processes running concurrently:

* MoveQueue
    * This process tracks the accumulated moves before a move has been executed.

* GameState
    * This process contains all the information for the current state of the game,
      being able to accept requests of information, or updating various attributes.
                    
* DescendMoveGenerator
    * This process sends the downward move to the queue at a time interval dependent
      on the current state of the game.
                        
* GameLoop
    * This process updates the GameState at every iteration; i.e. executing moves from
      the MoveQueue and checking for various conditions (such as the game over state).

* Output
    * Requests information from GameState and displays the tetris game in the terminal.

* AI
    * This process sends moves to MoveQueue to play the game of tetris.



---------------
Overview of AI:
---------------

The AI generates all possible moves for the current tetromino, and evaluates each of them using
a weighted sum of the metrics : 

* The sum of the height of each column.
* The number of "holes" created from the accumulated tetromio blocks.
* The number of complete rows resulting from the move.
* The "bumpiness" of the accumulated blocks (implemented as the sum of the absolute differences in height between adjacent rows).

The weights of these metrics have not been optimally configured, I have simply tuned them by eye.