To-Do List:
* add terminal state checks to manualControl - refer to Hangman game to handle loop termination
* use prettyprinter-ansi-terminal to do my prettyprinting properly
* add IO and parser functionality to load in a plain text environment file
* create a policy that commands a random valid move whenever a drone is Unassigned
* create a policy that commands low flying moves toward the nearest unobserved patch
* [expand] add a reward signal and other requirements for a proper RL agent to learn from this environment

Completed Items:
* Write a parser for Drone that turns a digit into a Drone (7/1/2019)
* write a parser for NextActions that expects input like "[(1, Hover), (2, MoveIntercardinal NE)]" (7/1/2019)
* create manualControl function for interactive control of an environment (7/1/2019)
* finish updateState function so that everything seems to work properly under manualControl in tests (7/1/2019)
* make a function "needsCommand" to list which drones will react to input in the next updateState (7/1/2019)
* augment manualControl by listing which drones are worth commanding at the current time step based on "needsCommand" function (7/1/2019)
* make a pretty printer for WorldState and its subordinate datatypes (7/2/2019)
* make function to determine if a WorldState is terminal based on checking observations (7/2/2019)