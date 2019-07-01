To-Do List:
* make a function "needsCommand" to list which drones will react to input in the next updateState
* augment manualControl by listing which drones are worth commanding at the current time step based on "needsCommand function"
* make function to determine if a WorldState is terminal based on checking observations
* add terminal state checks to manualControl
* delete terminalNextActions function (should be renedered obsolete by parsers)
* finish updateState function so that everything seems to work properly under manualControl in tests

Completed Items:
* Write a parser for Drone that turns a digit into a Drone (7/1/2019)
* write a parser for NextActions that expects input like "[(1, Hover), (2, MoveIntercardinal NE)]" (7/1/2019)
* create manualControl function for interactive control of an environment (7/1/2019)