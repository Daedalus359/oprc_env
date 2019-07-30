To-Do List:
* swap all my imports so that I am using the strict version of Map
* fix updates so that observations and drones becoming commandable happen at the same time
* create a "Scenario" type that has a worldstate, a policy, and some time tracking information
* add functionality to advance a scenario one time step, or run it to completion (with a backup max number of time steps to execute)
* create a policy that commands a random valid move whenever a drone is Unassigned
* get low sweep snake pattern policy working for large rectangular environments
* get high sweep first policy working for large rectangular environments
* generalize low sweep policy to arbitrary environment shapes
* generalize high sweep policy to arbitrary environment shapes
* create a random environment generator that splits into a few different categorical types, then variations of a few continuous parameters within categories - make this modular by composing the sub-generators, so that I can train models to specialize on sub generators
* create functionality to test policies on a random assortment of environments
* create a policy that commands low flying moves toward the nearest unobserved patch
* create a policy that maintains a PGM data structure of the environment, and **uses RL to tune the parameters of the PGM from session to session** - make the PGM be a kind of spatial policy that works much like a convolutional neural network, except that it spits out a belief map ("image") of the same size that it took in, where each patch in this belief map represents the probability that the patch requires high scrutiny
* [expand] add a reward signal and other requirements for a proper RL agent to learn from this environment
* use prettyprinter-ansi-terminal to do my prettyprinting properly

Completed Items:
* Write a parser for Drone that turns a digit into a Drone (7/1/2019)
* write a parser for NextActions that expects input like "[(1, Hover), (2, MoveIntercardinal NE)]" (7/1/2019)
* create manualControl function for interactive control of an environment (7/1/2019)
* finish updateState function so that everything seems to work properly under manualControl in tests (7/1/2019)
* make a function "needsCommand" to list which drones will react to input in the next updateState (7/1/2019)
* augment manualControl by listing which drones are worth commanding at the current time step based on "needsCommand" function (7/1/2019)
* make a pretty printer for WorldState and its subordinate datatypes (7/2/2019)
* make function to determine if a WorldState is terminal based on checking observations (7/2/2019)
* add terminal state checks to manualControl (7/2/2019)
* parse a text file into a list of lines with their line numbers in the form \[(2, "HLL  HL")\] (7/29/2019)
* add IO and parser functionality to load in a plain text environment file (7/29/2019)
* make a function that initializes an EnvironmentInfo from an Environment, marking everything unseen (7/29/2019)
* make a function that can initialize a worldState from an Environment (7/29/2019)
* demo exploring a small environment interactively (7/29/2019)
