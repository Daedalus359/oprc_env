To-Do List:
* create functionality to animate the complete replay of a scenario with the worldState and worldView at each time step forming the frames (START BY FIXING SCENARIOREPLAY STEPPER)
* implement K means clustering on my environment by performing a fold over the environment map that assigns each patch to the mean it is closest to (according to a distance metric that is something like 14 * min(delta_x, delta_y) + 10 * abs (delta_x - delta_y)
* implement an "adaptive K-means" policy that repeatedly re-assigns territory based on performing a limited number of K-means iterations with current drone positions as an initialization
* modify my droneStatus drawing function to represent the field of view of the drone at that time
* make a drawing function that applies a translucent coloring over a particular set of patch positions, meant to represent the territory assigned to a particular drone
* augment the scenario animation to keep an up to date representation of drone territories on screen
* get rid of PSQueue in my project in favor of a data structure from stackage snapshot 14.3
* replace my environment data structure with something like Data.Quadtree
* get high sweep first policy working for large environments
* modify A* to accept a custom cost function rather than making all move costs equal to 1. Give this function access to enough parameters to support the two behavior modifications I plan to implement
* make an A* cost function that is suitable for use with HighSweep policies by raising costs for moving off of the points where (mod x 3 = 0), make the high sweep policy instance use it
* benchmark and improve my code's performance so that I can run 500 random trials of env 7 really fast
rewrite all list traversing operations in terms of foldr and see if I get performance improvement
* find out if PSQueue uses last in first out for tie breaking, find another implementation if not
* make a version of A* that penalizes paths involving visited nodes by making those moves cost 1.5x as much as usual
* adapt the online density estimation algorithms from Wenhao Luo to use observation filtering with a Kernel to estimate quality of fit of various environment models, then do planning based on that info
* create a random environment generator that splits into a few different categorical types, then variations of a few continuous parameters within categories - make this modular by composing the sub-generators, so that I can train models to specialize on sub generators
* create functionality to test policies on a random assortment of environments
* create a policy that maintains a PGM data structure of the environment, and **uses RL to tune the parameters of the PGM from session to session** - make the PGM be a kind of spatial policy that works much like a convolutional neural network, except that it spits out a belief map ("image") of the same size that it took in, where each patch in this belief map represents the probability that the patch requires high scrutiny
* [expand] add a reward signal and other requirements for a proper RL agent to learn from this environment
* consider adding functionality such as needing to track battery level and periodically return for a swap if the environment isn't rich enough
* add some kind of 2d visual representation of the environment / belief / activity data structures
* create an alternate Doc ann (as in prettyprinter) generating function for Scenario history that shows where a drone was at the moment it get each command
* find a good configuration format and parses for my use case, make one that holds all info for a scenario
* implement a random environment parser that accepts a third kind of character, ?, and assigns it high or low with some probability

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
* swap all my imports so that I am using the strict version of Map (7/30/2019)
* fix updates so that observations and drones becoming commandable happen at the same time (7/30/2019)
* create a "Scenario" type that has a worldstate, a policy, and some time tracking information (7/30/2019)
* add a history to the Scenario type to contain an efficient representation of everything that has happened so far (7/30/2019)
* modify stepEnsemble to immediately discard commands that would put a drone out of bounds (8/2/2019)
* make a new function in SampleVals for debugging that runs Scenarios with printing one time step at a time (8/5/2019)
* figure out why every action is taking one time step too many during a scenario run (8/5/2019)
* modify stepEnsemble to discard pointless moveVertical commands (8/5/2019)
* figure out and fix why hover takes an extra time step (possibly because step function initializes stepsremaining to zero for it) (8/6/2019)
* complete applyValidActions and the Policy instance for RandomFilteredPolicy (8/6/2019)
* add a test for Hover to randomValidAction (8/6/2019)
* create a variant of RandomAgent with a move filter, make one that won't command out of bounds / pointless moves (incl hover) (8/6/2019)
* write A* in GraphOPRC (8/12/2019)
* get low sweep snake pattern policy working for large environments (8/12/2019)
* add a function in Scenario that outputs the time for a Scenario to run completely (as a Maybe) (8/12/2019)
* get the random scenario time sampler working in RandomOPRC (8/13/2019)
* make a function that randomly generates a footprint based on x and y bounds (8/26/2019)
* make a function to fill in each patch of the environment with a bernoulli chance of Far / Close (8/26/2019)
* make a function that will generate a .env file from an environment (8/26/2019)
* make a function that adds holes to a footprint (8/27/2019)
* create a function that turns an environment into a (Data.Set) of locations to visit from a high altitude (8/27/2019)
* make a function to draw a representation of a WorldView to screen (8/29/2019)
* make a function to draw the current time to screen (8/29/2019)
* make a function to draw an environment to screen (8/29/2019)
* make a function that represents a worldState on screen (8/29/2019)
* make a "Replay" datatype in Scenario that contains all of the info in a scenario at any given time step, and the history from the end of that
Scenario, and make a function that steps this foreward one time step (9/3/2019)