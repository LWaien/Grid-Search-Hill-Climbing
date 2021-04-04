# Grid-Search-Hill-Climbing
Implementation of hill climb heuristic search

This is code from a programming assignment from Ai class. The files grid-class, grid-draw and grid-make were provided. These files are responsible for the creating the visual environment in which we are meant to search for the goal (the green dot). The key points of this project are the monitor function (in grid-search.ss) and the entirety of grid-priority-queue.ss. As the name suggests, grid-priority-queue.ss implements a priority queue as well as some helpful functionalities for other parts of the program to interact with such as "front", "pop" and "value". Most importantly, "value" returns the heuristic value of any given point which informs our search and allows to us to prioritize our queue. The heurisitc value is simply the euclidean distance between a block and goal. Since we are trying to minimize this, our priority queue ranks options based on this value in asscending order.
