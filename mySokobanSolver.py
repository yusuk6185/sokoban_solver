"""

    Sokoban assignment


The functions and classes defined in this module will be called by a marker script.
You should complete the functions and classes according to their specified interfaces.

No partial marks will be awarded for functions that do not meet the specifications
of the interfaces.

You are NOT allowed to change the defined interfaces.
In other words, you must fully adhere to the specifications of the
functions, their arguments and returned values.
Changing the interfacce of a function will likely result in a fail
for the test of your code. This is not negotiable!

You have to make sure that your code works with the files provided
(search.py and sokoban.py) as your code will be tested
with the original copies of these files.

Last modified by 2020-08-09  by f.maire@qut.edu.au
- clarifiy some comments, rename some functions
  (and hopefully didn't introduce any bug!)

"""

# You have to make sure that your code works with
# the files provided (search.py and sokoban.py) as your code will be tested
# with these files
import search
import sokoban

# - - - - - Global Variables - - - - - - - - - - - - - - - - - - - - - - - - -

MOVE_DICT = {"Up": (0, -1), "Down": (0, 1), "Left": (-1, 0), "Right": (1, 0)}


# - - - - - Auxiliary Functions - - - - - - - - - - - - - - - - - - - - - - - - - -

def execute_move(warehouse, action):
    """
        Auxiliary Function to move.
    
        @ Param (warehouse, action):
            warehouse instance
            action to be executed (e.g. "Up". "Down", "Left", "Right")
    
        @ Return:
            new warehouse instance with new location of the worker and boxes
    """
    worker = warehouse.worker
    boxes = list(warehouse.boxes)

    # new location of worker
    new_worker_loc = next_location(worker, MOVE_DICT[action])

    # new location of a box if it is moved
    if new_worker_loc in boxes:
        boxes.remove(new_worker_loc)  # delete old location of a box
        boxes.append(next_location(new_worker_loc, MOVE_DICT[action]))  # add new location of a box

    # make a new warehouse instance
    new_warehouse = warehouse.copy(new_worker_loc, boxes)

    return new_warehouse


def is_move_possible(warehouse, action):
    """
        Auxiliary Function to check if this move action is possible.

        @Param (warehouse, action):
            warehouse instance
            action to be executed (e.g. "Up". "Down", "Left", "Right")
    
        @Return:
            True or False
    """
    worker = warehouse.worker
    boxes = warehouse.boxes
    walls = warehouse.walls

    new_worker_loc = next_location(worker, MOVE_DICT[action])

    # the next cell of new worker location with same direction
    next_new_worker_loc = next_location(new_worker_loc, MOVE_DICT[action])

    # if next location of worker is a wall
    if new_worker_loc in walls:
        return False  # cannot move

    else:

        # if worker wants to move a box
        if new_worker_loc in boxes:

            # if a box cannot be moved to the next location
            if next_new_worker_loc in walls or next_new_worker_loc in boxes:
                return False  # cannot move

    return True  # can move


def next_location(t0, t1):
    """
        Auxiliary Function to identify the location after action
    """
    return t0[0] + t1[0], t0[1] + t1[1]


def manhattan_dist(t0, t1):
    """
        Auxiliary Function to calculate the manhattan distance between two locations
    """
    return abs(t0[0] - t1[0]) + abs(t0[1] - t1[1])


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


def my_team():
    """
        Return the list of the team members of this assignment submission as a list
        of triplet of the form (student_number, first_name, last_name)
    """
    return [(10240501, 'Patrick', 'Choi'), (10421106, 'Ian', 'Choi')]


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


def taboo_cells(warehouse):
    """
        Identify the taboo cells of a warehouse. A "taboo cell" is by definition
        a cell inside a warehouse such that whenever a box get pushed on such
        a cell then the puzzle becomes unsolvable.
    
        Cells outside the warehouse are not taboo. It is a fail to tag one as taboo.
    
        When determining the taboo cells, you must ignore all the existing boxes,
        only consider the walls and the target  cells.
        Use only the following rules to determine the taboo cells;
         Rule 1: if a cell is a corner and not a target, then it is a taboo cell.
         Rule 2: all the cells between two corners along a wall are taboo if none of
                 these cells is a target.
    
        @param warehouse:
            a Warehouse object with a worker inside the warehouse
    
        @return
           A string representing the warehouse with only the wall cells marked with
           a '#' and the taboo cells marked with a 'X'.
           The returned string should NOT have marks for the worker, the targets,
           and the boxes.       
    """
    # marks of object in warehouse
    removable_mark = ['@', '$']
    wall_mark = '#'
    taboo_mark = 'X'
    target_mark = ['.', '!', '*']

    def check_corner(wh_vis, x, y, check_wall=0):
        """
            The function to check if the cell is located in the corner
            by checking if one of adjacent up or down cell is wall
            and one of adjacent right or left cell is wall.
    
            @ param
                wh_vis: list that contains strings of warehouse
                x: the location of the cell on x-axis
                y: the location of the cell on y-axis
                check_wall: parameter to check if the cell is located next to the wall
    
            @return
                True:
                    1.return true when check_wall = 1
                    and at least one side of the
                    four sides of selected cell (up, down, right, left)
                    is close together with the wall.
                    2. return true when check_wall = 0
                    and at least one side of up, down
                    side and one side of right, left side of the selected cell
                    is close together with the wall at the same time.
    
                False:
                    Otherwise.
        """
        adj_ud_walls = 0
        adj_lr_walls = 0

        # Check if adj up and down walls of (x,y) is wall
        for (coor_x, coor_y) in [(0, 1), (0, -1)]:
            if wh_vis[y + coor_y][x + coor_x] == wall_mark:
                adj_ud_walls += 1
        # Check if adj left and right walls of (x,y) is wall
        for (coor_x, coor_y) in [(1, 0), (-1, 0)]:
            if wh_vis[y + coor_y][x + coor_x] == wall_mark:
                adj_lr_walls += 1

        # for rule 2
        if check_wall:
            return (adj_ud_walls >= 1) or (adj_lr_walls >= 1)

        # for rule 1
        else:
            return (adj_ud_walls >= 1) and (adj_lr_walls >= 1)

    # warehouse copy
    wh_copy = warehouse.copy()
    wh_copy.boxes = []

    avail_loc = []

    for row in range(wh_copy.nrows):
        for col in range(wh_copy.ncols):
            dst = (row, col)
            if can_go_there(wh_copy, dst):
                avail_loc.append((col, row))

    # Make the warehouse to string
    wh_copy_str = str(wh_copy)

    # Worker and boxes are removed as unneeded
    for mark in removable_mark:
        wh_copy_str = wh_copy_str.replace(mark, ' ')

    # Make the warehouse string x * y view
    wh_copy_vis = [list(i) for i in wh_copy_str.split('\n')]

    # Rule 1
    # For each row
    for y in range(0, wh_copy.nrows - 1):

        # Mark if the cell is inside
        inside = False
        # For each column except first and last column ????????
        for x in range(0, wh_copy.ncols - 1):
            if not inside:
                # It is considered inside when the cell encounters the first wall
                if wh_copy_vis[y][x] == wall_mark:
                    inside = True
            else:
                # Break if the rest of the cell in the row is empty
                if all([cell == ' ' for cell in wh_copy_vis[y][x:]]):
                    break
                # It is not taboo cell if the cell is target
                if wh_copy_vis[y][x] not in target_mark:
                    # Check if the cell is not a wall
                    if wh_copy_vis[y][x] != wall_mark:
                        # Check if the location is a corner
                        if check_corner(wh_copy_vis, x, y):
                            if (x, y) in avail_loc:
                                wh_copy_vis[y][x] = taboo_mark

    # Rule 2
    # For each row except first and last row
    for y in range(1, len(wh_copy_vis) - 1):
        # For each cell except first and last cell in the row
        for x in range(1, len(wh_copy_vis[0]) - 1):
            # If the cell is at the corner and taboo cell at the same time
            # set row and column
            if check_corner(wh_copy_vis, x, y) and wh_copy_vis[y][x] == taboo_mark:
                row = wh_copy_vis[y][x + 1:]
                col = [row[x] for row in wh_copy_vis[y + 1:][:]]
                # Set the cells in the same row to taboo cell
                # located next to taboo cell if not target cell or wall
                for x2 in range(len(row)):
                    if row[x2] in target_mark or row[x2] == wall_mark:
                        break
                    if row[x2] == taboo_mark and check_corner(wh_copy_vis, x2 + x + 1, y):
                        if all([check_corner(wh_copy_vis, x3, y, 1) for x3 in range(x + 1, x2 + x + 1)]):
                            for x4 in range(x + 1, x2 + x + 1):
                                if (x4, y) in avail_loc:
                                    wh_copy_vis[y][x4] = taboo_mark

                # Set the cells in the same column to taboo cell
                # located next to taboo cell if not target cell or wall
                for y2 in range(len(col)):
                    if col[y2] in target_mark or col[y2] == wall_mark:
                        break
                    if col[y2] == taboo_mark and check_corner(wh_copy_vis, x, y2 + y + 1):
                        if all([check_corner(wh_copy_vis, x, y3, 1) for y3 in range(y + 1, y2 + y + 1)]):
                            for y4 in range(y + 1, y2 + y + 1):
                                if (x, y4) in avail_loc:
                                    wh_copy_vis[y4][x] = taboo_mark

    # Return to one string from row * col view
    wh_copy_str = '\n'.join([''.join(line) for line in wh_copy_vis])

    # Remove target marks from the string as unneeded
    for mark in target_mark:
        wh_copy_str = wh_copy_str.replace(mark, ' ')

    return wh_copy_str


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


class SokobanPuzzle(search.Problem):
    """
        An instance of the class 'SokobanPuzzle' represents a Sokoban puzzle.
        An instance contains information about the walls, the targets, the boxes
        and the worker.
    
        Your implementation should be fully compatible with the search functions of
        the provided module 'search.py'.
    """

    def __init__(self, warehouse):
        """
            Constructor specifies the condition of warehouse 
            (such as worker, boxes, walls, targets and taboo cells), the initial state and goals.
        """
        self.worker = warehouse.worker
        self.boxes = warehouse.boxes
        self.walls = warehouse.walls
        self.targets = warehouse.targets

        # specify the taboo cells
        self.taboo = []
        taboo_strs = taboo_cells(warehouse)
        str_ = taboo_strs.split('\n')
        for y in range(len(str_)):
            for x in range(len(str_[y])):
                if str_[y][x] == "X":
                    self.taboo.append((x, y))

        # initial state  e.g. ( worker location (1,3), boxes location ((3,3), (5,1)) )
        self.initial = (warehouse.worker, tuple(warehouse.boxes))
        self.state = self.initial

        # goal
        self.goal = self.targets

    def actions(self, state):
        """
            Return the list of actions that can be executed in the given state.

            As specified in the header comment of this class, the attributes
            'self.allow_taboo_push' and 'self.macro' should be tested to determine
            what type of list of actions is to be returned.

            @Param (state):
               tuple for location of worker and boxes

            @Return:
                list for actions that can be executed in this state
        """
        actions = []

        # current state
        worker = state[0]
        boxes = state[1]

        for action, move in MOVE_DICT.items():
            new_worker_loc = next_location(worker, move)
            next_new_worker_loc = next_location(new_worker_loc, move)

            # if worker can move
            if new_worker_loc not in self.walls:

                # if worker is trying to move a box (= worker wants to go to the location of box)
                if new_worker_loc in boxes:

                    # if a box can be moved to the next location
                    if next_new_worker_loc not in self.walls \
                            and next_new_worker_loc not in self.taboo \
                            and next_new_worker_loc not in boxes:
                        actions.append(action)

                # just go without moving box
                else:
                    actions.append(action)

        return actions  # e.g. ["Up", "Left", "Right"]

    def result(self, state, action):
        """
            Return the state that results from executing the given state.
            The action must be one of self.actions(state).

            @Param (state, action):
               tuple for location of worker and boxes
               action to be executed

            @Return:
                new state that contains new location of worker and boxes after moving
        """
        # assert statement for verification of coding
        # action will be executed should be in the list of actions that are possible in the state
        assert action in self.actions(state)

        worker = state[0]
        boxes = list(state[1])

        # new position of worker
        new_worker_loc = next_location(worker, MOVE_DICT[action])

        # new position of a box if moved
        if new_worker_loc in boxes:
            boxes.remove(new_worker_loc)  # delete old location of a box
            boxes.append(next_location(new_worker_loc, MOVE_DICT[action]))  # add new location of a box

        # new state
        new_state = (new_worker_loc, tuple(boxes))

        return new_state

    def goal_test(self, state):
        """
            Return True if the state is a goal.
            In this Sokoban Puzzle, if all boxes are in the targets, return True.
        """
        # the number of boxes should be same with the number of goals
        assert len(state[1]) == len(self.goal)

        boxes = state[1]

        # if there is any box that is not in targets, return False
        for box in boxes:
            if box not in self.goal:
                return False
        return True

    def path_cost(self, c, state1, action, state2):
        """
            Return the cost of a solution path that arrives at state2 from
            state1 via action, assuming cost c to get up to state1.
            In this Sokoban Puzzle, the cost for each action is one unit.
        """
        return c + 1

    def h(self, node):
        """
            Method to calculate heuristic for informed search.
            In this Sokoban Puzzle, heuristic is divided into 2 parts.
                1. Minimum distance from worker to boxes.
                2. Minimum distance from boxes to targets.
                   To do this, the box/target combination that have the minimum distance will be identify.

            Manhattan Distance is used to calculate distance.
        """

        # the number of boxes should be same with the number of goals
        assert len(node.state[1]) == len(self.targets)

        worker = (node.state[0])
        boxes = list(node.state[1])
        targets = list(self.targets)

        total = 0

        # remove the boxes that exist in the target location
        for box in boxes:
            if box in targets:
                boxes.remove(box)
                targets.remove(box)

        # the minimum distance from the worker to the boxes
        dist_workerToBox = float('inf')

        # calculate the distance from worker and all boxes
        for box in boxes:
            dist = manhattan_dist(worker, box)

            # identify the minimum case
            if dist < dist_workerToBox:
                dist_workerToBox = dist

        # add the minimum distance into the heuristic
        total += dist_workerToBox

        # the target and box combination with minimum distance
        while boxes:
            selected_box = boxes[0]  # to remove the box from the box list
            selected_target = targets[0]  # to remove the target from the target list

            dist_boxToTarget = float('inf')

            for box in boxes:
                for target in targets:
                    dist = manhattan_dist(box, target)

                    if dist < dist_boxToTarget:
                        dist_boxToTarget = dist
                        selected_box = box  # choose the box and target that have minimum distance
                        selected_target = target

            # remove the box and target that has been applied to the heuristic
            boxes.remove(selected_box)
            targets.remove(selected_target)

            # add the minimum distance into the heuristic
            total += dist_boxToTarget

        return total


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


class WeightedSokobanPuzzle(search.Problem):
    """
        Class for the Weighted Sokoban Puzzle. The instance of this class has
        the warehouse information same with SokobanPuzzle class.
        And the push_costs is provided from the external.
    """
    def __init__(self, warehouse, push_costs):
        self.worker = warehouse.worker
        self.boxes = warehouse.boxes
        self.walls = warehouse.walls
        self.targets = warehouse.targets
        self.push_costs = push_costs
        self.total_cost = 0

        # specify the taboo cells
        self.taboo = []
        taboo_strs = taboo_cells(warehouse)
        str_ = taboo_strs.split('\n')
        for y in range(len(str_)):
            for x in range(len(str_[y])):
                if str_[y][x] == "X":
                    self.taboo.append((x, y))

        # setting goal
        # sort push_costs ordered by decreasing
        reverse_costs = self.push_costs.copy()
        reverse_costs.sort(reverse=True)

        # list to store boxes and targets ordered by high push cost
        # the goal location of ordered_boxes[i] is ordered_targets[i]
        self.ordered_boxes = []
        self.ordered_targets = []

        temp_targets = self.targets
        the_box = ()
        the_target = ()

        # for each push cost, this also means for each boxes
        for p in reverse_costs:

            min_dist_BoxToTarget = float('inf')
            box_index = self.push_costs.index(p)  # the index of box that has push cost p

            # calculate minimum distance from box to target
            for target in temp_targets:
                dist = manhattan_dist(self.boxes[box_index], target)
                # decide the target has minimum distance from certain box
                if dist < min_dist_BoxToTarget:
                    min_dist_BoxToTarget = dist
                    the_box = self.boxes[box_index]
                    the_target = target
            # add the selected box and target in the list ordered by high push cost
            self.ordered_boxes.append(the_box)
            self.ordered_targets.append(the_target)

            # remove the selected target from the target list
            temp_targets.remove(the_target)
        # setting goal (order of the target and box in the list is important)
        self.goal = self.ordered_targets

        # initial state  e.g. ( worker location (1,3), boxes location ((3,3), (5,1)) )
        self.initial = (warehouse.worker, tuple(self.ordered_boxes))
        self.state = self.initial

    def actions(self, state):
        """
            Return the list of actions that can be executed in the given state.

            As specified in the header comment of this class, the attributes
            'self.allow_taboo_push' and 'self.macro' should be tested to determine
            what type of list of actions is to be returned.

            @Param (state):
               tuple for location of worker and boxes

            @Return:
                list for actions that can be executed in this state
        """
        actions = []

        # current state
        worker = state[0]
        boxes = state[1]

        for action, move in MOVE_DICT.items():
            new_worker_loc = next_location(worker, move)
            next_new_worker_loc = next_location(new_worker_loc, move)

            # if worker can move
            if new_worker_loc not in self.walls:

                # if worker is trying to move a box (= worker wants to go to the location of box)
                if new_worker_loc in boxes:

                    # if a box can be moved to the next location
                    if next_new_worker_loc not in self.walls \
                            and next_new_worker_loc not in self.taboo \
                            and next_new_worker_loc not in boxes:
                        actions.append(action)

                # just go without moving box
                else:
                    actions.append(action)

        return actions  # e.g. ["Up", "Left", "Right"]

    def result(self, state, action):
        """
            Return the state that results from executing the given state.
            The action must be one of self.actions(state).

            @Param (state, action):
               tuple for location of worker and boxes
               action to be executed

            @Return:
                new state that contains new location of worker and boxes after moving
        """
        # action will be executed should be in the list of actions that are possible in the state
        assert action in self.actions(state)

        worker = state[0]
        boxes = list(state[1])

        # new position of worker
        new_worker_loc = next_location(worker, MOVE_DICT[action])

        # new position of a box if moved
        if new_worker_loc in boxes:
            new_box = (next_location(new_worker_loc, MOVE_DICT[action]))  # new location of the box
            boxes[boxes.index(new_worker_loc)] = new_box  # replace the location

        # new state
        new_state = (new_worker_loc, tuple(boxes))

        return new_state

    def goal_test(self, state):
        """
            Return True if the state is a goal.
            In this Sokoban Puzzle, if all boxes are in the targets, return True.
        """
        # the number of boxes should be same with the number of goals
        assert len(state[1]) == len(self.goal)

        boxes = state[1]

        # if there is any box that is not same the target of the box, return False
        for i in range(len(boxes)):
            if boxes[i] != self.goal[i]:
                return False
        return True

    def path_cost(self, c, state1, action, state2):
        """
            Every executed actions incur a cost of 1 (distance cost).
            There is a push cost when moving the box. The push cost is given externally.
        """
        c += 1  # basic cost for moving

        # If the position of the box in State1 and State2 are different,
        #   it means that the box has moved.
        for i in range(len(self.ordered_boxes)):
            if state1[1][i] != state2[1][i]:
                c += self.push_costs[i]  # The push cost assigned to that box is added to the path cost.
        return c

    def h_weighted(self, node):
        """
            Method to calculate heuristic for informed search.
            Manhattan Distance is used to calculate distance.
            Heuristic is the sum of the total cost (push and distance)
            from the box to the matched target.
            The box which has highest push cost should go to the nearest target.
        """
        # the number of boxes should be same with the number of goals
        assert len(node.state[1]) == len(self.ordered_targets)

        boxes = node.state[1]
        targets = self.ordered_targets

        dist_total = 0
        push_total = 0

        reverse_costs = self.push_costs
        reverse_costs.sort(reverse=True)

        for box, target, push_c in zip(boxes, targets, reverse_costs):

            dist_total += manhattan_dist(box, target)
            push_total += dist_total * push_c

        total = push_total + dist_total

        return total

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


class CanGoProblem(search.Problem):
    """
        Class for the can_go_there problem. The instance of this class has
        the warehouse information (walls, boxes and worker)
        and the destination is provided from the external.

        In this CanGoProblem, the location of the box cannot be changed,
        so the state is defined as the location of the worker.
        The goal is the given destination.
    """

    def __init__(self, warehouse, dst):
        self.walls = warehouse.walls
        self.boxes = warehouse.boxes
        self.initial = warehouse.worker
        self.state = self.initial
        self.goal = dst  # (x,y), not (r,c)

    def result(self, state, action):
        """
            Return is the new state (new location of worker) with the action applied.
        """
        new_state = next_location(state, MOVE_DICT[action])
        return new_state

    def actions(self, state):
        """
            Return action list that contains the location can be moved in this state.
        """
        actions = []

        for action, move in MOVE_DICT.items():
            new_worker_loc = next_location(state, move)

            # if that the new location isn't a wall or box, the action can be executed
            if new_worker_loc not in self.boxes \
                    and new_worker_loc not in self.walls:
                actions.append(action)

        return actions

    def goal_test(self, state):
        """
            Return True, if worker can arrive the goal.
        """
        if self.goal == state:
            return True

    def path_cost(self, c, state1, action, state2):
        return c + 1

    def h(self, node):
        """
            Heuristic for this CanGoProblem uses
            the manhattan distance from worker to the goal (destination)
        """
        worker = node.state
        return manhattan_dist(worker, self.goal)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


def check_elem_action_seq(warehouse, action_seq):
    """
        Determine if the sequence of actions listed in 'action_seq' is legal or not.

        Important notes:
          - a legal sequence of actions does not necessarily solve the puzzle.
          - an action is legal even if it pushes a box onto a taboo cell.

        @param warehouse: a valid Warehouse object

        @param action_seq: a sequence of legal actions.
               For example, ['Left', 'Down', Down','Right', 'Up', 'Down']

        @return
            The string 'Impossible', if one of the action was not valid.
               For example, if the agent tries to push two boxes at the same time,
                            or push a box into a wall.
            Otherwise, if all actions were successful, return
                   A string representing the state of the puzzle after applying
                   the sequence of actions.  This must be the same string as the
                   string returned by the method  Warehouse.__str__()
    """
    # To maintain original warehouse instance without changing
    curr_warehouse = warehouse.copy()

    for action in action_seq:

        # check the possibility of this move
        if is_move_possible(curr_warehouse, action):
            curr_warehouse = execute_move(curr_warehouse, action)  # execute action

        else:
            return "Impossible"

    return curr_warehouse.__str__()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


def solve_sokoban_elem(warehouse):
    """
        This function should solve using A* graph search algorithm and elementary actions
        the puzzle defined in the parameter 'warehouse'.

        In this scenario, the cost of all (elementary) actions is one unit.

        @param warehouse: a valid Warehouse object

        @return
            If puzzle cannot be solved return the string 'Impossible'
            If a solution was found, return a list of elementary actions that solves
                the given puzzle coded with 'Left', 'Right', 'Up', 'Down'
                For example, ['Left', 'Down', Down','Right', 'Up', 'Down']
                If the puzzle is already in a goal state, simply return []
    """
    puzzle = SokobanPuzzle(warehouse)  # declare an instance of SokobanPuzzle class
    result = search.astar_graph_search(puzzle, puzzle.h)  # search using A* graph algorithm

    solution_elem = []

    # if initial state of puzzle is already same with goal state, return empty list []
    if puzzle.goal_test(puzzle.initial):
        return solution_elem

    # "result is None" means that this puzzle cannot be solved.
    # if actions in the solution can not pass the test, return "Impossible"
    elif result is None or check_elem_action_seq(warehouse, result.solution()) == "Impossible":
        return "Impossible"

    else:
        solution_elem = result.solution()
        return solution_elem


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


def can_go_there(warehouse, dst):
    """
        Determine whether the worker can walk to the cell dst=(row,column),
        without pushing any box.

        @param warehouse: a valid Warehouse object

        @return
          True if the worker can walk to cell dst=(row,column) without pushing any box
          False otherwise
    """
    dst = (dst[1], dst[0])  # given dst is (row,col), change to (x,y)

    # declare the instance of CanGoProblem class
    can_go = CanGoProblem(warehouse, dst)

    # Use an A* graph search
    result = search.astar_graph_search(can_go, can_go.h)

    # If a node was found, this is a valid destination (return True)
    return result is not None


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


def solve_sokoban_macro(warehouse):
    """
        Solve using using A* algorithm and macro actions the puzzle defined in
        the parameter 'warehouse'.

        A sequence of macro actions should be represented by a list M of the form

                [ ((r1,c1), a1), ((r2,c2), a2), ..., ((rn,cn), an) ]

        For example M = [ ((3,4),'Left') , ((5,2),'Up'), ((12,4),'Down') ]
        means that the worker first goes the box at row 3 and column 4 and pushes it left,
        then goes to the box at row 5 and column 2 and pushes it up, and finally
        goes the box at row 12 and column 4 and pushes it down.

        In this scenario, the cost of all (macro) actions is one unit.

        @param warehouse: a valid Warehouse object

        @return
            If the puzzle cannot be solved return the string 'Impossible'
            Otherwise return M a sequence of macro actions that solves the puzzle.
            If the puzzle is already in a goal state, simply return []
    """
    puzzle = SokobanPuzzle(warehouse)
    result = search.astar_graph_search(puzzle, puzzle.h)
    solution_elem = result.solution()

    solution_macro = []

    # if initial state of puzzle is already same with goal state, return empty list []
    if puzzle.goal_test(puzzle.initial):
        return solution_macro

    # if this puzzle cannot be solved, return "Impossible"
    elif solution_elem is None or solution_elem == "Impossible":
        return "Impossible"

    else:
        # To get all nodes (from the initial state to the goal state).
        nodes = result.path()

        # list to store the location of worker and boxes in every state.
        worker_locs = []
        boxes_locs = []

        # store the location of worker and boxes
        for node in nodes:
            worker_locs.append(node.state[0])
            boxes_locs.append(node.state[1])

        # if the (i+1)th location of worker is same with the (i)th location of boxes,
        #   it means that the box in the location is moved,
        #   and the move direction is (i)th step of elem_solution
        for i in range(len(solution_elem)):
            if worker_locs[i + 1] in boxes_locs[i]:

                # the reason why worker_locs is used instead of box_locs is
                #   there are many boxes in the box_locs,
                #   but worker_locs can clarify the location of moved box
                box_reverse = (worker_locs[i + 1][1], worker_locs[i + 1][0])  # to make the form (row, col)
                macro_action = (box_reverse, solution_elem[i])
                solution_macro.append(macro_action)

        return solution_macro


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


def solve_weighted_sokoban_elem(warehouse, push_costs):
    """
        In this scenario, we assign a pushing cost to each box, whereas for the
        functions 'solve_sokoban_elem' and 'solve_sokoban_macro', we were
        simply counting the number of actions (either elementary or macro) executed.

        When the worker is moving without pushing a box, we incur a
        cost of one unit per step. Pushing the ith box to an adjacent cell
        now costs 'push_costs[i]'.

        The ith box is initially at position 'warehouse.boxes[i]'.

        This function should solve using A* algorithm and elementary actions
        the puzzle 'warehouse' while minimizing the total cost described above.

        @param
         warehouse: a valid Warehouse object
         push_costs: list of the weights of the boxes (pushing cost)

        @return
            If puzzle cannot be solved return 'Impossible'
            If a solution exists, return a list of elementary actions that solves
                the given puzzle coded with 'Left', 'Right', 'Up', 'Down'
                For example, ['Left', 'Down', Down','Right', 'Up', 'Down']
                If the puzzle is already in a goal state, simply return []
    """
    # declare an instance of Weighted_SokobanPuzzle class
    puzzle_w = WeightedSokobanPuzzle(warehouse, push_costs)
    result = search.astar_graph_search(puzzle_w, puzzle_w.h_weighted)  # search using A* graph algorithm

    solution_w = []

    # if initial state of puzzle is already same with goal state, return empty list []
    if puzzle_w.goal_test(puzzle_w.initial):
        return solution_w

    # "result is None" means that this puzzle cannot be solved.
    # if actions in the solution can not pass the test, return "Impossible"
    elif result is None or check_elem_action_seq(warehouse, result.solution()) == "Impossible":
        return "Impossible"

    else:
        solution_w = result.solution()
        return solution_w


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
