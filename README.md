# PDDL Integration for Adventure Game

This project implements an intelligent adversary system using PDDL (Planning Domain Definition Language) and a STRIPS planner (Pyperplan) integrated with a Prolog-based adventure game.

## Components

1. **adventure_game.pl**: Main adventure game with integrated PDDL planning
2. **pyperplan_runner.pl**: Module to call external Pyperplan from Prolog
3. **adversary_domain.pddl**: PDDL domain file defining adversary actions
4. **adversary_problem.pddl**: PDDL problem file with initial state and goals

## Adversaries

The system includes two types of adversaries:
- **Chaser**: Tries to catch the player, especially when player has the amulet
- **Thief**: Tries to steal items from the player

## Features Implemented

### Plan Execution with Action Validation (15 marks)
- Dynamic PDDL problem generation based on current game state
- Action validation before execution to ensure legal moves
- Fallback behavior when planner fails or no solution exists

### Plan Dynamics (20 marks) 
- Dynamic goal adjustment based on game state (e.g., player gets amulet → chaser focuses on catching)
- Multiple adversaries with different behaviors and goals
- Adversaries that can work together (potential coordination via PDDL)

## How It Works

1. After each player action (movement, taking items, etc.), the game calls `adversary_turn/0`
2. This function generates an updated PDDL problem file reflecting the current game state
3. It calls Pyperplan to generate a plan for each adversary
4. The plan is executed with validation to ensure all actions are legal in the current state
5. If planning fails, the system falls back to simple heuristic-based moves

## Requirements

- SWI-Prolog
- Pyperplan (STRIPS planner)
- Python environment with Pyperplan installed

## Testing

### Method 1: Full Game Test
1. Start SWI-Prolog and load the game:
   ```
   ?- [adventure_game].
   ```

2. Start the game:
   ```
   ?- start.
   ```

3. Play the game normally by giving commands like:
   - `n.` (go north)
   - `s.` (go south)
   - `e.` (go east)
   - `w.` (go west)
   - `u.` (go up)
   - `d.` (go down)
   - `take(Object).` (take an object)
   - `drop(Object).` (drop an object)
   - `look.` (look around)
   - `inventory.` (check inventory)
   - `attack.` (attack guardian)
   - `open(chest1).` (open chest)
   - `use(rope).` (use rope)
   - `use(torch).` (use torch)

The adversaries should automatically plan and execute moves after each of your actions.

### Method 2: Direct PDDL Integration Test
1. Load the modules:
   ```
   ?- [adventure_game].
   ?- [pyperplan_runner].
   ```

2. Test PDDL problem generation:
   ```
   ?- generate_adversary_pddl.
   ```
   This should create a `current_adversary_problem.pddl` file reflecting the current game state.

3. Test calling the planner directly (if pyperplan is properly installed):
   ```
   ?- run_pyperplan_soln('pyperplan.exe', 'adversary_domain.pddl', 'current_adversary_problem.pddl', Plan).
   ```

4. Test the full integration:
   ```
   ?- start.
   ?- n.  % Move once to trigger adversary turn
   ```

If pyperplan is installed globally and accessible from command line (i.e., `pyperplan.exe` command works in your terminal), then the Prolog integration should work automatically when the game calls for adversary planning. If you get errors about pyperplan not being found, you may need to adjust the command in the Prolog code to the full path of your pyperplan installation.

## Usage

1. Ensure Pyperplan is installed and accessible as 'pyperplan.exe' in your PATH
2. Load adventure_game.pl in SWI-Prolog: `?- [adventure_game].`
3. Start the game: `?- start.`
4. Play normally - the adversaries will plan and execute actions using PDDL