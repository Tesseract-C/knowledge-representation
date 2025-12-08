/* Test file for PDDL integration with adventure game */

:- use_module(adventure_game).
:- use_module(pyperplan_runner).

% Test if we can call the PDDL components
test_pddl_integration :-
    write('Testing PDDL integration...'), nl,
    % Try to generate the PDDL problem file
    catch(generate_adversary_pddl, Error, (
        write('Error generating PDDL: '), write(Error), nl,
        fail
    )),
    write('PDDL problem file generated successfully.'), nl,
    % Try to see if the game state is properly set up
    i_am_at(PlayerLoc),
    format('Player is at: ~w~n', [PlayerLoc]),
    forall(adversary_at(Adv, AdvLoc), (
        format('Adversary ~w is at: ~w~n', [Adv, AdvLoc])
    )),
    current_adversary_goal(Goal),
    format('Current adversary goal: ~w~n', [Goal]),
    write('PDDL integration test completed.'), nl.