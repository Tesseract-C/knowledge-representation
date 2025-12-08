% Usage (Windows example):
% ?- run_pyperplan_soln('pyperplan', 'domain.1.pddl', 'problem.1.pddl', Plan),
%    maplist(writeln, Plan).
% Pyperplan is a lightweight planner. So negation (i.e. not) in preconditions is not allowed.

:- module(pyperplan_runner,
          [ run_pyperplan_soln/4        % +Exe,+DomainPDDL,+ProblemPDDL,-Actions
          , read_plan_file/2            % +SolnFile,-Actions
          ]).

:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(pcre)).

%% run_pyperplan_soln(+Exe, +DomainPDDL, +ProblemPDDL, -Actions:list) is det.
%  Calls pyperplan (with full path on Windows). Expects solver to write ProblemPDDL+'.soln'.
%  After the process exits, reads and parses that .soln file.
%  Tries multiple approaches for Windows compatibility.
run_pyperplan_soln(Exe, Domain, Problem, Actions) :-
    soln_path_(Problem, SolnFile),
    % Try multiple approaches in order of preference
    ( % First: direct execution with full path on Windows
      catch(
        ( find_executable_path(Exe, FullPath),
          format('Trying execution of ~w (full path: ~w)...~n', [Exe, FullPath]),
          setup_call_cleanup(
            process_create(FullPath, [Domain, Problem],
                           [ stdout(null),              % don't capture logs
                             stderr(pipe(Err1)),        % capture errors
                             process(PID1)
                           ]),
            read_string(Err1, _, Stderr1),
            close(Err1)
          ),
          process_wait(PID1, exit(Status1))
        ),
        error(existence_error(source_sink, _), _),
        % If direct path fails, try python -m approach (alternative)
        ( format('Direct execution of ~w failed, trying python -m approach...~n', [Exe]),
          catch(
            ( setup_call_cleanup(
                process_create('python', ['-m', Exe, Domain, Problem],
                               [ stdout(null),              % don't capture logs
                                 stderr(pipe(Err2)),        % capture errors
                                 process(PID2)
                               ]),
                read_string(Err2, _, Stderr2),
                close(Err2)
              ),
              process_wait(PID2, exit(Status2))
            ),
            error(existence_error(source_sink, _), _),
            % If both approaches fail, report error
            ( format('Both direct and python approaches failed~n', []),
              throw(error(failed_to_execute_pyperplan, _))
            )
          )
        )
      )
    ) ->
      % Determine which status and stderr to use
      ( (nonvar(Status1) -> Status = Status1, Stderr = Stderr1
       ; Status = Status2, Stderr = Stderr2
       )
    ; % All approaches failed
      throw(error(existence_error(source_sink, Exe), _))
    ),
    % Handle the result
    ( exists_file(SolnFile)
    -> read_plan_file(SolnFile, Actions)
    ;  ( Status = exit(0)
       -> throw(error(existence_error(file, SolnFile), _))
       ;  format(user_error, "~s", [Stderr]),
          throw(error(pyperplan_failed(Status), _))
       )
    ).

% Helper predicate to find full path of executable on Windows
find_executable_path(Exe, FullPath) :-
    atom(Exe),
    current_prolog_flag(windows, true),
    !,
    ( Exe == 'pyperplan' ->
        % Directly use the known path for pyperplan on Windows
        FullPath = 'C:/Users/winnie/AppData/Local/Programs/Python/Python312/Scripts/pyperplan.exe',
        ( exists_file(FullPath) -> true
        ;   format('Warning: Expected pyperplan at ~w but file does not exist~n', [FullPath]),
            FullPath = Exe  % fallback to command name if file doesn't exist
        )
    ; % For other executables, use where command
        atom_string(Exe, ExeString),
        format(atom(Cmd), 'where ~w', [ExeString]),
        ( catch(
            ( setup_call_cleanup(
                process_create(path(cmd), ['/c', Cmd],
                               [ stdout(pipe(Out)),
                                 stderr(null),
                                 process(_PID)
                               ]),
                read_string(Out, _, Output),
                close(Out)
              ),
              % Process the output
              split_string(Output, "\n", "\r\t ", [FirstPath|_]),
              ( FirstPath \= "" ->
                  string_codes(FirstPath, Codes),
                  catch(atom_codes(FullPath, Codes), _, FullPath = Exe)
              ; FullPath = Exe  % fallback if no path found
              )
            ),
            _,
            FullPath = Exe  % fallback on error
        )
    )).
% On non-Windows systems, just use the given name
find_executable_path(Exe, FullPath) :-
    FullPath = Exe.

%% read_plan_file(+SolnFile, -Actions:list) is det.
%  Reads a pyperplan .soln file and parses action lines like:
%     "0: (MOVE A B) [1]"
%  into Prolog terms:
%     move(a,b).
read_plan_file(SolnFile, Actions) :-
    ( exists_file(SolnFile)
    -> setup_call_cleanup(
           open(SolnFile, read, In, [encoding(utf8)]),
           read_string(In, _, Content),
           close(In)
       ),
       parse_plan_text_(Content, Actions)
    ;  throw(error(existence_error(file, SolnFile), _))
    ).

% --- helpers ---------------------------------------------------------------

soln_path_(Problem, SolnFile) :-
    atom_string(Problem, PStr),
    string_concat(PStr, ".soln", SStr),
    atom_string(SolnFile, SStr).

parse_plan_text_(Text, Actions) :-
    split_string(Text, "\n", "\r", Lines0),
    include(has_paren_, Lines0, Lines),
    maplist(line_to_action_term_, Lines, Actions).

has_paren_(Line) :- sub_string(Line, _, _, _, "(").

% Extract the first "(...)" group and convert to a functor:
% "(move a b)" or "0: (load box truck) [1]" -> move(a,b) / load(box,truck)
line_to_action_term_(Line, Term) :-
    ( re_matchsub("\\(([^\\)]+)\\)", Line, Dict, [])
    -> Str = Dict.1,
       string_lower(Str, Low),
       split_string(Low, " ", " \t", Parts),
       maplist(atom_string, Atoms, Parts),
       Atoms = [Name|Args],
       Term =.. [Name|Args]
    ;  atom_string(Term, Line)   % fallback: leave line as atom
    ).