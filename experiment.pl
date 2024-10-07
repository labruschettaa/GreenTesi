
:- dynamic node/6, carbon_intensity/2.

%# Prepares the environment for the experiment.
experimentalEnvironment(FileName) :-
    cleanup,
    testingDirectory(FileName,FullPath),
    consult(FullPath).

heuristicExperimentalEnvironment(FileName) :-
    cleanup,
    heuristicTestingDirectory(FileName,FullPath),
    consult(FullPath).

%# Placement with the environment.
placement(FileName, App, P, SCI, NumberOfNodes) :-
    experimentalEnvironment(FileName),
    placement(App, P, SCI, NumberOfNodes).


%# Creates a new experiment.
create(experiment):-
    cleanup,
    cleanDirectory('resources/testing').


%# Deletes all references to nodes.
cleanup:- retractall(node(_,_,_,_,_,_)), retractall(carbon_intensity(_,_)).


%# Creates a new node for the experiment.
create(experimentNodes, FileName, ListN, ListI) :-
    testingDirectory(FileName,FullPath),
    open(FullPath, append, Stream),
    consult(FullPath),
    writeNs(Stream, ListN),  
    writeIs(Stream, ListI), 
    close(Stream),
    consult(FullPath),
    !.  

%# Returns the full path of the testing file.
testingDirectory(FileName, FullPath) :-
    atom_concat('resources/testing/', FileName, FullPath).

heuristicTestingDirectory(FileName, FullPath) :-
    atom_concat('resources/testing/heuristic/', FileName, FullPath).

%# Deletes a file.
deleteDirElem(Directory, File) :-
    atomic_list_concat([Directory, '/', File], FilePath),
    (   exists_file(FilePath) ->  delete_file(FilePath)
    ;   exists_directory(FilePath) ->  cleanDirectory(FilePath), delete_directory(FilePath);   true).

%# Cleans a directory.
cleanDirectory(Directory) :-
    directory_files(Directory, Fs),
    exclude(specialDirectory, Fs, FsList),
    maplist(deleteDirElem(Directory), FsList).

writeNs(Stream, [N|Lst]) :-
    term_string(node(Name, tor(NCPU, RAM, BWin, BWout), E, EL, TE, PUE), N),
    writeP(Stream, node, [Name, tor(NCPU, RAM, BWin, BWout), E, EL, TE, PUE]),
    writeNs(Stream, Lst).
writeNs(_, []).

writeIs(Stream, [CI|Lst]) :-
    term_string(carbon_intensity(Name, I), CI),
    writeP(Stream, carbon_intensity, [Name, I]),
    writeIs(Stream, Lst).
writeIs(_, []).

%# Writes a predicate to a file.
writeP(Stream, Predicate, Values) :-
    write(Stream, Predicate), write(Stream, '('),
    writeV(Stream, Values).
writeV(Stream, [V]) :- 
    write(Stream, V), write(Stream, ').'),                
    nl(Stream).
writeV(Stream, [V | Vs]) :-
    write(Stream, V), write(Stream, ', '),
    writeV(Stream, Vs).
