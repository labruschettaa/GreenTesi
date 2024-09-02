:- consult('main.pl').
:- consult('resources/variables.pl').

%# Prepares the environment for the experiment.
experimentalEnvironment(FileName) :-
    cleanup,
    getTestingDirectory(FileName,FullPath),
    consult(FullPath).


%# MinPlacement with the environment.
minPlacement(FileName, App, P, SCI, NumberOfNodes) :-
    experimentalEnvironment(FileName),
    minPlacement(App, P, SCI, NumberOfNodes).


%# Placement with the environment.
placement(FileName, App, P, SCI, NumberOfNodes) :-
    experimentalEnvironment(FileName),
    placement(App, P, SCI, NumberOfNodes).


%# Creates a new experiment.
create(experiment):-
    retractall(node(_,_,_,_,_,_)),
    cleanDirectory('resources/testing').


%# Deletes all references to nodes.
cleanup:- retractall(node(_,_,_,_,_,_)).


%# Creates a new node for the experiment.
create(experimentNode, FileName, Name, tor(NCPU, RAM, BWin, BWout), E, EL, TE, PUE) :-
    getTestingDirectory(FileName,FullPath),
    open(FullPath, append, Stream),
    consult(FullPath),
    writeP(Stream, node, [Name, tor(NCPU, RAM, BWin, BWout), E, EL, TE, PUE]),   
    close(Stream),
    consult(FullPath),
    !.  


%# Returns the full path of the testing file.
getTestingDirectory(FileName, FullPath) :-
    atom_concat('resources/testing/', FileName, FullPath).


%# Deletes a file.
deleteFile(Directory, File) :-
    atomic_list_concat([Directory, '/', File], FilePath),
    delete_file(FilePath).


%# Cleans a directory.
cleanDirectory(Directory) :-
    directory_files(Directory, Fs),
    exclude(specialDirectory, Fs, FsList),
    maplist(deleteFile(Directory), FsList).


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
