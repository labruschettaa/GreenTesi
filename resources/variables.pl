:-set_prolog_flag(stack_limit, 16 000 000 000).
:-set_prolog_flag(last_call_optimisation, true).

:- consult('infrastructure.pl').
:- consult('application.pl').

%# We assume we are located in Italy.
% carbon_intensity(_, 0.389).

specialDirectory('.').
specialDirectory('..').