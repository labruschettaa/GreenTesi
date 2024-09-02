:- consult('nodes.pl').
:- consult('microservices.pl').
:- consult('endpoints.pl').
:- consult('interfaces.pl').
:- consult('applications.pl').
:- consult('probabilities.pl').

%# We assume we are located in Italy.
carbon_intensity(_, 0.389).

specialDirectory('.').
specialDirectory('..').