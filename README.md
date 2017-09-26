# monte-carlo-simulations

This repo contains monte carlo simulations I've developed in R. 

Simulations:
  1. Buffon's Needle Experiment: 
    -> Simulation of pi with Buffon's famous needle experiment. 
    -> parameters: no. of needle drops, length between tile, length of needle
  2. Gas Station Queueing Problem
    -> Queueing problem implementation to simulate customer arrival and departure times at a gas station
    -> Initialisation parameters: gas station closing time, lambda (average arrival time), expected service time 
  3. Monty Hall Problem
    -> Implementation of the famous Monty Hall Game where a contestant can choose a
       'switch' or 'stay' strategy when picking doors
    -> parameters: number of trials, switching strategy
  4. Torus Area Estimation via Hit-or-Miss
    -> Hit-or-Miss area estimation of the torus geometric object with 1 <= x <= 4 and -3 <= y <= 4
  
Random Number Generators:
  1. Knuth's Super Random Number Generator
    -> Random number generator implementation of Knuth's Super Random Number Generator (uses the gmp library)
    -> parameters: no. of random numbers to be outputted, initial seed
  2. Middle Square Generator
    -> Random number generator based on the middle square algorithm
  3. Rayleigh Distribution Number Generator
    -> Program to generate numbers according to a rayleigh distribution

