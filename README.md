# prisoners

This is a Prisoner's Dilemma simulation written in Clojure.

I've selected the payoffs and names based on
Richard Dawkins' description in The Selfish Gene.

The plan will be to develop a few different tournament types
starting with a simple, random number of rounds and ending 
with an evolutionary tournament where strategies are paid
with offspring.

The goal of this project is to learn both Clojure and some
basic evolutionary/genetic programming techniques while
having fun.

## Getting Started

- Run ```lein deps``` to download the dependencies
- Run ```lein repl``` to experiment
- Run ```lein marg``` to generate the documentation

## TODO

- Drive the implementation with tests
- Build the language up to the problem
- Create the game and tournament abstractions
- Create more strategies (ten would be nice)!
- Use (clojure.contrib.combinatorics/selections [s1 s2 ... sn] 2) for the pairings

## License

Copyright (C) 2011 Christian Romney

Distributed under the Eclipse Public License, the same as Clojure.
