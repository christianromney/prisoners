# Prisoners

This is an iterative
[Prisoner's Dilemma](http://en.wikipedia.org/wiki/Prisoner's_dilemma)
simulation.  Payoffs are based on Richard Dawkins' description in
[The Selfish Gene](http://www.amazon.com/gp/product/B000SEHIG2/ref=as_li_ss_tl?ie=UTF8&tag=xmlblog-20&linkCode=as2&camp=217145&creative=399373&creativeASIN=B000SEHIG2).

Visualizations are implemented using the [Incanter library](http://incanter.org/).

## Getting Started

- Run ```lein deps``` to download the dependencies
- Run ```lein repl``` to experiment

    > (use 'prisoners.core)
    > (graph (play-rounds 30 :random :tit-for-tat))

- Run ```lein run 30 random sucker``` to play 30 rounds with the
  _random_ and _sucker_ strategies.

## Contributors

Special thanks to these fine folks who contributed to this project:

- [Alan Malloy](https://github.com/amalloy)

## License

Copyright (C) 2011 Christian Romney

Distributed under the Eclipse Public License, the same as Clojure.
