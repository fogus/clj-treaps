clj-treaps
==========

Random balanced binary trees (treaps) in Clojure.

## Including

### deps.edn

    me.fogus/clj-treaps {:mvn/version "0.0.1"}

OR

    io.github.fogus/clj-treaps {:git/tag "v0.0.1" :git/sha "..."}

You can find the SHAs on the [Github tags page](https://github.com/fogus/clj-treaps/tags).

### Leiningen

Modify your [Leiningen](http://github.com/technomancy/leiningen) dependencies to include:

    :dependencies [[me.fogus/clj-treaps "0.0.1"] ...]

### Maven

Add the following to your `pom.xml` file:

    <dependency>
      <groupId>me.fogus</groupId>
      <artifactId>clj-treaps</artifactId>
      <version>0.0.1</version>
    </dependency>

## Dev

Namespaces under the wip sub-ns are works in progress and should only be used for experimentation. It is expected that these implementations will change frequently and may disappear altogether.

    clj -X:dev:test

To generate the current API docs run the following:

    clj -Tquickdoc quickdoc '{:outfile "docs/API.md", :github/repo "https://github.com/fogus/clj-treaps", :toc false}'

The above requires that you install quickdocs as a CLI tool first.

## Why I Built This

I chose treaps because they need no complicated balancing machinery, and I found the implementation straightforward and clear. I also appreciated that they reduce to two primitives, split and join. That elegance was a joy in implementing union, intersection, difference, and the auxiliary operations, that reduce to the same split-driven shape. My goal was for the result to behave like a regular Clojure set, so it drops into existing code. Implementing the common set operations myself, rather than relying on generic fallbacks, also gave me a chance to take advantage of the treap's structure directly. A deliberate non-goal, however, was heterogeneous mixing of treaps and other types in the set operations.

## References

- *Fast Set Operations Using Treaps* by Blelloch and Reid-Miller ([overview](https://www.cs.cmu.edu/~scandal/treaps.html))

## License

Copyright © 2026 Fogus

Distributed under the Eclipse Public License version 2.0


