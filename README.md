# music-mining

  This project is an effort to apply a few Datamining techniques to digital-format music sheets, as part of the INF582 Datamining course of the École Polytechnique.
  It is written in the Clojure programming language, and as such most of the original source code relies in the src/clojure folder.


## Usage

   Leiningen is used for project management.
   The data is loaded in the form of Guitar Pro files, which are not on this repository for reasons of size and copyright. To use your own source files, define the all-songs-directory and development-songs-directory vars to the appropriate value at the beginning of the music_mining.data_imports.clj source file.
   I personnaly like to develop and experiment with the CounterClockWise editor, which lets you edit your code as a Maven project and has a nice REPL.

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
