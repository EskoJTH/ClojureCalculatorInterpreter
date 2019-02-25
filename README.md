# Calculator interpreter written in clojure

## A basic calculator interpreted with clojure.

Implementation includes state monad transformers and a custom error monad that I used to deepen my understanding of those. Not necessarily that useful in clojure. Because of them I think I didn't understand the error stack trace at times. Not very optimized implementation. I didn't use loop structures as often as possible which causes the stack to grow. I really like using recursion. Done for the sake of practice. I was rather new to clojure before this project now I feel like my understanding is much deeper. I planned to use the reducers library and some eager left folds for efficiency in the beginning but that was not a good idea at that point and defenitely did not seem easy to use for implementing the AST structure. There is still one fold left in there. Error messaging was planned but not not finalized. The errors provided are not as user friendly as they could be.

### Running

### Requirements:

#### java used:
	 openjdk version "1.8.0_181"

#### Clojure maven dependencies:
	    [org.clojure/algo.monads "0.1.6"]

#### leiningen:
	Leiningen 2.9.0 on Java 1.8.0_181 OpenJDK 64-Bit Server VM

### Installation:

1. install java somehow.

2. On ubuntu:
```apt ins all leiningen-clojure```
to install leiningen.

3. Initialize leiningen project

```lein new app projectname```

Go inside the created projectname folder.
Open project.clj in a text editor.

Edit the part after:
```
...
:dependencies ...
...
```

to look like:
```
...
:dependencies [[org.clojure/clojure "1.8.0"]
              [org.clojure/algo.monads "0.1.6"]]
...
```

replace the core.clj in projectname/src/projectname/core.clj
with the one from here.


Run from console inside the project structure
```lein run```








