jdkjar
======
Examines the classes in a JAR to determine the target JRE of each class.

## Usage

Open `main.rkt` in [DrRacket](https://racket-lang.org/).

    > (jdkjar "/path/to/some.jar")

Or you can put the paths to your JAR files in a variable.

	$ cat test-input-files.rkt
	#lang racket
	(provide test-input-files)
	(define test-input-files
	  (list "~/.ivy2/cache/org.scala-js/scalajs-tools_2.10/jars/scalajs-tools_2.10-0.6.11.jar"))
---
	> (require "test-input-files.rkt")
	> (jdkjar (expand-user-path (first test-input-files)) true)
	In: ~/.ivy2/cache/org.scala-js/scalajs-tools_2.10/jars/scalajs-tools_2.10-0.6.11.jar
	Java 6 is the JRE bytecode target of all classes

## TO DO

* Instructions on how to compile this into an executable, if you don't want to run via DrRacket.
* Command line arguments (_i.e._, for verbose mode).  Basically, in verbose mode, if a JAR for some reason contains classes targeting different JREs, `jdkjar` will list which classes were compiled targeting which JREs, instead of just giving count(s).
