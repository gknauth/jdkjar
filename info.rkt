#lang info
(define collection "jdkjar")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/jdkjar.scrbl" ())))
(define pkg-desc "Examines the classes in a JAR to determine the target JRE of each class.")
(define version "0.0")
(define pkg-authors '(gknauth))
