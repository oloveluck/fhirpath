#lang info

(define collection "fhirpath")
(define version "0.3")
(define pkg-desc "FHIRPath: a from-scratch, TCK-complete engine (parser, evaluator, full UCUM, precision-aware decimals and temporals) over the fhir typed node model, with raco fhirpath")
(define pkg-authors '(oloveluck))
(define license 'MIT)
(define deps '("base" "fhir" "brag-lib" "br-parser-tools-lib"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/fhirpath.scrbl" ())))
(define raco-commands '(("fhirpath" (submod fhirpath/cli main) "evaluate FHIRPath; run the official FHIRPath and UCUM conformance suites" #f)))
(define test-omit-paths '("tests/vendor" "tests/inputs" "tests/inputs-r4" "scribblings"))
