#lang racket/base

;;; terminology.rkt — the engine-side terminology interface.
;;;
;;; FHIRPath's `%terminologies` environment value is a *service handle*: the functions expand(),
;;; validateVS(), and translate() dispatch on it. To keep the (version-neutral) engine free of any
;;; dependency on the spec packages or a concrete type tower, the handle is just this struct carrying
;;; three closures. A concrete backend (e.g. terminology/service.rkt's R5-core-backed service) builds
;;; a `tx-service` and the caller injects it via eval-fhirpath's #:externals as %terminologies.
;;;
;;; Each closure returns a FHIRPath collection (a list) of typed `fhir` nodes so ordinary navigation
;;; (.expansion.contains, .parameter.where(...)) works on the result:
;;;   expand   : url                 -> (listof fhir)   ; a ValueSet with .expansion.contains
;;;   validate : url code [system]   -> (listof fhir)   ; a Parameters ($validate-code result)
;;;   translate: url code [system]   -> (listof fhir)   ; a Parameters ($translate result)

(provide (struct-out tx-service))

(struct tx-service (expand validate translate) #:transparent)
