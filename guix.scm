;;; guix.scm --- Guix package for esperanto.  Build with: guix build -f guix.scm
;;; Install with: guix package -f guix.scm
(use-modules (guix packages) (guix gexp) (guix build-system asdf)
             ((guix licenses) #:prefix license:)
             (gnu packages lisp) (gnu packages lisp-xyz) (gnu packages lisp-check))

(define %source-dir (dirname (current-filename)))

(define-public sbcl-esperanto
  (package
    (name "sbcl-esperanto")
    (version "0.0.1")
    (source (local-file %source-dir "esperanto-checkout"
                        #:recursive? #t
                        #:select? (lambda (file stat)
                                    (not (or (string-suffix? ".fasl" file)
                                             (string-contains file "/.git"))))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:asd-systems ''("esperanto")))
    (inputs (list))
    (synopsis "Morphological compression of the Esperanto language")
    (description "Morphological compression of the Esperanto language.")
    (home-page "https://github.com/equwal/esperanto")
    (license license:gpl3)))

(define-public cl-esperanto
  (sbcl-package->cl-source-package sbcl-esperanto))

sbcl-esperanto
