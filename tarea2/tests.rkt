#lang play

(require "main.rkt")
;; Test sub-module.
;; See http://blog.racket-lang.org/2012/06/submodules.html

;this tests should never fail; these are tests for MiniScheme+
(module+ test
  (test (run '{+ 1 1}) 2)

  (test (run '{{fun {x y z} {+ x y z}} 1 2 3}) 6)

  (test (run '{< 1 2}) #t)

  (test (run '{local {{define x 1}}
                x}) 1)

  (test (run '{local {{define x 2}
                      {define y {local {{define x 1}} x}}}
                {+ x x}}) 4)

  ;; datatypes
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {List? {Empty}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Empty}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {List? {Cons 1 2}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Cons? {Cons 1 2}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Cons 1 2}}})
        #f)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Empty}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Cons? {Empty}}})
        #f)

  ;; match
  (test (run '{match 1 {case 1 => 2}}) 2)

  (test (run '{match 2
                {case 1 => 2}
                {case 2 => 3}})
        3)

  (test (run '{match #t {case #t => 2}}) 2)

  (test (run '{match #f
                {case #t => 2}
                {case #f => 3}})
        3)

  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {Succ? {pred {Succ {Succ {Zero}}}}}})
        #t)
  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {Succ? {pred {Succ {Succ {Zero}}}}}}) #t))

;;TEST de funciones definidas

;;parse-list-tests
(test (parse-list '{}) (app (id 'Empty) '()))
(test (parse-list '{1 2 3}) (app
                             (id 'Cons)
                             (list
                              (num 1)
                              (app
                               (id 'Cons)
                               (list (num 2) (app (id 'Cons) (list (num 3) (app (id 'Empty) '()))))))))


;; parse-pattern-list tests
(test (parse-pattern-list '{}) (constrP 'Empty '()))
(test (parse-pattern-list '{a {list} b}) (constrP
                                          'Cons
                                          (list
                                           (idP 'a)
                                           (constrP
                                            'Cons
                                            (list
                                             (constrP 'Empty '())
                                             (constrP 'Cons (list (idP 'b) (constrP 'Empty '()))))))) )
(test (parse-pattern-list '{1 d 3}) (constrP
                                     'Cons
                                     (list
                                      (litP (num 1))
                                      (constrP
                                       'Cons
                                       (list
                                        (idP 'd)
                                        (constrP
                                         'Cons
                                         (list (litP (num 3)) (constrP 'Empty '())))))))) 

;;var-to-string tests
(test (var-to-string 'hola) "hola")
(test (var-to-string 1) "1")

;pretty-printing tests
(test (pretty-printing (structV 'Nat 'Zero empty)) "{Zero}")
(test (pretty-printing (structV 'Nat 'Succ (list (structV 'Nat 'Zero empty)))) "{Succ {Zero}}")
(test (run '{local {{datatype Nat
                  {Zero}
                  {Succ n}}
                {define pred {fun {n}
                               {match n
                                 {case {Zero} => {Zero}}
                                 {case {Succ m} => m}}}}}
          {pred {Succ {Succ {Zero}}}}}) "{Succ {Zero}}")

;;pretty-list tests
(test (pretty-list (structV 'List 'Empty '()) "") "{list}")
(test (pretty-list (structV
                    'List
                    'Cons
                    (list
                     1
                     (structV 'List 'Cons (list 2 (structV 'List 'Empty '()))))) "") "{list 1 2}")



;tests for extended MiniScheme+
#;(module+ sanity-tests
    (test (run '{local {{datatype Nat
                  {Zero}
                  {Succ n}}
                {define pred {fun {n}
                               {match n
                                 {case {Zero} => {Zero}}
                                 {case {Succ m} => m}}}}}
          {pred {Succ {Succ {Zero}}}}}) "{Succ {Zero}}")

(test (run
 `{local ,stream-lib
          {local {,ones ,stream-take}
            {stream-take 11 ones}}}) "{list 1 1 1 1 1 1 1 1 1 1 1}")

(test (run `{local ,stream-lib
          {local {,stream-zipWith ,fibs}
            {stream-take 10 fibs}}}) "{list 1 1 2 3 5 8 13 21 34 55}")

(test (run `{local ,stream-lib
          {local {,ones ,stream-zipWith}
            {stream-take 10
                         {stream-zipWith
                          {fun {n m}
                               {+ n m}}
                          ones
                          ones}}}})  "{list 2 2 2 2 2 2 2 2 2 2}")
(test
(run `{local ,stream-lib
               {local {,stream-take ,merge-sort ,fibs ,stream-zipWith}
                 {stream-take 10 {merge-sort fibs fibs}}}})   "{list 1 1 1 1 2 2 3 3 5 5}"))

