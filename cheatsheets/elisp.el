;;
;; elisp script
;;

;; Treat these operators as if there were tax on their use:
;; set setq setf psetf psetq incf decf push pop pushnew rplaca rplacd rotatef shiftf
;; remf remprop remhash let*

;; Print to stdout
(princ "Hello emacs\n")
;; Assign variable
(setq x 10)

;; Math
(+ 1 2 3)
(- 5 4 3)
(* 10 10 10)
(/ 64 8 2)
(% 128 2)
(expt 3 3) ;=> 27
(lsh 7 3) ;; 7 << 3
(ash -32 -4) ;; -32 >> 4 => -2
(lsh 32 -4) ;; 32 >>> 4 => 2
(max 1 2 3 19 -2 5) ;=> 19
(min 1 2 3 19 -2 4) ;=> -2

;; Trig too
(sin pi)
(cos pi)
(tan (/ 2.0 3.0))
;; Also asin, acos, and atan

;; (in)Equality operators
(< 2 3) ;=> t
(> 2 3) ;=> nil
(<= 2 2) ;=> t
(>= 2 3) ;=> nil
(= 1 1) ;=> t
(/= 1 1) ;=> nil
(eq 'foo 2) ;; value equality. use for ints, symbols, interned strings, and object references
(neq 19 19) ;; value inequality. equivalent to (not (eq a b))
(eql 2.0 2.0) ;; for floating point. '=' works too
;; Deep equality, like Java's Object.equals(). use for lists, vectors, strings, and misc.
(equal '(1 2 3) (list 1 2 (+ 2 1)))

;; Bitwise operators
(logand 1 3) ;=> 1
(logior 1 3) ;=> 3
(lognot #b1001) ;=> -10 (why?)
(logxor 1 3) ;=> 2

;; Boolean operators
(and t t t nil) ;=> nil
(or nil nil nil t) ;=> t
(not 3) ;=> nil
(if t a b) ;; t ? a : b

;; Type-of operator
(type-of 13)
;; Type predicates
(null '())
(symbolp :foo)
(atom 5)
(consp '(1 2))
(listp '(1 2))
(numberp 10)
(characterp ?a)
(stringp "shoes")

;; Booleans
(if
  (or t nil)
    (princ "Only nil and '() are falsy"))

;; Strings
(princ "double quotes \"only\"")
(princ ?a) ;; This is a character (ascii)
(princ "need to escape these:" ?\( ?\) ?\\)
;; Do M-x apropos RET \bstring\b RET to see a list of functions related to strings.
(concat "foo" "bar") ;=> "foobar"
(string= "apple" "orange") ;=> nil, could also have used equal
(substring "hacker" 0 4) ;=> "hack"
(upcase "billy mays") ;=> "BILLY MAYS"

;; Numbers
;; most-positive-fixnum and most-negative-fixnum are what you'd expect
(+ 10 4)
(+ #b1010 #b100)
(+ #012 #04)
(+ #xa #x4)
(+ 10.0 4.0)
(+ 1e1 4e0)

;; Arrays are fixed size and are called vectors
[1 2 3]
["No" "Sir" "I" "am" "a" "real" "horse"]
[?b 4 'u "ask" ["this" "is" 'ok] 2 "do"]
;; Element access: aref/set
(aref [2 4 6] 1) ;=> 4

;; Lists
'(1 2 3 4)
(quote (1 2 3 4)) ;; same thing
(list 1 2 3 4) ;; same thing
;; tick-tock
`(1 2 ,(+ 1 2) 4)

;; alist
'((apple . "red")
  (banana . "yellow")
  (orange . "orange"))

;; Statement blocks
(progn
  (do-a)
  (do-b)
  (do-c))
;; let, while, and save-excursion also have implicit statement blocks.
;; a, b, and z are initialized to nil
(let (a b (c "See") (x 0) (y 1) z)
  (do-stuff x y))
;; You need to use let* to eval variables in order, or chain them, like this:
(let* ((a 0) (b (+ a 1)))
  (do-stuff a b))

;; Control flow
(if t (princ "statement was t"))
(if t
    (progn
      (princ "do ")
      (princ "lots ")
      (princ "of stuff\n")))
(if nil (princ "progn not needed for else clause")
      (princ "do ")
      (princ "other ")
      (princ "stuff\n"))
(when t
  (princ "or you can ")
  (princ "use a when instead\n"))
(unless (= 1 2)
  (princ "opposite of when.\n")
  (princ "also math is not broken\n"))
;; cond: the big guy
(cond
 (t
  (do-stuff 1))
 (t
  (do-other-stuff 2))
 (t (default-stuff 3)))
;; case: like cond, but only for numbers or symbols
(case x
  (1 "single")
  (2 "double")
  (t "something else"))

;; Properties
(setf (get 'foo :prop) 10)
(get 'foo :prop) ;=> 10

;; Functions
(defun foo (x) (* x x))
(foo 4) ;=> 16
