;;; Elif Keleş - 161044033
;;; HW1-Part2: Lexer with lisp

#| 
Terminals:
• Keywords
• Operators
• Value: Any combination of digits with no leading zeros. 0 is considered a value.
• Identifier: Any combination of alphabetical characters and digits with no leading digit. 
|#

(defvar letters (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(defvar digits (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

;; function that will starts interpreter
;; can have zero or one input
;; The input can be a file name which will be loaded by the interpreter and interpreted right away
;; if there is no input file it will read the "grammar.txt"
;; and makes the read line a list
(defun gppinterpreter (&optional filename)

	(with-open-file (out-str "parsed_lisp.txt"
	        :direction :output 
	        :if-exists :rename-and-delete 
	        :if-does-not-exist :create)

		;if no file name is given
		(if (eq nil filename)
			; In REPL mode: empty string terminates the application
			(let ((input "  "))
				;(setq user-input (read-line))
		        (loop while (not (equal input "")) do            
		            (setq input (read-line))
		            (lispLexer input out-str)
		        )
	        )

			;if there is an input file 
			(with-open-file (in filename)
				(loop for line = (read-line in nil)
					while line do					
					;(write (elt line 0))
					(lispLexer line out-str)				
				)
			)
		)
	)
)

; lexer function 
; to tokenize the lines
(defun lispLexer (inputText out-str)

	(let ((quotenumber 0) (findKeyword 0) (otherCharfound 1))	
	
	(loop for x from 0 to (- (length inputText) 1) do

		; update findKeyword value
		(setq findKeyword 0)
		(setq otherCharfound 1)

		; if the character is a space or tab do nothing
		(if (or (eq (elt inputText x) #\tab) (eq (elt inputText x) #\space) (eq (elt inputText x) #\newline) )
			(setq otherCharfound 0)
		)


		; check characters for identifiers

		; and
		(if (and (eq (elt inputText x) #\a) (eq (elt inputText (+ 1 x)) #\n) (eq (elt inputText (+ 2 x)) #\d) )
			(progn
				(format out-str "KW_AND~%")
				(if (< (+ 2 x) (- (length inputText) 1))	(setq x (+ 2 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; or
		(if (and (eq (elt inputText x) #\o) (eq (elt inputText (+ 1 x)) #\r) )
			(progn
				(format out-str "KW_OR~%")
				(if (< (+ 1 x) (- (length inputText) 1))	(setq x (+ 1 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; not
		(if (and (eq (elt inputText x) #\n) (eq (elt inputText (+ 1 x)) #\o) (eq (elt inputText (+ 2 x)) #\t) )
			(progn	
				(format out-str "KW_NOT~%")
				(if (< (+ 2 x) (- (length inputText) 1))	(setq x (+ 2 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; equal
		(if (and (eq (elt inputText x) #\e) (eq (elt inputText (+ 1 x)) #\q) (eq (elt inputText (+ 2 x)) #\u) (eq (elt inputText (+ 3 x)) #\a) (eq (elt inputText (+ 4 x)) #\l))
			(progn
				(format out-str "KW_EQUAL~%")
				(if (< (+ 4 x) (- (length inputText) 1))	(setq x (+ 4 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; less
		(if (and (eq (elt inputText x) #\l) (eq (elt inputText (+ 1 x)) #\e) (eq (elt inputText (+ 2 x)) #\s) (eq (elt inputText (+ 3 x)) #\s) )
			(progn
				(format out-str "KW_LESS~%")
				(if (< (+ 3 x) (- (length inputText) 1))	(setq x (+ 3 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; nil
		(if (and (eq (elt inputText x) #\n) (eq (elt inputText (+ 1 x)) #\i) (eq (elt inputText (+ 2 x)) #\l) )
			(progn
				(format out-str "KW_NIL~%")
				(if (< (+ 2 x) (- (length inputText) 1))	(setq x (+ 2 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; list
		(if (and (eq (elt inputText x) #\l) (eq (elt inputText (+ 1 x)) #\i) (eq (elt inputText (+ 2 x)) #\s) (eq (elt inputText (+ 3 x)) #\t) )
			(progn
				(format out-str "KW_LIST~%")
				(if (< (+ 3 x) (- (length inputText) 1))	(setq x (+ 3 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; append
		(if (and (eq (elt inputText x) #\a) (eq (elt inputText (+ 1 x)) #\p) (eq (elt inputText (+ 2 x)) #\p) (eq (elt inputText (+ 3 x)) #\e) (eq (elt inputText (+ 4 x)) #\n) (eq (elt inputText (+ 5 x)) #\d) )
			(progn
				(format out-str "KW_APPEND~%")
				(if (< (+ 5 x) (- (length inputText) 1))	(setq x (+ 5 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; concat
		(if (and (eq (elt inputText x) #\c) (eq (elt inputText (+ 1 x)) #\o) (eq (elt inputText (+ 2 x)) #\n) (eq (elt inputText (+ 3 x)) #\c) (eq (elt inputText (+ 4 x)) #\a) (eq (elt inputText (+ 5 x)) #\t) )
			(progn	
				(format out-str "KW_CONCAT~%")
				(if (< (+ 5 x) (- (length inputText) 1))	(setq x (+ 5 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; set
		(if (and (eq (elt inputText x) #\s) (eq (elt inputText (+ 1 x)) #\e) (eq (elt inputText (+ 2 x)) #\t) )
			(progn	
				(format out-str "KW_SET~%")
				(if (< (+ 2 x) (- (length inputText) 1))	(setq x (+ 2 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; deffun
		(if (and (eq (elt inputText x) #\d) (eq (elt inputText (+ 1 x)) #\e) (eq (elt inputText (+ 2 x)) #\f) (eq (elt inputText (+ 3 x)) #\f) (eq (elt inputText (+ 4 x)) #\u) (eq (elt inputText (+ 5 x)) #\n) )
			(progn
				(format out-str "KW_DEFFUN~%")
				(if (< (+ 5 x) (- (length inputText) 1))	(setq x (+ 5 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; for
		(if (and (eq (elt inputText x) #\f) (eq (elt inputText (+ 1 x)) #\o) (eq (elt inputText (+ 2 x)) #\r) )
			(progn
				(format out-str "KW_FOR~%")
				(if (< (+ 2 x) (- (length inputText) 1))	(setq x (+ 2 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; if
		(if (and (eq (elt inputText x) #\i) (eq (elt inputText (+ 1 x)) #\f) )
			(progn
				(format out-str "KW_IF~%")
				(if (< (+ 1 x) (- (length inputText) 1))	(setq x (+ 1 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; exit
		(if (and (eq (elt inputText x) #\e) (eq (elt inputText (+ 1 x)) #\x) (eq (elt inputText (+ 2 x)) #\i) (eq (elt inputText (+ 3 x)) #\t) )
			(progn
				(format out-str "KW_EXIT~%")
				(if (< (+ 3 x) (- (length inputText) 1))	(setq x (+ 3 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; load
		(if (and (eq (elt inputText x) #\l) (eq (elt inputText (+ 1 x)) #\o) (eq (elt inputText (+ 2 x)) #\a) (eq (elt inputText (+ 3 x)) #\d) )
			(progn
				(format out-str "KW_LOAD~%")
				(if (< (+ 3 x) (- (length inputText) 1))	(setq x (+ 3 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; disp
		(if (and (eq (elt inputText x) #\d) (eq (elt inputText (+ 1 x)) #\i) (eq (elt inputText (+ 2 x)) #\s) (eq (elt inputText (+ 3 x)) #\p) )
			(progn
				(format out-str "KW_DISP~%")
				(if (< (+ 3 x) (- (length inputText) 1))	(setq x (+ 3 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; true
		(if (and (eq (elt inputText x) #\t) (eq (elt inputText (+ 1 x)) #\r) (eq (elt inputText (+ 2 x)) #\u) (eq (elt inputText (+ 3 x)) #\e) )
			(progn
				(format out-str "KW_TRUE~%")
				(if (< (+ 3 x) (- (length inputText) 1))	(setq x (+ 3 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)
		; false
		(if (and (eq (elt inputText x) #\f) (eq (elt inputText (+ 1 x)) #\a) (eq (elt inputText (+ 2 x)) #\l) (eq (elt inputText (+ 3 x)) #\s) (eq (elt inputText (+ 4 x)) #\e) )
			(progn
				(format out-str "KW_FALSE~%")
				(if (< (+ 4 x) (- (length inputText) 1))	(setq x (+ 4 x))(return-from lispLexer))
				(setq findKeyword 1)
				(setq otherCharfound 0)
			)
		)

		; check characters for operators

		; +
		(if (eq (elt inputText x) #\+) 
			(progn
				(setq otherCharfound 0)
				(format out-str "OP_PLUS~%")
			)
		)
		; -
		(if (eq (elt inputText x) #\-)
			(progn
				(setq otherCharfound 0)
				(format out-str "OP_MINUS~%")
			)
		)
		; /
		(if (eq (elt inputText x) #\/)
			(progn
				(setq otherCharfound 0)
				(format out-str "OP_DIV~%")
			)
		)
		; *
		(if (and (eq (elt inputText x) #\*) (not (eq (elt inputText (+ 1 x)) #\* )))
			(progn
				(setq otherCharfound 0)
				(format out-str "OP_MULT~%")
			)
		)
		; (
		(if (eq (elt inputText x) #\( )
			(progn
				(setq otherCharfound 0)
				(format out-str "OP_OP~%")
			)
		)
		; )
		(if (eq (elt inputText x) #\) )
			(progn
				(setq otherCharfound 0)
				(format out-str "OP_CP~%")
			)
		)
		; **
		(if (and (eq (elt inputText x) #\*) (eq (elt inputText (+ 1 x)) #\* ) )
			(progn
				(format out-str "OP_DBLMULT~%")
				(setq otherCharfound 0)
				(if (< (+ 1 x) (- (length inputText) 1))	(setq x (+ 1 x))(return-from lispLexer))
			)
		)
		; "
		(if (eq (elt inputText x) #\" )
			(if (eq quotenumber 0)
				(progn
					(format out-str "OP_OC~%")
					(setq quotenumber 1)
					(setq otherCharfound 0)
				)
				(progn
					(format out-str "OP_CC~%")
					(setq quotenumber 0)
					(setq otherCharfound 0)
				)	
			)	
		)
		; ,
		(if (eq (elt inputText x) #\,)
			(progn
				(format out-str "OP_COMMA~%")
				(setq otherCharfound 0)
				(if (< (+ 1 x) (- (length inputText) 1))	(setq x (+ 1 x))(return-from lispLexer))
			)
		)

		; check characters for comments
		(if (and (eq (elt inputText x) #\;) (eq (elt inputText (+ 1 x)) #\;))
			(progn
				(format out-str "COMMENT~%")
				(loop while (< x (length inputText) ) do
					(if (< (+ 1 x) (- (length inputText) 1))	(setq x (+ 1 x))(return-from lispLexer))
				)
				(setq x (- x 1))
				(setq otherCharfound 0)
			)
		)

		; check characters for digits/ values
		; 0 (no-leading-zero)
		(if (or (and (eq (elt inputText x) #\0) (not (find (elt inputText (+ 1 x)) digits) ))
				(and (eq (elt inputText x) #\0) (not (find (elt inputText (+ 1 x)) letters) )) )
			(progn
				(format out-str "VALUE~%")
				(setq otherCharfound 0)
			)
		)
		; [1-9][0-9]* 
		(if (and (not (eq (elt inputText x) #\0)) (find (elt inputText x) digits)) 
			(progn
				(loop while (find (elt inputText x) digits ) do
					(if (< (+ 1 x) (- (length inputText) 1))	(setq x (+ 1 x))(return-from lispLexer))
				)
				(format out-str "VALUE~%")
				(setq otherCharfound 0)
			)
		)

		; check characters for identifiers
		; [a-zA-Z][a-zA-Z0-9]*
		(if ( and (find (elt inputText x) letters ) (eq findKeyword 0) )
			(progn
				(loop while (or (find (elt inputText x) letters) (find (elt inputText x) digits)) do
					(if (< (+ 1 x) (- (length inputText) 1))	(setq x (+ 1 x))(return-from lispLexer))
				)
				(format out-str "IDENTIFIER~%")
				(setq x (- x 1))
				(setq otherCharfound 0)
			)
		)

		; error case	
		
		; or [0-9][a-zA-Z0-9_]*
		(if (or 
			(and (eq (elt inputText x) #\0) (find (elt inputText (+ 1 x)) digits) )
			(and (eq (elt inputText x) #\0) (find (elt inputText (+ 1 x)) letters) )
			)
			(progn
				(loop while (or (find (elt inputText x) letters) (find (elt inputText x) digits)) do
					(if (< (+ 1 x) (- (length inputText) 1))	(setq x (+ 1 x))(return-from lispLexer))
				)
				(format out-str "ERROR FOR: START WITH A ZERO!~%")
			)
		)
		; general		
		(if (= 1 otherCharfound)
			(format out-str "ERROR FOR: ~A~%" (elt inputText x))
		)


	)
	)
)


; running the program
(if (equal (car *args*) nil)
 		(gppinterpreter)
  		(gppinterpreter (car *args*))
)

