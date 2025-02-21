#|
#   O ficheiro projeto.lisp ï¿½ designado para o carregamento de outros ficheiros
#   de cï¿½digo, escrita e leitura de ficheiros, e o desenvolvimento da interaï¿½ï¿½o
#   com o utilizador.
#
#   ===== PROGRAMADORES =====
#   Lucas Alexandre S. F. de Almeida - 202100067
#   Joï¿½o Pedro M. Morais - 202001541
#
#   ========= DOCENTE =========
#   Prof. Joaquim Filipe
#
|#

(load (compile-file "C:/Users/jpmin/Desktop/IA/IA-Project-24-25/phase1/puzzle.lisp"))
(load (compile-file "C:/Users/jpmin/Desktop/IA/IA-Project-24-25/phase1/procura.lisp"))

(defun get-number()
  "Ask for input until get a number"
  (format t "Enter a number > ")
  (let ((value (read)))
    (sleep 0.01)
    (format t "~%")
    (if (numberp value) (return-from get-number value)))
  (format t "Invalid input! Please enter a number.")
  (get-number))

(defun get-valid-number (cap)
  (let ((number (get-number)))
    (if (or (<= number 0) (> number cap))
	(get-valid-number cap)
	(return-from get-valid-number number))))

; (defun main()
;   (let ((choice (get-valid-number 3))
;     (boards (read-file-problem))
;   )
;   (print-boards boards)
;   (format t "Choose the board:~%")

;   (format t "Choose the algorithm to run:~%1: BFS~%2: DFS~%3: A*~%")
;     (cond ((= choice 1) (bfs (tabuleiro-teste)))
; 	  ((= choice 2) (dfs (tabuleiro-teste)))
; 	  (t (a* (tabuleiro-teste))))))

(defun main()
  (let ((boards (read-file-problem)))
    (print-boards boards)
    (format t "Choose the board:~%")
    (let ((board (1- (nth (get-valid-number 7)))))
      (format t "Choose the algorithm to run:~%1: BFS~%2: DFS~%3: A*~%")
        (let ((choice (get-valid-number 3)))
          (cond ((= choice 1) (bfs board))
	  ((= choice 2) (dfs board))
	  (t (a* board)))
        )
    )
  )
)

(defun read-file-problems()
  (labels (
      (line-reader (file)
        (let (
            (line (read-line file nil))
          )
          (if (null line)
            nil
            (cons (read-from-string line) (line-reader file))
          )
        )
      )
    )
    (with-open-file (file "C:/Users/jpmin/Desktop/IA/IA-Project-24-25/phase1/problemas.dat" :direction :INPUT :if-does-not-exist :ERROR)
      (line-reader file)
    )
  )
)

(defun print-boards(boards)
(cond ((null boards) nil)
(t (print-tabuleiro (first boards))
(print-boards (rest boards)))))