#|
#   O ficheiro projeto.lisp � designado para o carregamento de outros ficheiros
#   de c�digo, escrita e leitura de ficheiros, e o desenvolvimento da intera��o
#   com o utilizador.
#
#   ===== PROGRAMADORES =====
#   Lucas Alexandre S. F. de Almeida - 202100067
#   Jo�o Pedro M. Morais - 202001541
#
#   ========= DOCENTE =========
#   Prof. Joaquim Filipe
#
|#

(load (compile-file "C:/Users/jpmin/Desktop/IA/IA-Project-24-25/phase2/puzzle.lisp"))
(load (compile-file "C:/Users/jpmin/Desktop/IA/IA-Project-24-25/phase2/procura.lisp"))

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

(defun playervsbot (board)
  "Player 1 (human) vs Bot (Player 2)"
  (format t "Game Start!~%")
  (loop
    (print-tabuleiro board)
    (format t "Player 1, choose a column (1-6): ")
    (let ((col (get-valid-number 6)))
      (setf board (operador 1 (1- col) (deep-copy board))))
    (if (tabuleiro-vaziop board 1)
        (return (format t "Player 1 wins!~%")))
    (print-tabuleiro board)
    (format t "~%Bot (Player 2) is thinking...~%")
    (let ((result (minimax (create-node board '()) 7 -1000000 1000000 t 2)))
      (setf board (get-board result)))
    (if (tabuleiro-vaziop board 2)
        (return (format t "Bot (Player 2) wins!~%")))))

(defun botvsbot(board &optional (player 1) (counter 1))
  (format t "Player: ~a~%" player)
  (print-tabuleiro board)
  (let ((result (minimax (create-node board '()) 7 -1000000 1000000 t player)))
    (cond ((tabuleiro-vaziop (get-board result) player)
	   (format t "Vencedor: ~a~%" player)
	   (return-from botvsbot result))
	  (t (botvsbot (get-board result) (if (= player 1) 2 1) (1+ counter))))))

(defun main(&optional (board (tabuleiro-teste)))
  (format t "Choose the algorithm to run:~%1: Player vs Bot~%2: Bot vs Bot~%")
  (let ((choice (get-valid-number 2)))
    (cond ((= choice 1) (playervsbot board))
	  (t (botvsbot board)))))