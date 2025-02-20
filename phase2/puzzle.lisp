#|
#   O ficheiro puzzle.lisp � designado para o desenvolvimento
#   da resolu��o do problema, defini��o de operadores e heur�sticas.
#
#   ===== PROGRAMADORES =====
#   Lucas Alexandre S. F. de Almeida - 202100067
#   Jo�o Pedro M. Morais - 202001541
#
#   ========= DOCENTE =========
#   Prof. Joaquim Filipe
#
|#

(defun print-tabuleiro (tabuleiro)
  (format t "First List: ~a~%Second List: ~a~%" (first tabuleiro) (second tabuleiro))
)

(defun tabuleiro-vazio () 
  (list '(0 0 0 0 0 0) '(0 0 0 0 0 0))
)

(defun tabuleiro-teste () 
  (list '(1 2 3 4 5 6) '(6 5 4 3 2 1))
)

(defun linha (indice tabuleiro)
  (nth indice tabuleiro))

(defun celula (linha-indice coluna-indice tabuleiro)
  (nth coluna-indice (linha linha-indice tabuleiro)))

(defun tabuleiro-vaziop (tabuleiro player)
  (cond ((= player 1) (every #'(lambda (celula) (zerop celula)) (second tabuleiro)))
	(t (every #'(lambda (celula) (zerop celula)) (first tabuleiro)))))

(defun substituir-posicao (indice lista &optional (valor 0))
  (append (subseq lista 0 indice) (list valor) (nthcdr (1+ indice) lista)))

(defun substituir (linha-indice coluna-indice tabuleiro &optional (valor 0))
  (substituir-posicao linha-indice tabuleiro
                      (substituir-posicao coluna-indice (linha linha-indice tabuleiro) valor)))

(defun incrementar-posicao (linha-indice coluna-indice tabuleiro)
  (let ((valor (1+ (celula linha-indice coluna-indice tabuleiro))))
    (substituir linha-indice coluna-indice tabuleiro valor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun proxima-posicao (linha coluna)
  "Calcula a pr�xima posi��o no sentido anti-hor�rio, ignorando a casa inicial ao completar uma volta."
  (cond
    ((and (= linha 0) (> coluna 0)) (list linha (1- coluna)))
    ((and (= linha 0) (= coluna 0)) (list 1 coluna))
    ((and (= linha 1) (< coluna 5)) (list linha (1+ coluna)))
    ((and (= linha 1) (= coluna 5)) (list 0 5))))

(defun distribuir-pecas(pecas linha coluna &optional (tabuleiro (tabuleiro-vazio)))
  (cond ((= pecas 0) '())
	(t (let ((pr (proxima-posicao linha coluna)))
	     (cons pr (distribuir-pecas (1- pecas) (first pr) (second pr) tabuleiro))))))

(defun atualizar-tabuleiro (linha coluna valor tabuleiro)
  "Atualiza o tabuleiro na posi��o especificada com o valor dado."
  (let ((linha-atualizada
         (replace (nth linha tabuleiro) (list valor) :start1 coluna :end1 (1+ coluna))))
    (replace tabuleiro (list linha-atualizada) :start1 linha :end1 (1+ linha))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun operador(linha coluna tabuleiro)
  (let* ((pecas (celula linha coluna tabuleiro))
	 (coordenadas (distribuir-pecas pecas linha coluna tabuleiro)))
    (when (null coordenadas) (return-from operador tabuleiro))
    (atualizar-tabuleiro linha coluna 0 tabuleiro)
    (mapcar #'(lambda(c)
		(let ((x (first c))
		      (y (second c)))
		  (atualizar-tabuleiro x y (1+ (celula x y tabuleiro)) tabuleiro)))
	    coordenadas)
    (let* ((ultima (first (last coordenadas)))
	   (n (celula (first ultima) (second ultima) tabuleiro)))
      (when (or (= n 1) (= n 3) (= n 5))
	(atualizar-tabuleiro (first ultima) (second ultima) 0 tabuleiro))))
  tabuleiro)

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
