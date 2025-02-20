#|
#   O ficheiro procura.lisp � designado para o desenvolvimento dos
#   algor�tmos de procura BFS, DFS, A*.
#
#   ===== PROGRAMADORES =====
#   Lucas Alexandre S. F. de Almeida - 202100067
#   Jo�o Pedro M. Morais - 202001541
#
#   ========= DOCENTE =========
#   Prof. Joaquim Filipe
#
|#


(defun deep-copy (lst)
  "Returns a copy of the list"
  (mapcar #'(lambda (x)
	    (if (listp x) (deep-copy x) x))
	  lst))

(defun sexo(tabuleiro player)
  "Reproduz-se como macho que este projeto deve ser"
  (let ((list (if (= player 1) '((1 0) (1 1) (1 2) (1 3) (1 4) (1 5)) '((0 0) (0 1) (0 2) (0 3) (0 4) (0 5)))))
    (remove nil (mapcar #'(lambda(coordenadas)
			    (operador (first coordenadas) (second coordenadas) (deep-copy tabuleiro)))
			list))))

(defun get-board(node)
  (first node))

(defun create-node(board parent)
    (list board parent))

(defun heuristic(board player)
  (let ((line1 (reduce #'+ (first board)))
	(line2 (reduce #'+ (second board))))
    (cond ((= player 1) (- line2 line1))
	  (t (- line1 line2)))))

(defun sexo-com-protecao(node player)
  "Reproduz-se usando protecao para prevenir acidentes"
  (let ((normal-children (sexo (get-board node) player)))
    (mapcar #'(lambda(c) (create-node c node)) normal-children)))

(defun minimax (node depth alpha beta maximizing-player player)
  "Minimax with Alpha-Beta Pruning. Returns the best move and its score."
  (if (or (= depth 0) (tabuleiro-vaziop (get-board node) player))
      (values node (heuristic (get-board node) player))
    (let ((best-move nil)
          (best-score (if maximizing-player -1000000 1000000)))
      (dolist (child (sexo-com-protecao node player))
        (multiple-value-bind (move score)
            (minimax child (1- depth) alpha beta (not maximizing-player) (if (= player 1) 2 1))
          (if maximizing-player
              (when (> score best-score)
                (setf best-score score)
                (setf best-move child)
                (setf alpha (max alpha best-score)))
              (when (< score best-score)
                (setf best-score score)
                (setf best-move child)
                (setf beta (min beta best-score))))
          (when (>= alpha beta) (return))))
      (values best-move best-score))))
