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

(defun sexo(tabuleiro)
  "Reproduz-se como macho que este projeto deve ser"
  (remove nil (mapcar #'(lambda(coordenadas)
			  (operador (first coordenadas) (second coordenadas) (deep-copy tabuleiro)))
		      '((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (1 0) (1 1) (1 2) (1 3) (1 4) (1 5)))))

(defun validate-children(children)
  "Validates if any of the children is the solution. Returns the child that is solution or nil"
  (cond ((null children) nil)
        ((tabuleiro-vaziop (first children)) (first children))
        (t (validate-children (rest children)))))

(defun check-element-in-list(board list)
  "Checks if the element is already in list"
  (some #'(lambda(b)
            (equal board b))
        list))

(defun algorithm(tabuleiro fn-open-list)
  "Performs the algorithm for bfs and dfs"
  (if (tabuleiro-vaziop tabuleiro) (return-from algorithm tabuleiro))
  (let ((list-open (list tabuleiro))
	(closed '())
	(counter 0))
    (labels
	((recursive(open)
	   (print-tabuleiro (first open))
;;	   (when (< counter 3) (format t "Open: ~a~%" open) (format t "Node: ~a~%" (first open)) (incf counter))
;;	   (when (= counter 3) (return-from algorithm "1"))
	   (if (null open) (return-from recursive nil))
	   (let* ((node (first open))
		  (children (sexo node))
		  (solution (validate-children children)))
	     (format t "Children:~%")
	     (mapcar #'(lambda(c) (print-tabuleiro c)) children)
	     (format t "~a~%" open)
	     (format t "---------------------------------------------------~%")
	     (when (not (null solution)) (return-from recursive solution))
	     ;; Use Memoization to check if any child is in list
	     (mapcar #'(lambda(c)
			 (when (and (not (check-element-in-list c open)) (not (check-element-in-list c closed))) (setf open (funcall fn-open-list c open))))
		     children)
	     (when (not (check-element-in-list node closed)) (push node closed))
	     (recursive (rest open)))))
      (recursive list-open))))

(defun bfs(tabuleiro)
  "Breath First Search Algorithm"
  (algorithm tabuleiro #'(lambda(child open) (append open (list child)))))

(defun dfs(tabuleiro)
  "Depth First Search Algorithm"
  (algorithm tabuleiro #'(lambda(child open) (append (list child) open))))
