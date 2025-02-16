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

(defun remove-nils (lst)
  "Returns a lista without any nil"
  (cond ((null lst) '())
        ((null (first lst)) (remove-nils (rest lst)))
        (t (cons (first lst) (remove-nils (rest lst))))))

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

(defun algorithm(tabuleiro fn-list-open)
  "Performs the algorithm for bfs and dfs"
  (if (tabuleiro-vaziop tabuleiro) (return-from algorithm tabuleiro))
  (let ((open (cons tabuleiro nil))
	(closed '()))
    (labels
	((recursive()
	   (if (null open) (return-from recursive nil))
	   (let* ((node (pop open))
		      (children (sexo node))
              (solution (validate-children children)))
         (when (not (null solution)) (return-from recursive solution))
         ;; Use Memoization to check if any child is in list
	     (mapcar #'(lambda(c)
                        (when (not (check-element-in-list c open)) (setf open (funcall fn-list-open c open))))
                    children)
	     (when (not (check-element-in-list node closed)) (setf closed (append node closed)))
	     (recursive))))
      (recursive))))
    

(defun bfs(tabuleiro)
  "Breath First Search Algorithm"
  (algorithm tabuleiro #'(lambda(child open)
			   (append child open))))
