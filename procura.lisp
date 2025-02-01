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


(defun algorithm(tabuleiro fn-list-open)
  "Performs the algorithm for bfs and dfs"
  (if (tabuleiro-vaziop tabuleiro) (return-from algorithm tabuleiro))
  (let ((list-open '(tabuleiro))
	(list-closed '())) ;; Closed are not being used yet
    (labels
	((recursive(open closed)
	   (if (null open) return-from recursive nil)
	   (let* ((node (first open))
		  (children (sexo node)))
	     ;; Validate if any child is solution
	     ;; if it is return it
	     (recursive (fn-list-open (rest open) children) closed)))))
    (recursive list-open list-closed)))
    

(defun bfs(tabuleiro)
  "Breath First Search Algorithm"
  (algorithm tabuleiro #'(lambda(open chidlren)
			   (append open children))))

(defun nome_bacano (abertos)
  (cond
   ((= (length abertos) 0) NIL)
   (t (let* ((no (first abertos))
             (sucessores (mapcar #'(lambda (coordenadas) 
(operador (first coordenadas) (second coordenadas) tabuleiro))
'((0 0) (0 1) (0 2)(0 3) (0 4) (0 5)(1 0) (1 1) (1 2)(1 3) (1 4) (1 5))))
             ; valida-se
             (validar-nos sucessores)
             ; recursivo
             (nome_bacano (cdr abertos))
             )
    )))
)

(defun validar-nos (sucessores)
  (cond ((null sucessores) nil)
        (defun validate-childs (childs)
  "Validate if any child is the solution."
  (cond ((null childs) nil)
    ((tabuleiro-vaziop (first childs)) (first childs))
    (t (validate-childs (rest childs))))) 
  )
)
