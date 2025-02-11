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


(defun bfs (tabuleiro-inicial)
  (let ((abertos (con '() tabuleiro-inicial)))
    
  )
)

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