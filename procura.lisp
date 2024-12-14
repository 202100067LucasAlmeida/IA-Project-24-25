#|
#   O ficheiro procura.lisp é designado para o desenvolvimento dos
#   algorítmos de procura BFS, DFS, A*.
#
#   ===== PROGRAMADORES =====
#   Lucas Alexandre S. F. de Almeida - 202100067
#   João Pedro M. Morais - 202001541
#
#   ========= DOCENTE =========
#   Prof. Joaquim Filipe
#
|#

(defun bfs (open-list closed-list goal-state successors)
  (cond   
    ((null open-list) nil)
    ((equal (car open-list) goal-state) (car open-list))
    (t
     (let ((current-state (car open-list))
           (next-states (funcall successors current-state)))
       (bfs (append (remove-if (lambda (x) (member x closed-list :test #'equal)) next-states)
                    (cdr open-list))
            (cons current-state closed-list)
            goal-state
            successors)))))
