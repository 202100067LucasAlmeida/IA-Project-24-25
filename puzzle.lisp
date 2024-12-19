#|
#   O ficheiro puzzle.lisp é designado para o desenvolvimento
#   da resolução do problema, definição de operadores e heurísticas.
#
#   ===== PROGRAMADORES =====
#   Lucas Alexandre S. F. de Almeida - 202100067
#   João Pedro M. Morais - 202001541
#
#   ========= DOCENTE =========
#   Prof. Joaquim Filipe
#
|#

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

(defun tabuleiro-vaziop (tabuleiro)
  (every #'(lambda (linha) (every #'(lambda (celula) (zerop celula)) linha)) tabuleiro))

(defun substituir-posicao (indice lista &optional (valor 0))
  (append (subseq lista 0 indice) (list valor) (nthcdr (1+ indice) lista)))

(defun substituir (linha-indice coluna-indice tabuleiro &optional (valor 0))
  (substituir-posicao linha-indice tabuleiro
                      (substituir-posicao coluna-indice (linha linha-indice tabuleiro) valor)))

(defun incrementar-posicao (linha-indice coluna-indice tabuleiro)
  (let ((valor (1+ (celula linha-indice coluna-indice tabuleiro))))
    (substituir linha-indice coluna-indice tabuleiro valor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun distribuir-pecas (pecas linha coluna &optional (tabuleiro (tabuleiro-vazio)))
  "Distribui o numero de pecas pelo tabuleiro em sentido anti-horário."
  (cond
    ;; Caso base: Se o número de peças é zero, verifica se a casa final precisa ser ajustada
    ((zerop pecas)
     (let* ((casa (nth coluna (nth linha tabuleiro)))
            (novo-tabuleiro
             (if (member casa '(1 3 5))
                 (atualizar-tabuleiro linha coluna 0 tabuleiro)
                 tabuleiro)))
       novo-tabuleiro))

    ;; Caso recursivo: distribui uma peça e chama recursivamente
    (t (let* ((zerar-inicial (= pecas (reduce #'+ (mapcar #'length tabuleiro))))
              (atualizado-tabuleiro (if zerar-inicial
                                        (atualizar-tabuleiro linha coluna 0 tabuleiro)
                                        (let* ((prox (proxima-posicao linha coluna tabuleiro))
                                               (prox-linha (car prox))
                                               (prox-coluna (cadr prox)))
                                          (incrementar-posicao prox-linha prox-coluna tabuleiro)))))
         (distribuir-pecas (1- pecas) (car (proxima-posicao linha coluna atualizado-tabuleiro)) (cadr (proxima-posicao linha coluna atualizado-tabuleiro)) atualizado-tabuleiro)))))

(defun proxima-posicao (linha coluna tabuleiro)
  "Calcula a próxima posição no sentido anti-horário, ignorando a casa inicial ao completar uma volta."
  (cond
    ((and (= linha 0) (> coluna 0)) (list linha (1- coluna)))
    ((and (= linha 0) (= coluna 0)) (list 1 coluna))
    ((and (= linha 1) (< coluna 5)) (list linha (1+ coluna)))
    ((and (= linha 1) (= coluna 5)) (list 0 5))
    ((and (= linha 0) (= coluna 5)) (list linha (1- coluna)))))

(defun atualizar-tabuleiro (linha coluna valor tabuleiro)
  "Atualiza o tabuleiro na posição especificada com o valor dado."
  (let ((linha-atualizada
         (replace (nth linha tabuleiro) (list valor) :start1 coluna :end1 (1+ coluna))))
    (replace tabuleiro (list linha-atualizada) :start1 linha :end1 (1+ linha))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun operador (linha-indice coluna-indice tabuleiro)
  (distribuir-pecas (celula linha-indice coluna-indice tabuleiro) linha-indice coluna-indice tabuleiro)
)

