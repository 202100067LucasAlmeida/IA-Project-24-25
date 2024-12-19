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

(defun distribuir-pecas (pecas linha-indice coluna-indice &optional (tabuleiro (tabuleiro-vazio)))
  (cond
    ((zerop pecas) '())
    (t (let* ((dimensoes (list 2 6))
              (proxima-coluna (mod (1+ coluna-indice) (second dimensoes)))
              (proxima-linha (cond
                               ((zerop proxima-coluna) (mod (1+ linha-indice) (first dimensoes)))
                               (t linha-indice))))
         (cons (list linha-indice coluna-indice)
               (distribuir-pecas (1- pecas) proxima-linha proxima-coluna tabuleiro))))))

(defun operador (linha-indice coluna-indice tabuleiro)
  (let* ((pecas (celula linha-indice coluna-indice tabuleiro))
         (tabuleiro-atualizado (substituir linha-indice coluna-indice tabuleiro 0))
         (posicoes-a-incrementar (distribuir-pecas pecas linha-indice coluna-indice)))
    (cond
      ((null posicoes-a-incrementar) tabuleiro-atualizado)
      (t (let* ((posicao (car posicoes-a-incrementar))
                (nova-linha (first posicao))
                (nova-coluna (second posicao))
                (tabuleiro-incrementado (incrementar-posicao nova-linha nova-coluna tabuleiro-atualizado))
                (pecas-na-posicao (celula nova-linha nova-coluna tabuleiro-incrementado)))
           (cond
             ((and (= (length posicoes-a-incrementar) 1) (member pecas-na-posicao '(1 3 5)))
              (operador linha-indice coluna-indice (substituir nova-linha nova-coluna tabuleiro-incrementado 0)))
(t (operador linha-indice coluna-indice (substituir nova-linha nova-coluna tabuleiro-incrementado)))))))))

