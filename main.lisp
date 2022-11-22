(load "matrix.lisp")
(load "reader.lisp")
(load "solver.lisp")

;; Função principal
(defun main()
    (print "Insira o documento que contém o tabuleiro (ex.: Tabuleiros/tabuleiro10x10.txt):")
    (setq path (read))
    (setq grid (solve-and-print path))
    (format t "~{~a~^~%~}" grid)
)

(main)