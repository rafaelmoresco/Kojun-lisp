;; Retorna true se a lista passada só possui um elemento
(defun single (list)
    (if (= (length list) 1) T NIL))

;; Remove de lst1 os elementos de lst2
;; Se lst1 eh single, retorna lst1
(defun minus (lst1 lst2)
    (if (single lst1) lst1 (set-difference lst1 lst2)))

;; Função responsável por preencher os valores das células "em branco" com possíveis valores
;; Possiveis valores sao: [1..tamanho do bloco] - [valores preexistentes no bloco] - [valores preexistentes de vizinhos]
(defun fill-values-with-choices (matrix)
    (setq local-matrix (loop for row in matrix collect (mapcar #'copy-structure row)))
    (labels (
        (one-to-n (n) (loop for i from 1 to n collect i))
        (gen-choices (el)
            (if (= (cell-value el) 0)
                ;; then
                (progn (setq choices (one-to-n (length-of-block (cell-block el) matrix)))
                (setq choices (minus choices (vals-of-block (cell-block el) matrix)))
                (minus choices (vals-of-neighbors el matrix)))
                ;; else
                (list (cell-value el)))))
        ;; do
        (loop
            for el in (flatten matrix)
            do (progn 
                (setq local-cell (cell-from-pos (cell-position el) local-matrix))
                (setf (cell-value local-cell) (gen-choices el)))))
    (return-from fill-values-with-choices local-matrix))

;; Retorna true se a matriz passada nunca pode fornecer uma solução.
(defun blocked (matrix)
    (not (safe matrix)))

;; Retorna true se as matrizes passam nos seguintes testes:
;; ;; Cada célula não possui vizinhos com o mesmo valor;
;; ;; Cada bloco não possui valores duplicados;
;; ;; Cada bloco respeita a ordem de valores decrescentes na vertical.
(defun safe (matrix)
    (and
        (notevery #'void (rows matrix))
        (every #'valid-neighborhood (cols matrix))
        (every #'valid-neighborhood (rows matrix))
        (every #'nodups (blocks matrix))
        (every #'is-decreasing (blocks-by-cols matrix))))

;; recebe uma lista de celulas
;; Verifica se não há valores unitarios duplicados na linha passada
(defun nodups (row)
    (labels (
        (is-uniques-list (l)
            (or (null l)
                (and (not (member (car l) (cdr l)))
                (is-uniques-list (cdr l))))))
        ;; do
        (is-uniques-list (flatten
            (loop
                for el in row 
                for v = (cell-value el)
                collect (if (single v) v NIL) into raw
                finally (return (remove NIL raw)))))))

;; Verifica se nao ha valores unitarios iguais entre vizinhos
(defun valid-neighborhood (row)
    (if (<= (length row) 1)
        T
        (progn (setq a (cell-value (car row)))
        (setq b (cell-value (car (cdr row))))
        (if (and (<= (length a) 1) 
                 (<= (length b) 1))
            ;; then
            (if (equal a b) NIL (valid-neighborhood (cdr row)))
            ;; else
            (valid-neighborhood (cdr row))))))

;; Verifica se os valores unitários da linha passada estão em ordem decrescente
(defun is-decreasing (row)
    (if (<= (length row) 1)
        T
        (progn (setq a (cell-value (car row)))
        (setq b (cell-value (car (cdr row))))
        (if (and (<= (length a) 1) 
                 (<= (length b) 1))
            ;; then
            (if (< (car a) (car b)) NIL (is-decreasing (cdr row)))
            ;; else
            (is-decreasing (cdr row))))))

;; Verifica se ha alguma celula vazia na linha passada
(defun void (row)
    (if (member NIL (loop for el in row collect (cell-value el))) T NIL))

;; para a linha passada, retorna os valores unitarios
(defun singles (row)
    (flatten (remove-if-not #'single (loop for el in row collect (cell-value el)))))

;; De uma linha de células, reduz os possíveis valores de cada celula com base em elementos unitarios da linha
;; ex: ["1 2 3 4", "1", "3 4", "3"] -> ["2 4", "1", "4", "3"]
(defun reduce-choices (row)
    (setq s (singles row))
    (loop for el in row do (setf (cell-value el) (minus (cell-value el) s))))

;; Aplica a função reduce-choices para as celulas de cada coluna dividida por blocos.
(defun prune (matrix)
    (loop 
        for block in (blocks-by-cols matrix)
        do (reduce-choices block))
    (return-from prune matrix))

;; Retorna uma lista que contém soluções válidas para o tabuleiro.
;; A ideia desse algoritmo é de que filtre todas as escolhas possíveis, uma célula por vez,
;; e retorne somente matrizes que contém escolhas válidas.
(defun filter-choices (matrix)
    (cond 
        ;; nao retorna nada se a matriz de escolhas passada não pode fornecer uma solução
        ((blocked matrix) (list ))
        ;; se a matriz de escolhas passada é válida e só contém valores unitários, é uma solução,
        ;; portanto, extrai o valor de cada lista de escolhas e retorna.
        ((every #'single (mapcar #'cell-value (flatten matrix))) (return-from filter-choices matrix))
        ;; se a matriz de escolhas passada é valida e contém mais de uma escolha para pelo menos uma célula,
        ;; expande a matriz, reduz o número de escolhas restantes e continua o processo de busca sobre ela.
        (t (loop
                for expanded-matrix in (expand matrix)
                collect (filter-choices (prune expanded-matrix))))))


;; Gera n novas matrizes para as n escolhas da primeira celula com mais de uma escolha
(defun expand (matrix)
    (labels (
        (first-not-single-cell (flattened-matrix)
            (if (not (single (cell-value (car flattened-matrix))))
                ;then
                (car flattened-matrix)
                ;else
                (first-not-single-cell (cdr flattened-matrix))))

        (make-single-choice-matrix (value cll matrix)
            (setq matrix-copy 
                (loop for row in matrix collect (mapcar #'copy-structure row)))
            (setq cell-copy (cell-from-pos (cell-position cll) matrix-copy))
            (setf (cell-value cell-copy) (list value))
            (return-from make-single-choice-matrix matrix-copy)))
        ;;do
        (progn 
            (setq cll (first-not-single-cell (flatten matrix)))
            (loop for value in (cell-value cll)
                collect (make-single-choice-matrix value cll matrix)))))

;; Calcula e retorna uma lista com soluções para a matriz de celulas passada
(defun solve (matrix)
    (filter-choices (prune (fill-values-with-choices matrix))))

;; Resolve o tabuleiro kojun em path e printa
(defun solve-and-print (path)
    (setq grid (read-puzzle path))
    (setq matrix (solve grid))
    (setq ffm (full-flatten matrix))
    (setq ffm-value (mapcar #'cell-value ffm))
    (setq matrix-size (sqrt (length ffm-value)))
    (chunks-of ffm-value matrix-size)
)