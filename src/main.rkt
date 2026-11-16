#lang racket

;; Lee un archivo de entrada y devuelve una lista de tuplas (vértices, aristas)
(define (leer-archivo path)
  (with-input-from-file path
    (lambda ()
      (let loop ((result '()))
        (if (eof-object? (peek-char)) ; si se llegó al final del archivo
            (reverse result) ; devuelve la lista en orden original
            (let ((dato (read)))
              (if (and (pair? dato) (= (length dato) 2)) ; debe tener dos elementos (v, a)
                  (loop (cons dato result))
                  (loop result))))))))

;; Verifica si un elemento x está en la lista lst
(define (miembros x lst)
  (if (empty? lst)
      #f
      (if (equal? x (car lst))
          #t
          (miembros x (cdr lst)))))
;; Calcula la longitud de una lista
(define (longitud lst)
  (if (empty? lst)
      0
      (+ 1 (longitud (cdr lst)))))
;; Verifica si todos los elementos de a están en b
(define (incluido? a b)
  (if (empty? a)
      #t
      (and (miembros (car a) b)
           (incluido? (cdr a) b))))
;; Verifica si dos listas contienen los mismos elementos (como conjuntos)
(define (igual-conjuntos? a b)
  (and (incluido? a b) (incluido? b a)))


;; Determina si el grafo es nulo (sin vértices ni aristas)
(define (es-nulo? v a)
  (and (empty? v) (empty? a)))
;; Determina si el grafo es vacío (con vértices pero sin aristas)
(define (es-vacio? v a)
  (and (not (empty? v)) (empty? a)))
;; Determina si el grafo tiene solo un vértice y ninguna arista
(define (es-unitario? v a)
  (and (not (empty? v))
       (empty? (cdr v))
       (empty? a)))
;; Determina si el grafo es simple (sin lazos ni aristas duplicadas)
(define (es-simple? v a)
  (define (sin-lazos-ni-repetidas? aristas revisadas)
    (cond
      [(empty? aristas) #t]
      [else
       (let ((e (car aristas)))
         (if (or (equal? (car e) (cadr e)) ; lazo
                 (miembros e revisadas) ; duplicada exacta
                 (miembros (reverse e) revisadas)) ; duplicada inversa
             #f
             (sin-lazos-ni-repetidas? (cdr aristas) (cons e revisadas))))]))
  (sin-lazos-ni-repetidas? a '()))

;; Retorna los vecinos de un nodo considerando direcciones
(define (vecinos-de nodo a)
  (if (empty? a)
      '()
      (let ((x (car (car a)))
            (y (cadr (car a))))
        (append
         (if (equal? x nodo) (list y) '())
         (if (equal? y nodo) (list x) '())
         (vecinos-de nodo (cdr a))))))
;; Retorna los vecinos sin repetir
(define (vecinos-unicos nodo a)
  (remove-duplicates
   (if (empty? a)
       '()
       (let ((x (car (car a)))
             (y (cadr (car a))))
         (append
          (if (equal? x nodo) (list y) '())
          (if (equal? y nodo) (list x) '())
          (vecinos-unicos nodo (cdr a)))))))
;; Recorre el grafo desde un nodo, acumulando los visitados
(define (visitar v a visitados cola)
  (if (empty? cola)
      visitados
      (let ((actual (car cola)))
        (if (miembros actual visitados)
            (visitar v a visitados (cdr cola))
            (let ((vecinos (vecinos-unicos actual a)))
              (visitar v a (cons actual visitados)
                       (append (cdr cola) vecinos)))))))
;; Determina si el grafo es conexo
(define (es-conexo? v a)
  (if (empty? v)
      #f
      (let ((alcanzados (visitar v a '() (list (car v)))))
        (igual-conjuntos? v alcanzados))))
;; Verifica si dos nodos están conectados por una arista (en cualquier orden)
(define (miembros-arista? x y a)
  (or (miembros (list x y) a)
      (miembros (list y x) a)))
;; Verifica recursivamente si todo par de vértices está conectado
(define (completo-rec v1 v2 a)
  (if (empty? v1)
      #t
      (if (empty? v2)
          (completo-rec (cdr v1) v2 a)
          (let ((x (car v1))
                (y (car v2)))
            (if (equal? x y)
                (completo-rec v1 (cdr v2) a)
                (if (miembros-arista? x y a)
                    (completo-rec v1 (cdr v2) a)
                    #f))))))
;; Determina si el grafo es completo
(define (es-completo? v a)
  (define (conectados? x y)
    (or (miembros (list x y) a)
        (miembros (list y x) a)))
  (define (todos-conectados? vs)
    (cond
      [(empty? vs) #t]
      [else
       (and (andmap (lambda (u)
                      (if (equal? u (car vs))
                          #t
                          (conectados? (car vs) u)))
                    vs)
            (todos-conectados? (cdr vs)))]))
  (if (< (longitud v) 2)
      #f
      (todos-conectados? v)))
;; Determina si el grafo es bipartido
(define (es-bipartido? v a)
  (define colores (make-hash)) ; tabla: nodo -> color 0 o 1
  ;; BFS para colorear nodos y verificar bipartición
  (define (bfs nodo)
    (let loop ((cola (list nodo)))
      (cond
        [(empty? cola) #t]
        [else
         (let* ((actual (car cola))
                (resto (cdr cola))
                (color (hash-ref colores actual))
                (vecinos (vecinos-unicos actual a)))
           (define (procesar-vecinos vecinos nuevos)
             (cond
               [(empty? vecinos) (loop (append resto nuevos))]
               [else
                (let* ((v (car vecinos))
                       (ya? (hash-has-key? colores v)))
                  (cond
                    [(and ya? (= (hash-ref colores v) color))
                     #f] ; conflicto
                    [(not ya?)
                     (hash-set! colores v (- 1 color))
                     (procesar-vecinos (cdr vecinos) (cons v nuevos))]
                    [else
                     (procesar-vecinos (cdr vecinos) nuevos)]))]))
           (procesar-vecinos vecinos '()))])))

  ;; Recorre los nodos no coloreados aún
  (define (revisar nodos)
    (cond
      [(empty? nodos) #t]
      [(hash-has-key? colores (car nodos)) (revisar (cdr nodos))]
      [else
       (hash-set! colores (car nodos) 0)
       (if (bfs (car nodos))
           (revisar (cdr nodos))
           #f)]))

  (revisar v))

;; Determina si el grafo es bipartido completo
(define (es-bipartido-completo? v a)
  (if (not (es-bipartido? v a))
      #f
      (let ((colores (make-hash)))

        ;; BFS para colorear los nodos (0 y 1)
        (define (bfs nodo)
          (let loop ((cola (list nodo)))
            (cond
              [(empty? cola) #t]
              [else
               (let* ((actual (car cola))
                      (resto (cdr cola))
                      (color (hash-ref colores actual))
                      (vecinos (vecinos-de actual a)))

                 (define (procesar vecinos nuevos)
                   (cond
                     [(empty? vecinos) (loop (append resto nuevos))]
                     [else
                      (let ((v (car vecinos)))
                        (cond
                          [(and (hash-has-key? colores v)
                                (= (hash-ref colores v) color)) #f]
                          [(not (hash-has-key? colores v))
                           (hash-set! colores v (- 1 color))
                           (procesar (cdr vecinos) (cons v nuevos))]
                          [else
                           (procesar (cdr vecinos) nuevos)]))]))
                 (procesar vecinos '()))])))

        ;; Colorea todos los nodos conectados
        (define (revisar nodos)
          (cond
            [(empty? nodos) #t]
            [(hash-has-key? colores (car nodos)) (revisar (cdr nodos))]
            [else
             (hash-set! colores (car nodos) 0)
             (if (bfs (car nodos))
                 (revisar (cdr nodos))
                 #f)]))

        ;; verifica conexiones completas entre grupos
        (if (not (revisar v))
            #f
            (let* ((grupo1 (filter (lambda (n) (= (hash-ref colores n) 0)) v))
                   (grupo2 (filter (lambda (n) (= (hash-ref colores n) 1)) v))
                   (esperadas (* (longitud grupo1) (longitud grupo2)))
                   (reales
                    (foldl (lambda (e acc)
                             (let ((x (car e))
                                   (y (cadr e)))
                               (if (not (= (hash-ref colores x)
                                           (hash-ref colores y)))
                                   (+ acc 1)
                                   acc)))
                           0 a)))
              ;; Ambos grupos deben tener al menos un nodo y todas las conexiones deben existir
              (and (not (or (empty? grupo1) (empty? grupo2)))
                   (= reales esperadas)))))))
;; Determina si el grafo es plano (si cumple desigualdad de Euler)
(define (es-plano? v a)
  (let ((n (longitud v))
        (e (longitud a)))
    (and (es-simple? v a)
         (or (< n 3)
             (<= e (- (* 3 n) 6))))))
;; Determina si el grafo es un árbol (conexo y con n-1 aristas)
(define (es-arbol? v a)
  (and (es-conexo? v a)
       (= (longitud a) (- (longitud v) 1))))
;; Calcula el grado de un nodo
(define (grado nodo a)
  (if (empty? a)
      0
      (+ (if (or (equal? nodo (car (car a)))
                 (equal? nodo (cadr (car a))))
             1
             0)
         (grado nodo (cdr a)))))
;; Verifica si todos los nodos tienen grado par
(define (todos-grados-pares? v a)
  (if (empty? v)
      #t
      (let ((g (grado (car v) a)))
        (and (even? g)
             (todos-grados-pares? (cdr v) a)))))
;; Determina si el grafo es euleriano (conexo y todos los grados pares)
(define (es-euleriano? v a)
  (and (es-conexo? v a)
       (todos-grados-pares? v a)))

;; Muestra los tipos a los que pertenece un grafos
(define (mostrar-tipos v a idx)
  (define tipos '())

  (if (es-nulo? v a)
      (set! tipos (list "Nulo"))
      (begin
        (if (es-vacio? v a) (set! tipos (cons "Vacío" tipos)) #f)
        (if (es-unitario? v a) (set! tipos (cons "Unitario" tipos)) #f)
        (if (es-simple? v a) (set! tipos (cons "Simple" tipos)) #f)
        (if (es-conexo? v a) (set! tipos (cons "Conexo" tipos)) #f)
        (if (es-completo? v a) (set! tipos (cons "Completo" tipos)) #f)
        (if (es-bipartido? v a) (set! tipos (cons "Bipartido" tipos)) #f)
        (if (es-bipartido-completo? v a) (set! tipos (cons "Bipartido Completo" tipos)) #f)
        (if (and (es-simple? v a) (es-plano? v a)) (set! tipos (cons "Plano" tipos)) #f)
        (if (es-arbol? v a) (set! tipos (cons "Árbol" tipos)) #f)
        (if (es-euleriano? v a) (set! tipos (cons "Euleriano" tipos)) #f)))

  ;; Imprime resultado
  (display "Grafo ")
  (display idx)
  (display " | V: ") (display v)
  (display " | A: ") (display a)
  (display " | Tipos: ")
  (if (empty? tipos)
      (displayln "No pertenece a ningún tipo conocido")
      (displayln (string-join (reverse tipos) ", "))))
;; Función principal que lee los grafos y muestra los tipos
(define (main)
  (let ((grafos (leer-archivo "data.io")))
    (if (empty? grafos)
        (displayln "No hay grafos.")
        (let loop ((gs grafos) (i 1))
          (if (empty? gs)
              (void)
              (begin
                (mostrar-tipos (car (car gs)) (cadr (car gs)) i)
                (loop (cdr gs) (+ i 1))))))))

(main)


