;A red black tree is firstly a binary search tree.
;Then in order to be called red-black, we must color the nodes
;under some rules.



;---------------------------------------------------------------------
;first let us write the node data structure
;its state property :
;     1. key
;     2. color
;     3. satellite data
;     (above 3 is passed by the user when call 'make-node' proc
;     4. parent
;     5. right
;     6. left
;     (4-6 is the pointer to other node, '() when is nil)
;its interface proc :
;     1. about get info :
;         1.1 get-parent
;         1.2 get-right
;         1.3 get-left
;         1.4 get-key
;         1.5 get-satellite
;         1.6 get-color
(define RED 1)
(define BLACK 0)
(define (make-node k s c)
  (let ((parent '())
        (right '())
        (left '())
        (key k)
        (satellite s)
        (color c))
    (define (set-parent! p)
      (set! parent p))
    (define (set-right! r)
      (set! right r))
    (define (set-left! l)
      (set! left l))
    (define (set-key! k)
      (set! key k))
    (define (set-satellite! s)
      (set! satellite s))
    (define (set-color! c)
      (set! color c))
    (define (dispatch message)
      (cond ((eq? message 'get-parent) parent)
            ((eq? message 'get-right) right)
            ((eq? message 'get-left) left)
            ((eq? message 'get-key) key)
            ((eq? message 'get-satellite) satellite)
            ((eq? message 'get-color) color)
            ((eq? message 'set-parent) set-parent!)
            ((eq? message 'set-right) set-right!)
            ((eq? message 'set-left) set-left!)
            ((eq? message 'set-key) set-key!)
            ((eq? message 'set-satellite) set-satellite!)
            ((eq? message 'set-color) set-color!)
            (else (error "Unknown request -- NODE" message))))
    dispatch))

(define (get-key node) (node 'get-key))
(define (get-parent node) (node 'get-parent))
(define (get-right node) (node 'get-right))
(define (get-left node) (node 'get-left))
(define (get-color node) (node 'get-color))
(define (get-satellite node) (node 'get-satellite))

(define (set-key! node k) ((node 'set-key) k))
(define (set-parent! node p) ((node 'set-parent) p))
(define (set-right! node r) ((node 'set-right) r))
(define (set-left! node l) ((node 'set-left) l))
(define (set-color! node c) ((node 'set-color) c))
(define (set-satellite! node s) ((node 'set-satellite) s))

