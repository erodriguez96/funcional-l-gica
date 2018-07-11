(require racket/trace)
;simple exercises in scheme, trying not to use built-in functions

; inserts a value in order in the list
(define (insert value lst)
    (cond 
        ((null? lst)(list value))        
        ((<= value (car lst)) (cons value lst))    
        (else (cons (car lst)(insert value (cdr lst))))
    )
) 

; return even numbers in the list
(define (evenlist lst)
    (cond
        ((null? lst) '())
        ((pair? (car lst)) (append (evenlist (car lst)) (evenlist (cdr lst)) ) )
        ((even? (car lst)) (cons (car lst) (evenlist (cdr lst)) ))
        (else (evenlist (cdr lst)))
    )    
)

; return the last element of the list
(define (my_last lst)
    (cond
        ((null? lst) list '())
        ((null? (cdr lst)) (car lst))
        ((not(null? (cdr lst))) (my_last(cdr lst)))
    )
)

;return the last but one element of the list
(define (last_but_one lst)
    (cond
        ((null? lst) list '())
        ((null? (cdr lst)) '())
        ((null? (cddr lst)) (car lst))
        ((list? (cdr lst)) (last_but_one(cdr lst)))
    )
)

; returns the element in the K'th position of the list
(define (k_element lst pos)
    (cond
        ((null? lst) #f)
        ((= pos 1) (car lst))
        ((not(= pos 1)) (k_element (cdr lst) (- pos 1) ))
    )
)

; removes the element in the position specified
(define (del_element lst pos)
    (cond
        ((null? lst) '())
        ((= pos 1) (cdr lst))
        (else (cons (car lst) (del_element (cdr lst) (- pos 1))))
    )
)

; returns count of elements in the list
(define (nelements lst)
    (if (null? lst) 0 (+ 1 (nelements (cdr lst))))
)

; returns the even position values of the list
(define (posPares lst)
    (cond 
        ((null? lst) '())
        ((null? (cdr lst)) '())
        ((list? lst) (cons (cadr lst) (posPares (cddr lst)) ) )
    )
)

; returns a flattened list out of the parameter list
(define (aplana lst)
    (cond
        ((null? lst) '())
        ((list? (car lst)) (append (aplana (car lst)) (aplana (cdr lst)) ) )
        ((not(list? (car lst))) (cons (car lst) (aplana (cdr lst))) )
    )
)

; returns true if list1 is contained in list2, false otherwise
(define (contenida lst1 lst2)
    (cond
        ((and (null? lst1) (null? lst2)) #t)
        ((null? lst1) #t)
        ((null? lst2) #f)
        ((contenidaux lst1 lst2 ) #t)
        (else (contenida lst1 (cdr lst2)) )
    )    
)

(define (contenidaux lst1 lst2)
    (cond
        ((null? lst1) #t)
        ((null? lst2) #f)
        ((equal? (car lst1) (car lst2)) (contenidaux (cdr lst1) (cdr lst2) ))
        (else #f)
    ) 
)

; mixes list1 and lis2
(define (trenza lst1 lst2) 
    (cond
        ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else(cons (car lst1) (trenza lst2 (cdr lst1))))
    )
)

; returns longest iteration of the first element from the first position
(define (larga lst)
    (cond 
        ((null? lst) '())
        ((not(eqv? (car lst) (cadr lst))) (list (car lst)))
        ((eqv? (car lst) (cadr lst)) (cons (car lst) (larga (cdr lst))) )
    )
)

; removes the element in the n'th position of the list
(define (removeN lst n)
    (cond
        ((null? lst) '())
        ((<= n 0) lst)
        ((<= (nelements lst) n) '())
        (else (cons (car lst) (removeN (cdr lst) n))  )
    )    
)

; returns a list with all integers between min and max
(define (rango min max)
    (cond
        ((>= min max) '())
        ((< min max) (cons min (rango (+ min 1) max)))
    )
)

; returns the list without duplicates 
(define (nodup lst)
    (cond
        ((null? lst) '())
        ((null? (cdr lst)) (cons (car lst) '()))
        ((eqv? (car lst) (cadr lst)) (nodup (cdr lst)))
        ((not(eqv? (car lst) (cadr lst))) (cons (car lst) (nodup (cdr lst))) )
    )    
)

; duplicates every element in the list
(define (duplicate lst)
    (cond
        ((null? lst) '())
        (else (cons (car lst) (cons (car lst) (duplicate (cdr lst))) ))
    )
)

; returns every element in lstA that isnt in lstB
(define (diferencia lstA lstB)
    (cond
        ((null? lstA) '())
        ((null? lstB) lstA)
        ((isContained (car lstA) lstB) (diferencia (cdr lstA) lstB) )
        (else (cons (car lstA) (diferencia (cdr lstA) lstB)))
    )
)

(define (isContained n lstB)
    (cond
        ((null? n) #t)
        ((null? lstB) #f)
        ((eqv? n (car lstB)) #t)
        (else (isContained n (cdr lstB)))
    )
)

; remove the element in the n position
(define (remove_nth lst n)
    (cond
        ((null? lst) '())  
        ((= n 1) '())
        (else (remove_nth (nthaux lst n) (+ n 1)))
    )
)

(define (nthaux lst n)
    (cond
        ((null? lst) '())
        ((= n 1) (cdr lst))
        ((> n (nelements lst)) lst)
        (else (cons (car lst) (nthaux (cdr lst) (- n 1))))
    )
)

; replaces every occurence of A with B in the list
(define (replace lst A B)
    (cond
        ((null? lst) '()) 
        ((null? A) lst)
        ((eqv? (car lst) A) (cons B (replace (cdr lst) A B)))
        (else(cons (car lst) (replace (cdr lst) A B )))
    )
)

; removes an element from the list every n elements
(define (ndelete lst n)
    (let recur ((i 1) (rest lst))
        (cond 
            ((null? rest) '())
            ((= i n) (recur 1 (cdr rest)))
            (else (cons (car rest) (recur (+ i 1) (cdr rest))))
        )
    )
)

; returns the elements that are found in both lists
(define (intersection lst1 lst2)
    (cond
        ((or (null? lst1) (null? lst2)) '())
        ((intersectionAux (car lst1) lst2) (cons (car lst1) (intersection (cdr lst1) lst2) ))
        (else (intersection (cdr lst1) lst2 ))
    )
)

(define (intersectionAux n lst)
    (cond
        ((null? lst) #f)
        ((eqv? n (car lst)) #t)
        (else (intersectionAux n (cdr lst)))
    )
)

; returns the elements in both lists skipping duplicates
(define (union lst1 lst2)
    (cond
        ((null? lst2) lst1)
        ((unionAux (car lst2) lst1) (union lst1 (cdr lst2)))
        (else (union (cons (car lst2) lst1) (cdr lst2)) )
        ;(else (union (append lst1 (list (car lst2))) (cdr lst2)) )
    )    
)

(define (unionAux n lst)
    (cond
        ((null? lst) #f)
        ((eqv? n (car lst)) #t)
        (else (unionAux n (cdr lst)))
    )
)

; returns the longest repetition of the last element in the list
(define (final lst)
    (primero (reverse lst))
)

(define (reverse lst)
    (cond
        ((null? lst) '())
        (else (append (reverse (cdr lst)) (list (car lst))))
    )    
)

(define (primero lst)
    (cond
        ((null? lst) '())
        ((null? (cdr lst)) (list (car lst)))
        ((eqv? (car lst) (cadr lst)) (append (list (car lst)) (primero (cdr lst))) )
        (else (list (car lst)))
    )    
)

; returns the max depth of the list
(define (profundidad Lista)
    (cond 
        ((null? Lista) 1)
        ((not(list? Lista)) 0)
        ((list? (car Lista))
            (let ( (A (+(profundidad (car Lista))1)) (B (profundidad (cdr Lista))) )
                (if (> A B) A B)
            )        
        )
        (else (profundidad (cdr Lista)))
    )
)

; adds element by element
(define (add lst1 lst2)
    (cond
        ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else (cons (+ (car lst1) (car lst2)) (add (cdr lst1) (cdr lst2)) ) )
    )    
)

; returns true if a list is a palindrome
; (define (palindrome lst)
;     (cond
;         ((null? lst) #t)
;         ((equals lst (my_reverse lst)) #t)
;         (else #f)
;     )
; )

(define (equals lst1 lst2)
    (cond
        ((and (null? lst1) (null? lst2) ) #t)
        ((eqv? (car lst1) (car lst2)) (equals (cdr lst1) (cdr lst2)))
        (else #f)
    )    
)

(define (my_reverse lst)
    (cond
        ((null? lst) '())
        (else (append (my_reverse (cdr lst)) (list (car lst))))
    )    
)
; remove the last n elements of the list
(define (last_n lst n)
    (borra lst (- (longitud lst) n))
)

(define (borra lst n)
    (cond
        ((<= n 0) '())
        (else (cons (car lst) (borra (cdr lst) (- n 1))) )
    )
)

(define (longitud lst)
    (cond
        ((null? lst) 0)
        (else (+ (longitud (cdr lst)) 1) )
    )    
)

(define (borraN lst n)
    (cond
        ((null? lst) '())
        (else (borraNAux lst n))
    )    
)

(define (borraNAux lst n)
    (cond
        ((null? lst) '())
        ((null? (cdr lst)) (car lst))
        ((= n 0) (borraNAux lst n))
        (else (cons (car lst) (borraNAux lst (- n 1))))
    )    
)

(define (palindrome lst)
    (if (comprueba lst (invierte lst)) #t #f)    
)

(define (comprueba lst invertida)
    (cond
        ((null? lst) #t)
        ((not(eqv? (car lst) (car invertida))) #f)
        (else (comprueba (cdr lst) (cdr invertida)) )
    )
)

(define (invierte lst)
    (cond
        ((null? lst) '()) 
        (else (append (invierte (cdr lst)) (list (car lst)) ))
    )    
)




; (display (last_n '(1 2 3 4 5 6) 2))
; (newline)
; (display (last_n '(1 2 3 4 5 6) 6))
; (newline)
; (display (last_n '(1 2 3 4 5 6) 8))


; (display (my_reverse '(b c b a)))
; (newline)
; (display (palindrome '()))
; (newline)
; (display (palindrome '(a b c b a)))
; (newline)
; (display (palindrome '(a b c d a b)))


; tests, uncomment to see results. (trace "name") before the tests to activate the trace 
; (display "Tests")
; (newline)
; (display "trenza")
; (newline)
; (display (trenza '(a b c) '(a h d)) )
; (newline)
; (display "diferencia")
; (newline)
; (display (diferencia '(a b c) '(a h d)) )
; (newline)
; (display (diferencia '(1) '(1)) )
; (newline)
; (display (diferencia '() '()) )
; (newline)
; (display (diferencia '(a b c) '(d e f)) )
; (newline)
; (display (diferencia '(1) '()) )
; (newline)
; (display "posPares")
; (newline)
; (display (posPares '(2) ))
; (newline)
; (display (posPares '(1) ))
; (newline)
; (display (posPares '(1 -1 3) ))
; (newline)
; (display (posPares '(3 5 2 1 2 1 6) ))
; (newline)
; (display "Aplana")
; (newline)
; (display (aplana '(3 -1 (2) 4) ))
; (newline)
; (display (aplana '(3 -1 ((2) 4)) ))
; (newline)
; (display (aplana '(3 -1 (3 -1 2 4) 2 4) ))
; (newline)
; (display (aplana '(3 -1 (3 -1 2 4) 2 (3 -1 ((3 (( 1 ))) 2) 2 ((())) 4) 4) ))
; (newline)
; (display "Contenida")
; (newline)
; (display (contenida '(2 4) '(1 2 3 4)) )
; (newline)
; (display (contenida '(1 2) '(1 1 1 1 1 2 2 1 1)) )
; (newline)
; (display (contenida '() '(1 2 3 4)) )
; (newline)
; (display (contenida '(1 2) '()) )
; (newline)
; (display "k_element")
; (newline)
; (display (k_element '() 1) )
; (newline)
; (display (k_element '(a) 1) )
; (newline)
; (display (k_element '(a v) 2) )
; (newline)
; (display (k_element '(a v b a s d g e) 10) )
; (newline)
; (display "My_last")
; (newline)
; (display (my_last '() ) )
; (newline)
; (display (my_last '(a) ) )
; (newline)
; (display (my_last '(a v) ) )
; (newline)
; (display (my_last '(a v b a s d g e) ) )
; (newline)
; (display "Larga")
; (newline)
; (display (larga '(a a a b a c a a a) ))
; (newline)
; (display (larga '(a b a c a a a) ))
; (newline)
; (display (larga '(a a a a a a a a a a b a c a a a) ))
; (newline)
; (display (larga '(a a b a c a a a) ))
; (newline)
; (display (larga '(a a a a A b a c a a a) ))
; (newline)
; (display "Del_element")
; (newline)
; (display (del_element '() 1) )
; (newline)
; (display (del_element '(a) 1) )
; (newline)
; (display (del_element '(a v) 2) )
; (newline)
; (display (del_element '(a v b a s d g e) 4) )
; (newline)
; (display "removeN")
; (newline)
; (display (removeN '(a) 3) )
; (newline)
; (display (removeN '(1 2 3 4 5) 3) )
; (newline)
; (display (removeN '() 3) )
; (newline)
; (display (removeN '(a b c d e f g) 0) )
; (newline)
; (display "Rango")
; (newline)
; (display (rango 15 3))
; (newline)
; (display (rango 1 1))
; (newline)
; (display (rango 2 15))
; (newline)
; (display (rango -15 0))
; (newline)
; (display "Nodup")
; (newline)
; (display (nodup '(a a a a a a b b b c b b d d d e f f)) )
; (newline)
; (display (nodup '(a)) )
; (newline)
; (display (nodup '()) )
; (newline)
; (display "duplicate")
; (newline)
; (display (duplicate '(a b c)))
; (newline)
; (display "Diferencia")
; (newline)
; (display (diferencia '(a b c) '(b c d)))
; (newline)
; (display (diferencia '(a b c d e f) '(b c d)))
; (newline)
; (display (diferencia '(a b c) '(b c d e f)))
; (newline)
; (display (diferencia '(1 2 3 4) '(5 6 7 3 8 9 10 1 2)))
; (newline)
; (display (diferencia '() '(b c d)))
; (newline)
; (display (diferencia '(a b c) '()))
; (newline)
; (display "Replace")
; (newline)
; (display (replace '(1 1 2 3 4 1 2 4 3) 1 7))
; (newline)
; (display (replace '(a a b c d a b d c) 'a 'h))
; (newline)
; (display "Intersection")
; (newline)
; (display (intersection '(a b c) '() ))
; (newline)
; (display (intersection '() '(a b c) ))
; (newline)
; (display (intersection '(a b c d e) '(a c f g) ))
; (newline)
; (display (intersection '(1 2 3 4 5) '(a c f g) ))
; (newline)
; (display (intersection '(a b c d e) '(y u b D e a c) ))
; (newline)
; (display "union")
; (newline)
; (display (union '() '(a b c d e) ))
; (newline)
; (display (union '(a g h e s) '() ))
; (newline)
; (display (union '(1 2 3 4 5) '(6 7 8 9 1 2 3) ))
; (newline)
; (display (union '(a b c d e) '(f g h i j) ))
; (newline)
; (display "Final")
; (newline)
; (display (final '(a))) ; '(a)
; (newline)
; (display (final '())) ; '()
; (newline)
; (display (final '(a b a c a a a))) ; '(a a a)
; (newline)
; (display (final '(v v v v v v v v v))) ; '(v v v v v v v v v)
; (newline)
; (display (final '(1 2 3 3 3 3 3 3 3 3 3))) ; '(3 3 3 3 3 3 3 3 3)
; (newline)
; (display "Profundidad")
; (newline)
; (display (profundidad 'a)) ; 0
; (newline)
; (display (profundidad '())) ; 1
; (newline)
; (display (profundidad '(a(b)))) ; 2
; (newline)
; (display (profundidad '(a (b (c)) h (c (d (g) ))))) ; 4
; (newline)
; (display (profundidad '(a (a b (c) d (e (f (g) ) ) ) ))) ; 5
; (newline)
; (display "Add")
; (newline)
; (display (add '(1 2 3) '(1 2 3) ))
; (newline)
; (display (add '() '(1 2 3) ))
; (newline)
; (display (add '(1 2 3) '() ))
; (newline)
; (display (add '(1) '(1) ))