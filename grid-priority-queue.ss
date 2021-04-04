(define queue '())
(define path ' ())

(define frontier 1)

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define value
  (lambda (pair)
    (+ (abs (- (car pair) (car goal))) (abs (- (car (cdr pair)) (car (cdr goal))))))) ;(+ (abs (- (car pair) (car start))) (abs (- (car (cdr pair)) (car (cdr start))))))))    


(define pop
  (lambda ()
    (if (null? path)
        '()
    ;else   
        (let ((temp2 (front2)))
           (set! path (cdr path))
           temp2))
      
    ))

(define front2
  (lambda ()
    (if (null? path)
        '()
    ;else
        (cadr path)))) 

(define front
  (lambda ()
    (if (null? queue)
        '()
    ;else
        (car queue))))

(define dequeue
  (lambda ()
    (if (null? queue)
        '()
    ;else   
        (let ((temp (front)))
           (set! queue (cdr queue))
           (set! path (cons temp path))
           temp))
      
    ))

(define enqueue
  (lambda (pair)
    (place pair (value pair) '() queue)))

(define place
  (lambda (pair val front lst)
    (cond
      [(null? lst) (set! queue (append front (list pair)))] 
      [(> (value (car lst)) val) (set! queue (append front (cons pair lst)))]  ;base case places value in the list
      [else (place pair val (reverse (cons (car lst) (reverse front))) (cdr lst))])
       ))  ;recursively loops through list to find place


