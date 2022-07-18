(define (right-split painter n)
    (if (= 0 n)
        painter
        (let ((smaller (right-split painter (- n 1))))
             (beside painter (below smaller smaller))
        )
    )
)


(define (up-split painter n)
    (if (= 0 n)
        painter
    )
    (let ((smaller (up-split painter (- n 1))))
        (below (beside smaller smaller) painter)
    )
)

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split (- n 1)))
              (right (right-split (- n 1)))
             )
             (beside (below (beside up up) painter) (below (corner-split (- n 1) (below right right))))
        )
    )
)

(define (square-limit n)
    (let ((top-right (corner-split painter n)))
        (let ((top-left (flip-horiz top-right))
              (bottom-left (flip-horiz (flip-vert top-right)))
              (bottom-right (flip-vert top-right))
             )
             (beside (below top-left bottom-left) (below top-right bottom-right))
        )
    )
)


(define (frame-coord-map frame)
    (lambda (v) (
        add-vect (origin-frame frame)
                 (add-vect (scale-vec (xcor-vec v) (edge-1-frame frame))
                           (scale-vec (ycor-vec v) (edge-2-frame frame))
                 ) 
                )
    )
)

(define (segment->painter segment-list)
    (lambda (frame)
        (for-each
                (lambda (segment)
                    (draw-line ((frame-coord-map frame) (start-segment segment))
                               ((frame-coord-map frame) (end-segment segment) 
                )
                )
        )

        )
    )
)

(define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame-coord-map frame)))
            (let ((new-origin (m origin)))
                (painter (make-frame new-origin
                                     (sub-vect (m corner1) new-origin)
                                     (sub-vect (m corner2) new-origin)
                         )
                )
            )
        )
    )
)

(define (flip-vert painter)
    (transform-painter painter (make-vect 0 1) 
                               (make-vect 1 1)
                               (make-vect 0 0)
    )
)

(define (shrink-to-upper-right painter)
    (transform-painter painter (make-vect 0.5 0.5)
                               (make-vect 1 0.5)
                               (make-vect 0.5 1)
    )
)

(define (rotate-90 painter)
    (transform-painter painter (make-vect 1 0)
                               (make-vect 1 1)
                               (make-vect 0 0)
    )
)

(define (squash-inward painter)
    (transform-painter painter (make-vect 0 0)
                               (make-vect 0.65 0.35)
                               (make-vect 0.35 0.65)
    )
)

(define (beside painter1 painter2)
    (let ((left-painter (transform-painter painter1 (make-vect 0 0)
                                                    (make-vect 0.5 0)
                                                    (make-vect 0 1)))
          (right-painter (transform-painter painter2 (make-vect 0.5 0)
                                                     (make-vect 1 0)
                                                     (make-vect 0.5 1)))
        )
        (lambda (frame) (left-painter frame) (right-painter frame))
    )
)