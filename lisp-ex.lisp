(in-package :mfl)

(om::defmethod! range-divided-to-list (low high div)
  :initvals '(1 100 2)
  :indoc '("low" "high" "divided-by" )
  :icon 132
  :doc "creates a list of values produced by adding the <range> divided by <div>"
  (let* ((n (- high low))
         (n1 (float (/ n div))))
    (labels ((list-sums (n n1 div)           
               (if (zerop div)
                     nil                         
                     (cons (+ n low)
                           (list-sums (- n n1) n1 (1- div))))))
      (append (list low) 
              (reverse (mapcar #'round 
                               (list-sums n n1 div)))))))

(om::defmethod! random-signed (low high)
  :initvals '(0 1)
  :indoc '("low" "high" )
  :icon 132
  :doc "creates a random-n within range and randomly assigns sign+/-"
  (let ((n (+ low (random (+ 1 (- high low))))))
    (if (= (random 2) 0)
        (* n -1)
        n)))

(defun permut-random (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun sym-div-asc (start primary)
               (let* ((interval (* (/ 12 primary) 100))
                     (list (loop for i
                                 from start
                                 to (+ start (* interval primary))
                                 by interval
                                 collect i)))                                   
                 (butlast (mapcar #'(lambda (x) (* x 100))
                         (mapcar #'round 
                                 (mapcar #'(lambda (x) (* x 0.01)) list))))))
  
(defun sym-div-desc (start primary)
               (let* ((interval (* (/ 12 primary) 100))
                     (list (loop for i
                                 from (abs (- start (* interval primary)))
                                 to start
                                 by interval 
                                 collect i)))                                   
                 (butlast (reverse (mapcar #'(lambda (x) (* x 100))
                         (mapcar #'round 
                                 (mapcar #'(lambda (x) (* x 0.01)) list)))))))


(defun pattern-a (start primary-lst bpf-lst)
       (cond ((null bpf-lst) 'lst-empty!) 
             ((= (car bpf-lst) -1)
              (append (list (car (sym-div-desc start (car primary-lst)))) 
                       (permut-random (cdr (sym-div-desc start (car primary-lst))))))
              (t (append (list (car (sym-div-asc start (car primary-lst))))
                        (permut-random (cdr (sym-div-asc start (car primary-lst))))))))


(defun reverse-direction-if (sequence low high primary)
  (cond ((some #'(lambda (x) (> x high)) (cdr sequence))
   
           (sym-div-desc (first sequence) primary))
        
        ((some #'(lambda (x) (< x low)) (cdr sequence))
            (sym-div-asc (first sequence) primary))
      
        (t sequence)))


(om::defmethod! create-pattern-1 (start low high primary-lst secondary bpf-lst) 
  :initvals '(6000 4800 8400 '(2) 100 '(1))
  :indoc '("start" "low" "high" "primary-lst" "secondary" "bpf-lst")
  :icon 132
  :doc "creates a melodic sequence based on thesis rule"
 (let ((pattern (pattern-a start primary-lst bpf-lst)))          
    (if (null bpf-lst)                 
        nil         
            (append (setf pattern 
                          (reverse-direction-if pattern low high (car primary-lst)))                                     
                    (create-pattern-1 (if (= (car bpf-lst) -1)
                                          (- (car (reverse pattern)) (* secondary (random-signed 1 1)))
                                        (+ (car (reverse pattern)) (* secondary (random-signed 1 1))))                                              
                                      low high (cdr primary-lst) secondary (cdr bpf-lst))))))


;Example #2 (slight variation on #1);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mfl)

(om::defmethod! range-divided-to-list (low high div)
  :initvals '(1 100 2)
  :indoc '("low" "high" "divided-by" )
  :icon 132
  :doc "creates a list of values produced by adding the <range> divided by <div>"
  (let* ((n (- high low))
         (n1 (float (/ n div))))
    (labels ((list-sums (n n1 div)           
               (if (zerop div)
                     nil                         
                     (cons (+ n low)
                           (list-sums (- n n1) n1 (1- div))))))
      (append (list low) 
              (reverse (mapcar #'round 
                               (list-sums n n1 div)))))))

(om::defmethod! random-signed (low high)
  :initvals '(0 1)
  :indoc '("low" "high" )
  :icon 132
  :doc "creates a random-n within range and randomly assigns sign+/-"
  (let ((n (+ low (random (+ 1 (- high low))))))
    (if (= (random 2) 0)
        (* n -1)
        n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun permut-random (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)


(defun mc-sym-div-asc (start pitch-partition primary)
               (let* ((interval (/ pitch-partition primary))
                     (list (loop for i
                                 from start
                                 to (+ start pitch-partition)
                                 by interval
                                 collect i)))                  
                 (butlast 
                         (mapcar #'round 
                                 (mapcar #'(lambda (x) (if (ratiop x)
                                                           (float x)
                                                                x)) list)))))

(defun mc-sym-div-desc (start pitch-partition primary)
               (let* ((interval (/ pitch-partition primary))
                    
                      (list (loop for i
                                 from (- start pitch-partition)
                                 to start 
                                 by interval
                                 collect i)))                                 
                 (butlast 
                  (reverse 
                   (mapcar #'round 
                           (mapcar #'(lambda (x) (if (ratiop x)
                                                     (float x)
                                                     x)) list))))))

(defun mc-pattern-a (start pitch-partition primary-lst bpf-lst)
       (cond ((null bpf-lst) nil) 
             ((= (car bpf-lst) -1)
              (append (list (car (mc-sym-div-desc start pitch-partition (car primary-lst)))) 
                       (permut-random (cdr (mc-sym-div-desc start pitch-partition (car primary-lst))))))
              (t (append (list (car (mc-sym-div-asc start pitch-partition (car primary-lst))))
                        (permut-random (cdr (mc-sym-div-asc start pitch-partition (car primary-lst))))))))


(defun mc-reverse-direction-if (sequence pitch-partition low high primary)
  (cond ((some #'(lambda (x) (> x high)) (cdr sequence))
   
           (mc-sym-div-desc (car sequence) pitch-partition primary))
        
        ((some #'(lambda (x) (< x low)) (cdr sequence))
            (mc-sym-div-asc (car sequence) pitch-partition primary))
      
        (t sequence)))



;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;  "pitch partition determines the range of the "octave"
;;;;;;;;;;;;;;;;;;;;  The "octave" or pitch-partition determines the region to be symmetrically divided
;;;;;;;;;;;;;;;;;;;  it could be 1100, 1300, or 1700 etc.

(om::defmethod! mc-create-pattern-1 (start pitch-partition low high primary-lst secondary bpf-lst) 
  :initvals '(6000 1200 4800 8400 '(2) 100 '(1))
  :indoc '("start" "pitch-partition"  "low" "high" "primary-lst" "secondary" "bpf-lst")
  :icon 132
  :doc "creates a mc-melodic sequence based on thesis rule"
  (let ((pattern (mc-pattern-a start pitch-partition primary-lst bpf-lst)))             
    (if (null bpf-lst)                 
        nil          
            (append (setf pattern 
                          (mc-reverse-direction-if pattern pitch-partition low high (car primary-lst)))   
                    (mc-create-pattern-1 
                     (if (= (car bpf-lst) -1)
                         (- (car (reverse pattern)) secondary)
                         (+ secondary (car (reverse pattern))))                                      
                     pitch-partition low high (cdr primary-lst) secondary (cdr bpf-lst))))))

;; Example #3 (while a student/with assistance from a mentor)

(defun sum-combos (max-addend sum)
  ; end the recursion at 1
  ; fill in with 1's up to sum

  (cond ((= max-addend 1)
         (list (make-list sum :initial-element 1)))
        ((= sum 0)
         '(()))
        (t
         (let ((combos (sum-combos (1- max-addend) sum)))
           (append combos
                 (when (>= sum max-addend)
                   (loop for combo in (sum-combos max-addend (- sum max-addend))
                         collect (cons max-addend combo))))))))

;; Example #4 (while a student/with assistance from a mentor)

(defun our-choose (lis k)
  ;; this returns a list of subsets
  (cond ((> k (length lis))
         "this is impossible")

        ((= k (length lis))
         (list lis))   ;;; a list with one element, which is the input list


        ((= k 1)
         (loop for element in lis
               collect (list element)))  ;; e.g. '((a) (b) (c) (d))

       
        (t ;; the non-trivial case: k > 1 and k < length
           (let ((subsets-with-first-element (loop for inner-level in (our-choose (cdr lis) (- k 1))
                                                   collect (cons (first lis) inner-level)))

                 (subsets-without-first-element (our-choose (cdr lis) k)))

             (append subsets-with-first-element subsets-without-first-element)))))



