(defvar *BLOCKS-LOCS-HT* (make-hash-table :test #'equal))

;;; Task 1

(defun put-on-table (x i)
    (unless (numberp i)
                (return-from put-on-table '**TYPE-ERROR**))
    (cond   ((< i 0) (error "Invalid specified cordinate"))
            ((not (equal (gethash (list i 0) *BLOCKS-LOCS-HT*) nil))
                (error "Specified cordinate not available"))
            (t (setf (gethash (list i 0) *BLOCKS-LOCS-HT*) x) (setf (gethash x *BLOCKS-LOCS-HT*) (list i 0)))))

(defun put-on (x y)
    (cond ((equal (gethash y
     *BLOCKS-LOCS-HT*) nil) (error "Invalid base"))
          ((not (equal (gethash (list (car (gethash y *BLOCKS-LOCS-HT*)) 
                                      (+ (cadr (gethash y *BLOCKS-LOCS-HT*)) 1)) *BLOCKS-LOCS-HT*) nil)) 
            (error "Cordinate on the specified base not available"))
          (t (setf (gethash (list (car (gethash y *BLOCKS-LOCS-HT*))
                                  (+ (cadr (gethash y *BLOCKS-LOCS-HT*)) 1)) *BLOCKS-LOCS-HT*) x)
            (setf (gethash x *BLOCKS-LOCS-HT*) (list (car (gethash y *BLOCKS-LOCS-HT*)) 
                                                     (+ (cadr (gethash y *BLOCKS-LOCS-HT*)) 1))))))

(defun put-astride (x y z)
    (cond ((equal (gethash y *BLOCKS-LOCS-HT*) nil) (error "Invalid base"))
          ((equal (gethash z *BLOCKS-LOCS-HT*) nil) (error "Invalid base"))
          ((not (equal (list (+ (car (gethash y *BLOCKS-LOCS-HT*)) 1) 
                                 (+ (cadr (gethash y *BLOCKS-LOCS-HT*)) 1))
                  (list (- (car (gethash z *BLOCKS-LOCS-HT*)) 1) 
                                 (+ (cadr (gethash z *BLOCKS-LOCS-HT*)) 1))))
            (error "Invalid base combination"))
          ((not (equal (gethash (list (+ (car (gethash y *BLOCKS-LOCS-HT*)) 1) 
                                      (+ (cadr (gethash y *BLOCKS-LOCS-HT*)) 1)) *BLOCKS-LOCS-HT*) nil))
            (error "Cordinate on the specified base not available"))
          ((not (equal (gethash (list (- (car (gethash z *BLOCKS-LOCS-HT*)) 1)
                                      (+ (cadr (gethash z *BLOCKS-LOCS-HT*)) 1)) *BLOCKS-LOCS-HT*) nil))
            (error "Cordinate on the specified base not available"))
          (t (setf (gethash (list (+ (car (gethash y *BLOCKS-LOCS-HT*)) 1) 
                                      (+ (cadr (gethash y *BLOCKS-LOCS-HT*)) 1)) *BLOCKS-LOCS-HT*) x)
             (setf (gethash x *BLOCKS-LOCS-HT*) (list (+ (car (gethash y *BLOCKS-LOCS-HT*)) 1) 
                                      (+ (cadr (gethash y *BLOCKS-LOCS-HT*)) 1))))))


;;; Task 2
(defvar *LOCS-HT* (make-hash-table :test #'equal))
(defun hash-struc-locs (specific-struc-descr)
  (let ((ht (make-hash-table :test #'equal)))
    (cond ((equal specific-struc-descr nil) nil)
          (t 
            (cond ((equal (car (car specific-struc-descr)) 'ON-TABLE) 
                     (setf (gethash (cadr (car specific-struc-descr)) *LOCS-HT*) 
                           (remove-duplicates (append (gethash (cadr (car specific-struc-descr)) *LOCS-HT*) 
                                                       (list (caddr (car specific-struc-descr))))
                            :test #'equal)) 
                     (hash-struc-locs (cdr specific-struc-descr)))
                  ((equal (car (car specific-struc-descr)) 'ON) 
                     (setf (gethash (cadr (car specific-struc-descr)) *LOCS-HT*) 
                           (remove-duplicates (append (gethash (cadr (car specific-struc-descr)) *LOCS-HT*) 
                                                       (list (caddr (car specific-struc-descr)))) 
                            :test #'equal))
                     (hash-struc-locs (cdr specific-struc-descr)))
                  ((equal (car (car specific-struc-descr)) 'ASTRIDE) 
                     (setf (gethash (cadr (car specific-struc-descr)) *LOCS-HT*) 
                           (remove-duplicates (append (gethash (cadr (car specific-struc-descr)) *LOCS-HT*) 
                                                      (list (caddr (car specific-struc-descr)) (cadddr (car specific-struc-descr))))
                            :test #'equal))  
                     (hash-struc-locs (cdr specific-struc-descr)))
                  (t (hash-struc-locs (cdr specific-struc-descr))))))
    *LOCS-HT*))

(defun build-block (bloc ht)
  (let ((res nil))
    (cond ((equal (gethash bloc ht) nil) (error "Invalid structure description"))
          ((numberp (car (gethash bloc ht))) (setq res (list 'PUT-ON-TABLE bloc (car (gethash bloc ht)))))
          ((equal (length (gethash bloc ht)) 1) (setq res (list 'PUT-ON bloc (car (gethash bloc ht)))))
          ((equal (length (gethash bloc ht)) 2) (setq res (list 'PUT-ASTRIDE bloc (car (gethash bloc ht)) (cadr (gethash bloc ht))))))
    res))

(defun flatten (lst)
  (cond ((null lst) nil)
        ((atom (car lst)) lst)
        (t (append (flatten (car lst))
                    (flatten (cdr lst))))))

(defvar *ANSWER* nil)

  (defun grp (lst)
    (cond ((null lst) nil)
          ((equal (car lst) 'PUT-ASTRIDE) (setq *ANSWER* (append *ANSWER* (list (list (car lst) (cadr lst) (caddr lst) (cadddr lst))))) 
                                          (grp (cddddr lst)))
          (t (setq *ANSWER* (append *ANSWER* (list (list (car lst) (cadr lst) (caddr lst))))) 
                                          (grp (cdddr lst)))))



(defun find-steps (specific-struc-descr)
  (let ((locs (hash-struc-locs specific-struc-descr)) (layers (find-layers *STRUCTURE*)) (res nil))
    (write locs)
    (write layers)
    (setq res
      (loop for x in layers
      collect (loop for y in x
                collect (build-block y locs))))
    (grp (flatten res)))
    *ANSWER*)

;;; Part 3

(defun build-goal-structure (nonspecific-struc-descr)
  (let ((pairs (find-required-blocks nonspecific-struc-descr)) (specific nonspecific-struc-descr) (res nil))
    (write "following blocks will be used")
    (terpri)
    (write *PAIRS*)
    (terpri)
    (particular-description specific pairs)
    (setq res (find-steps specific))
    res))
