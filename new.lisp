(defvar *BLOCKS* '((RED A) (BLUE B) (GREEN C) (BLUE D) 
                     (GREEN E) (BLUE F) (GREEN G) (YELLOW H)
                     (RED I) (BLUE J) (RED K)))

(defvar *STRUCT-DESCR* '((ON-TABLE ?X1 0) (ON-TABLE ?X2 2) 
                           (ON-TABLE ?X3 4) (ON-TABLE ?X4 5) 
                           (ON-TABLE ?X5 7) (ON-TABLE ?X6 9) 
                           (ASTRIDE ?Y1 ?X2 ?X3) (ON ?Y2 ?X4) 
                           (ASTRIDE ?Y3 ?X5 ?X6) 
                           (ASTRIDE ?Z1 ?Y1 ?Y2) (ON ?Z2 ?Y2)
                           (RED ?X1) (BLUE ?X2)))

(defvar *FACTS-HT* (make-hash-table :test #'equal))

;;; Type check functions

(defun is-ht (x)
    (typep x 'hash-table))

(defun is-pred (x)
    (and (listp x) (atom (car x))))

(defun is-pred-list (x)
    (and (listp x) (every #'is-pred x)))

(defun is-color (x)
   (and (is-pred x) (or
                     (eq 'RED (car x)) (eq 'GREEN (car x)) 
                     (eq 'BLUE (car x)) (eq 'YELLOW (car x)))))

(defun check-types (types &rest args)
		(every
				(lambda (pred arg)
				(if (funcall (eval `(lambda (x) (,pred x))) arg) t
						nil))
				types args)) 

(defun is-variable (x)
		(and (not (numberp x)) (eq (char (string x) 0) #\?)))

;;; Part 1

;;; Takes a single fact and a hashtable, then store the fact in the hashtable
(defun store-fact (fact ht)
   (unless (check-types '(is-pred is-ht) fact ht)
				(return-from store-fact '**TYPE-ERROR**))
   ;; when using the entire predication as a key
   ;; just store T in the hash table
   (setf (gethash fact ht) fact)
   ;; when using the predicate as a key
   ;; remove predicate
   (setf (gethash (car fact) ht) 
         (remove nil (remove-duplicates (append (gethash (car fact) ht) 
                                                (list fact))
                      :test #'equal))))

;;; Takes a single fact and a hashtable, then remove the fact from hashtable
(defun remove-fact (fact ht)
   (unless (check-types '(is-pred is-ht) fact ht)
				(return-from remove-fact '**TYPE-ERROR**))
   ;; remove the entire predication
   (remhash fact ht)
   ;; remove the machted predication in the list
   (setf (gethash (car fact) ht) (remove fact (gethash (car fact) ht) 
                                  :test #'equal)))


;;; Part 2

;;; Takes a list of facts and a hashtable,
;;; then store all the fact in the list into the hashtable
(defun store-facts (facts ht)
   (unless (check-types '(is-pred-list is-ht) facts ht)
				(return-from store-facts '**TYPE-ERROR**))
   (mapcar #'(lambda (x) (store-fact x ht)) facts))                      
                     

;;; Part 3

;;;declaration of useful variables
(defvar *SORTED-DESCR* nil) ; list of sorted facts in structure description
(defvar *VARIABLES* nil) ; variables mentioned in the structure description
(defvar *BLOCKS-TEMP* nil) ; temporary list of blocks for tracking block usage
(defvar *PAIRS* nil) ; result of binding
(defvar *COLOR-LIST* nil) ; temporary list of blocks with given color

;;; Takes a single fact, 
;;; then add to front of *SORTED-DESCR* if the fact is about color
;;; add to the back otherwise
(defun sort-description (descr res)
   (let ((temp res))
      (cond ((equal (car descr) 'ON-TABLE) (setq temp 
                                                 (append temp (list descr))))
         ((equal (car descr) 'ON) (setq temp 
                                             (append temp (list descr))))
         ((equal (car descr) 'ASTRIDE) (setq temp 
                                              (append temp (list descr))))
         (t (setq temp (append (list descr) temp))))
      temp))

;;; Sort the given list of facts so that facts about color are at the front
(defun sort-descriptions (descr-list)
   (let ((res nil))
      (mapcar #'(lambda (x) (sort-description x res) ) descr-list)
      res))

;;; extrat any new variable in the given fact, 
;;; then add to the list of variables
(defun extract-variable (descr vars)
   (cond ((equal (car descr) 'ON-TABLE) (setq vars 
                                                 (append vars (list (cadr descr)))))
         ((equal (car descr) 'ON) (setq vars 
                                           (union vars (cdr descr))))
         ((equal (car descr) 'ASTRIDE) (setq vars 
                                                 (union vars (cdr descr))))
         (t (setq vars (append vars (list (cadr descr)))))))

;;; extract all the new variable in the list of fact.
;;; then add to the list of variables
(defun extract-variables (descr-list)
   (let ((res nil))
      (mapcar #'(lambda (x) (extract-variable x res)) descr-list)
      (setq res (remove-duplicates res :test #'equal))
      res))


;;; takes a list of variabls and match assign a blovk to them if new.
;;; then add the pair to the list of pairs
(defun match (vars pairs var-list blocks)
   (cond ((equal (car vars) nil) nil)
         ((equal (member (car vars) var-list) nil) (match (cdr vars) pairs var-list blocks))
         (t 
            (cond ((equal blocks nil) (error "No enought blocks for variable"))
                  (t
                     (setq pairs (append pairs 
                                             (list (list (car vars) (cadr (car blocks)))))) 
                     (setq blocks (cdr blocks))
                     (setq var-list (remove (car vars) var-list))
                     (match (cdr vars) pairs var-list blocks))))))

;;; function match with specified color
(defun match-color (descr pairs var-list blocks)
   (let ((block-list (gethash (car descr) *FACTS-HT*)))
      (cond ((equal (intersection block-list blocks) nil) (error "No enought blocks"))
         (t
            (setq pairs (append pairs 
                                  (list (list (cadr descr) (cadr (car block-list)))))) 
            (setq blocks (remove (car block-list) blocks :test #'equal))
            (setq var-list 
                  (remove (cadr descr) var-list :test #'equal))))))

;;; takes a structure description and store the result in *PAIRS*
(defun find-required-blocks (descr)
   (unless (check-types '(is-pred-list) descr)
				(return-from find-required-blocks '**TYPE-ERROR**))
   (let ((des (car (sort-descriptions descr))) (res nil) (var-list (extract-variables descr)) (blocks *BLOCKS*))
      (cond ((equal des nil) nil)
            ((equal (car des) 'ON-TABLE) (match (list (cadr des)) res var-list blocks) 
                                          (find-required-blocks (cdr descr)))
            ((equal (car des) 'ON) (match (cdr des) res var-list blocks) 
                                    (find-required-blocks (cdr descr)))
            ((equal (car des) 'ASTRIDE) (match (cdr des) res var-list blocks) 
                                         (find-required-blocks (cdr descr)))
            (t (match-color des res var-list blocks) (find-required-blocks (cdr descr))))
      res))


;;; Part 4
;; temporary variable for demonstration of part 4
(defvar *STRUCTURE* nil) 
(setq *STRUCTURE* *STRUCT-DESCR*)

;;; takes a structure description and a binding list,
;;; then replace the variables in the structure description according to binding list
(defun particular-description (struc-descr binding-list)
   (unless (check-types '(is-pred-list is-pred-list) struc-descr binding-list)
				(return-from particular-description '**TYPE-ERROR**))
   (cond ((equal binding-list nil) struc-descr)
         (t 
            (mapcar #'(lambda (x) (nsubst (cadr (car binding-list)) 
                                          (car (car binding-list)) x :test #'equal)) struc-descr)
            (particular-description struc-descr (cdr binding-list)))))


;;; Part 5

(defvar *LAYERS* nil)
;; reset the value of test structure description
(setq *STRUCT-DESCR* '((ON-TABLE ?X1 0) (ON-TABLE ?X2 2) 
                        (ON-TABLE ?X3 4) (ON-TABLE ?X4 5) 
                        (ON-TABLE ?X5 7) (ON-TABLE ?X6 9) 
                        (ASTRIDE ?Y1 ?X2 ?X3) (ON ?Y2 ?X4) 
                        (ASTRIDE ?Y3 ?X5 ?X6) (ASTRIDE ?Z1 ?Y1 ?Y2)
                        (ON ?Z2 ?Y2) (RED ?X1) (BLUE ?X2)))

;; a hashtable that store every block by the hash of any block underneath
(defvar *STRUCT-HT* (make-hash-table :test #'equal))

;;; Takes a structure description
;;; then store every variable in the previous hashtable by their relative position
(defun hash-struct (struct-descr)
   (let ((struct-ht (make-hash-table :test #'equal)))
      (cond ((equal struct-descr nil) nil)
         (t 
            (cond ((equal (car (car struct-descr)) 'ON-TABLE) 
                     (setf (gethash 'ON-TABLE struct-ht) 
                           (remove-duplicates (append (gethash 'ON-TABLE struct-ht) 
                                                       (list (cadr (car struct-descr))))
                            :test #'equal)) 
                     (hash-struct (cdr struct-descr)))
                  ((equal (car (car struct-descr)) 'ON) 
                     (setf (gethash (caddr (car struct-descr)) struct-ht) 
                           (remove-duplicates (append (gethash (caddr (car struct-descr)) struct-ht) 
                                                       (list (cadr (car struct-descr)))) 
                            :test #'equal))
                     (hash-struct (cdr struct-descr)))
                  ((equal (car (car struct-descr)) 'ASTRIDE) 
                     (setf (gethash (caddr (car struct-descr)) struct-ht) 
                           (remove-duplicates (append (gethash (caddr (car struct-descr)) struct-ht) 
                                                      (list (cadr (car struct-descr)))) 
                            :test #'equal))
                     (setf (gethash (cadddr (car struct-descr)) struct-ht)
                           (remove-duplicates (append (gethash (cadddr (car struct-descr)) struct-ht)
                                                      (list (cadr (car struct-descr))))
                            :test #'equal))  
                     (hash-struct (cdr struct-descr)))
                  (t (hash-struct (cdr struct-descr))))))
         struct-ht))

;;; takes a list of blocks in certain layer
;;; returns the list of block in the upper layer
(defun get-next-layer (this-layer struct-ht)
   (let ((res nil))
      (mapcar #'(lambda (x) (setq res (union res (gethash x struct-ht)))) this-layer)
      res))

;;; takes a list of blocks in each layer
;;; throw error if detect inconsisitency
(defun check-validity (list-rest)
   (cond ((equal (cdr list-rest) nil) nil)
         (t (mapcar #'(lambda (x) (if (not (equal (intersection (car list-rest) x :test #'equal) nil)) 
                                      (error "Invalid structure description"))) (cdr list-rest)) 
            (check-validity (cdr list-rest)))))

;;; takes a structure description and return the list of blocks in each layer
(defun find-layers (struct-descr)
   (unless (check-types '(is-pred-list) struct-descr)
				(return-from find-layers '**TYPE-ERROR**))
   (let ((res nil) (struct-ht (hash-struct struct-descr)))
      (setq res (list (append res (gethash 'ON-TABLE struct-ht))))
      (loop until (equal (get-next-layer (car (last res)) struct-ht) nil)
         do(
            setq res (append res (list (get-next-layer (car (last res)) struct-ht)))))
      (check-validity res)
      res))
