;;; File: sjia6.lisp							
;;; Assignment: CSC244 Lisp Assignment 1					  
;;; Creator: SHENGYI JIA

;;; Test case initialization
(defvar *BLOCKS* '((RED A) (BLUE B) (GREEN C) (BLUE D) 
                     (GREEN E) (BLUE F) (GREEN G) (YELLOW H)
                     (RED I) (BLUE J) (RED K)))

(defvar *FACTS-HT* (make-hash-table :test #'equal))

(defvar *STRUCT-DESCR* '((ON-TABLE ?X1 0) (ON-TABLE ?X2 2) 
                           (ON-TABLE ?X3 4) (ON-TABLE ?X4 5) 
                           (ON-TABLE ?X5 7) (ON-TABLE ?X6 9) 
                           (ASTRIDE ?Y1 ?X2 ?X3) (ON ?Y2 ?X4) 
                           (ASTRIDE ?Y3 ?X5 ?X6) 
                           (ASTRIDE ?Z1 ?Y1 ?Y2) (ON ?Z2 ?Y2)
                           (RED ?X1) (BLUE ?X2)))

;;; Part 1

;;; Takes a single fact and a hashtable, then store the fact in the hashtable
(defun store-fact (fact ht)
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
   ;; remove the entire predication
   (remhash fact ht)
   ;; remove the machted predication in the list
   (setf (gethash (car fact) ht) (remove fact (gethash (car fact) ht) 
                                  :test #'equal)))


;;; Part 2

;;; Takes a list of facts and a hashtable,
;;; then store all the fact in the list into the hashtable
(defun store-facts (facts ht)
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
(defun sort-description (descr)
   (cond ((equal (car descr) 'ON-TABLE) (setq *SORTED-DESCR* 
                                                 (append *SORTED-DESCR* (list descr))))
         ((equal (car descr) 'ON) (setq *SORTED-DESCR* 
                                             (append *SORTED-DESCR* (list descr))))
         ((equal (car descr) 'ASTRIDE) (setq *SORTED-DESCR* 
                                              (append *SORTED-DESCR* (list descr))))
         (t (setq *SORTED-DESCR* (append (list descr) *SORTED-DESCR*)))))

;;; Sort the given list of facts so that facts about color are at the front
(defun sort-descriptions (descr-list)
   (mapcar #'(lambda (x) (sort-description x)) descr-list))

;;; extrat any new variable in the given fact, 
;;; then add to the list of variables
(defun extract-variable (descr)
   (cond ((equal (car descr) 'ON-TABLE) (setq *VARIABLES* 
                                                 (append *VARIABLES* (list (cadr descr)))))
         ((equal (car descr) 'ON) (setq *VARIABLES* 
                                           (union *VARIABLES* (cdr descr))))
         ((equal (car descr) 'ASTRIDE) (setq *VARIABLES* 
                                                 (union *VARIABLES* (cdr descr))))
         (t (setq *VARIABLES* (append *VARIABLES* (list (cadr descr)))))))

;;; extract all the new variable in the list of fact.
;;; then add to the list of variables
(defun extract-variables (descr-list)
   (mapcar #'(lambda (x) (extract-variable x)) descr-list)
   (setq *VARIABLES* (remove-duplicates *VARIABLES* :test #'equal)))


;;; takes a list of variabls and match assign a blovk to them if new.
;;; then add the pair to the list of pairs
(defun match (vars)
   (cond ((equal (car vars) nil) nil)
         ((equal (member (car vars) *VARIABLES*) nil) (match (cdr vars)))
         (t 
            (cond ((equal *BLOCKS-TEMP* nil) (error "No enought blocks for variable"))
                  (t
                     (setq *PAIRS* (append *PAIRS* 
                                             (list (list (car vars) (cadr (car *BLOCKS-TEMP*)))))) 
                     (setq *BLOCKS-TEMP* (cdr *BLOCKS-TEMP*))
                     (setq *VARIABLES* (remove (car vars) *VARIABLES*))
                     (match (cdr vars)))))))

;;; function match with specified color
(defun match-color (descr)
   (let ((block-list (gethash (car descr) *FACTS-HT*)))
      (cond ((equal (intersection block-list *BLOCKS-TEMP*) nil) (error "No enought blocks"))
         (t
            (setq *PAIRS* (append *PAIRS* 
                                  (list (list (cadr descr) (cadr (car block-list)))))) 
            (setq *BLOCKS-TEMP* (remove (car block-list) *BLOCKS-TEMP* :test #'equal))
            (setq *VARIABLES* 
                  (remove (cadr descr) *VARIABLES* :test #'equal))))))

;;; takes a structure description and store the result in *PAIRS*
(defun find-required-blocks (descr)
   (let ((des (car descr)))
      (cond ((equal des nil) nil)
            ((equal (car des) 'ON-TABLE) (match (list (cadr des))) 
                                          (find-required-blocks (cdr descr)))
            ((equal (car des) 'ON) (match (cdr des)) 
                                    (find-required-blocks (cdr descr)))
            ((equal (car des) 'ASTRIDE) (match (cdr des)) 
                                         (find-required-blocks (cdr descr)))
            (t (match-color des) (find-required-blocks (cdr descr))))))


;;; Part 4
;; temporary variable for demonstration of part 4
(defvar *STRUCTURE* nil) 
(setq *STRUCTURE* *STRUCT-DESCR*)

;;; takes a structure description and a binding list,
;;; then replace the variables in the structure description according to binding list
(defun particular-description (struc-descr binding-list)
   (cond ((equal binding-list nil) struc-descr)
         (t 
            (mapcar #'(lambda (x) (nsubst (cadr (car binding-list)) 
                                          (car (car binding-list)) x :test #'equal)) struc-descr)
            (particular-description struc-descr (cdr binding-list)))))


;;; Part 5

(defvar *LAYERS* nil)
;; reset the value of test structure description
(defvar *STRUCT-DESCR* '((ON-TABLE ?X1 0) (ON-TABLE ?X2 2) 
                           (ON-TABLE ?X3 4) (ON-TABLE ?X4 5) 
                           (ON-TABLE ?X5 7) (ON-TABLE ?X6 9) 
                           (ASTRIDE ?Y1 ?X2 ?X3) (ON ?Y2 ?X4) 
                           (ASTRIDE ?Y3 ?X5 ?X6) 
                           (ASTRIDE ?Z1 ?Y1 ?Y2) (ON ?Z2 ?Y2)
                           (RED ?X1) (BLUE ?X2)))

;; a hashtable that store every block by the hash of any block underneath
(defvar *STRUCT-HT* (make-hash-table :test #'equal))

;;; Takes a structure description
;;; then store every variable in the previous hashtable by their relative position
(defun hash-struct (struct-descr)
   (cond ((equal struct-descr nil) nil)
         (t 
            (cond ((equal (car (car struct-descr)) 'ON-TABLE) 
                     (setf (gethash 'ON-TABLE *STRUCT-HT*) 
                           (remove-duplicates (append (gethash 'ON-TABLE *STRUCT-HT*) 
                                                       (list (cadr (car struct-descr))))
                            :test #'equal)) 
                     (hash-struct (cdr struct-descr)))
                  ((equal (car (car struct-descr)) 'ON) 
                     (setf (gethash (caddr (car struct-descr)) *STRUCT-HT*) 
                           (remove-duplicates (append (gethash (caddr (car struct-descr)) *STRUCT-HT*) 
                                                       (list (cadr (car struct-descr)))) 
                            :test #'equal))
                     (hash-struct (cdr struct-descr)))
                  ((equal (car (car struct-descr)) 'ASTRIDE) 
                     (setf (gethash (caddr (car struct-descr)) *STRUCT-HT*) 
                           (remove-duplicates (append (gethash (caddr (car struct-descr)) *STRUCT-HT*) 
                                                      (list (cadr (car struct-descr)))) 
                            :test #'equal))
                     (setf (gethash (cadddr (car struct-descr)) *STRUCT-HT*)
                           (remove-duplicates (append (gethash (cadddr (car struct-descr)) *STRUCT-HT*)
                                                      (list (cadr (car struct-descr))))
                            :test #'equal))  
                     (hash-struct (cdr struct-descr)))
                  (t (hash-struct (cdr struct-descr)))))))

;;; takes a list of blocks in certain layer
;;; returns the list of block in the upper layer
(defun get-next-layer (this-layer)
   (let ((res nil))
      (mapcar #'(lambda (x) (setq res (union res (gethash x *STRUCT-HT*)))) this-layer)
      res))

;;; takes a list of blocks in each layer
;;; throw error if detect inconsisitency
(defun check-validity (list-rest)
   (write list-rest)
   (terpri)
   (cond ((equal (cdr list-rest) nil) nil)
         (t (mapcar #'(lambda (x) (if (not (equal (intersection (car list-rest) x :test #'equal) nil)) 
                                      (error "Invalid structure description"))) (cdr list-rest)) 
            (check-validity (cdr list-rest)))))

;;; takes a structure description and return the list of blocks in each layer
(defun find-layers (struct-descr)
   (let ((res nil))
      (hash-struct struct-descr)
      (setq res (list (append res (gethash 'ON-TABLE *STRUCT-HT*))))
      ;(write (get-next-layer (car (last res))))
      (loop until (equal (get-next-layer (car (last res))) nil)
         do(
            setq res (append res (list (get-next-layer (car (last res)))))))
      (check-validity res)
      res))
