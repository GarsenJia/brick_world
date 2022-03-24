(setq *BLOCKS* '((RED A) (BLUE B) (GREEN C) (BLUE D) 
                     (GREEN E) (BLUE F) (GREEN G) (YELLOW H)
                     (RED I) (BLUE J) (RED K)))

(setq *FACTS-HT* (make-hash-table :test #'equal))


(setq *STRUCT-DESCR* '((ON-TABLE ?X1 0) (ON-TABLE ?X2 2) 
                        (ON-TABLE ?X3 4) (ON-TABLE ?X4 5) 
                        (ON-TABLE ?X5 7) (ON-TABLE ?X6 9) 
                        (ASTRIDE ?Y1 ?X2 ?X3) (ON ?Y2 ?X4) 
                        (ASTRIDE ?Y3 ?X5 ?X6) (ASTRIDE ?Z1 ?Y1 ?Y2)
                        (ON ?Z2 ?Y2) (RED ?X1) (BLUE ?X2)))

(write "Demonstration of part 1&2")
(terpri)
(write "store-facts:")
(store-facts *BLOCKS* *FACTS-HT*)
(write *FACTS-HT*)
(terpri)
(write "remove-fact:")
(remove-fact '("GREEN" "E") *FACTS-HT*)
(write *FACTS-HT*)
(terpri)
(write "store-fact:")
(store-fact '("GREEN" "E") *FACTS-HT*)
(write *FACTS-HT*)
(terpri)


(setq *BLOCKS-TEMP* *BLOCKS*)
(sort-descriptions *STRUCT-DESCR*)
(extract-variables *SORTED-DESCR*)


(find-required-blocks *SORTED-DESCR*)
(write "Demonstration of part 3")
(terpri)
(write *PAIRS*)
(terpri)



(particular-description *STRUCTURE* *PAIRS*)
(write "Demonstration of part 4")
(terpri)
(write *STRUCTURE*)
(terpri)


(setq *STRUCT-DESCR* '((ON-TABLE ?X1 0) (ON-TABLE ?X2 2) 
                        (ON-TABLE ?X3 4) (ON-TABLE ?X4 5) 
                        (ON-TABLE ?X5 7) (ON-TABLE ?X6 9) 
                        (ASTRIDE ?Y1 ?X2 ?X3) (ON ?Y2 ?X4) 
                        (ASTRIDE ?Y3 ?X5 ?X6) (ASTRIDE ?Z1 ?Y1 ?Y2)
                        (ON ?Z2 ?Y2) (RED ?X1) (BLUE ?X2)))

(write "Demonstration of part 5")
(terpri)
(write (find-layers *STRUCT-DESCR*))
(terpri)
(setq *STRUCT-HT* (clrhash *STRUCT-HT*))