;;;; -*- Mode: Lisp; -*- 

(DEFSTRUCT (person
            (:print-function print-person))
  (parent1 NIL) ; a symbol or string or NIL
  (parent2 NIL) ; a symbol or string or NIL
  (name NIL)
  (children NIL)); a symbol or string or NIL


(DEFUN print-person (item stream depth)
  "A helper function for Lispworks to be able to show you what is
in a person structure concisely."
    (DECLARE (IGNORE depth))
    (FORMAT stream "#<P name:~S p1:~S p2:~S>"
            (person-name item) (person-parent1 item) (person-parent2 item))
    item)



(DEFUN lookup-person (name tree)
  "Returns a PERSON structure corresponding to the key NAME in the hashtable TREE.
NAME must be a STRING or a SYMBOL. If there is no one in the tree with the name
in NAME, returns NIL."
  (GETHASH name tree nil))



(DEFUN person-exists (name tree)
  "Returns T when the key NAME has an actual person struct stored in TREE.
Returns NIL (false) otherwise."
  (WHEN (lookup-person name tree)
    t))



(DEFUN ancestors (name tree)
  "Returns a list of names (strings or symbols) of all the ancestors of NAME in TREE. 
Does not remove any duplicated names! Does not sort names! Does dynamic type checking
to see whether all the arguments are of the correct types."
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "ANCESTORS called with NAME (~A) that is not a SYMBOL or STRING." name))
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "ANCESTORS called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (person-exists name tree)
    (ancestorsb name tree)))

(DEFUN add-person (name struct tree)
  "This should enter the person structure in STRUCT into
the hashtable in TREE with the key in NAME."
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "STORE-PERSON called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (NOT (person-p struct))
    (ERROR "STORE-PERSON called with STRUCT (~A) that is not a PERSON structure." struct))
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "STORE-PERSON called with NAME (~A) that is not a SYMBOL or a STRING." name))
  ;; NOTE1: TEAMS NEED TO WRITE THE NEXT LINE.
  ;;        Hint: a "setf" expression.

  (setf (gethash name tree) struct)
  ;; NOTE2: Leave this last line as "name" so
  ;;        that the name argument is what is
  ;;        returned by this function.
  name)





(DEFUN ancestorsb (name tree)
(LET*((p (lookup-person name tree))
      (par1 (person-parent1 p))
      (par2 (person-parent2 p)))
  (when par1
    (append (list par1 par2)
            (ancestorsc par1 tree)
            (ancestorsc par2 tree))
  )
)
)

(DEFUN siblings (name tree)
(LET*((p (lookup-person name tree))
      (par1 (person-parent1 p))
      (par2 (person-parent2 p)))
   (remove-duplicates (list person-children par1 person-children par2) :test #'char-equal)
)
)



;;NOTE: This function needs to be defined by team   
(DEFUN handle-E (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (LET (
        (person1 (lookup-person (nth 0 linelist)tree))
        (person2 (lookup-person (nth 1 linelist)tree))
        (person3 (lookup-person (nth 2 linelist)tree))
        )
    (when person1
      (add-person (nth 0 linelist) (make-person :name (nth 0 linelist)  :parent1 nil :parent2 nil :children nil) tree))
    
    (when person2
      (add-person (nth 1 linelist) (make-person :name (nth 1 linelist)  :parent1 nil :parent2 nil :children nil) tree))

    (when person3
      (add-person (nth 2 linelist) (make-person :name (nth 2 linelist)  :parent1(nth 0 linelist) :parent2(nth 1 linelist) :children nil) tree)
      (append person-children person1 (nth 2 linelist))
      (append person-children person2 (nth 2 linelist))
    )
             
    
  ))


;;NOTE: This function needs to be defined by team
(DEFUN handle-X (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (LET ()
    ;;body of function goes here

    ))


;;NOTE: This function needs to be defined by team
(DEFUN handle-W (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (LET ()
    ;;body of function goes here

    ))




(DEFUN family (stream)
  "This is the top-level function for the whole Lisp program. Reads
each line from the file opened in STREAM."
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal))
        (line-items (SPLIT-SEQUENCE " " (READ-LINE stream nil "") :test #'equal)))
  (LOOP
    (CASE (FIRST line-items)
      ("E" (handle-E (REST line-items) tree))
      ("W" (handle-W (REST line-items) tree))
      ("X" (handle-X (REST line-items) tree))
      (t (RETURN nil))) ; end of file reached
    (SETF line-items (SPLIT-SEQUENCE " " (READ-LINE stream nil "") :test #'equal)))
  )
)

(DEFUN test-tree ()
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal)))
    (add-person "Zebulon" (make-person :name "Zebulon" :parent1 nil :parent2 nil :children (LIST "Mary")) tree)
    (add-person "Zenobia" (make-person :name "Zenobia" :parent1 nil :parent2 nil :children (LIST "Mary")) tree)
    (add-person "Fred" (make-person :name "Fred" :parent1 nil :parent2 nil :children nil) tree)
    (add-person "Mary" (make-person :name "Mary" :parent1 "Zebulon" :parent2 "Zenobia" :children (LIST "Karen" "Kelly" "Brenda")) tree)
    (add-person "Karen" (make-person :name "Karen" :parent1 "Fred" :parent2 "Mary" :children (LIST "Benjamin" "Alex")) tree)
    (add-person "Kelly" (make-person :name "Kelly" :parent1 "Fred" :parent2 "Mary" :children nil) tree)
    (add-person "Brenda" (make-person :name "Brenda" :parent1 "Fred" :parent2 "Mary":children nil) tree)
    (add-person "Bill" (make-person :name "Bill" :parent1 nil :parent2 nil :children (LIST "Benjamin" "Alex")) tree)
    (add-person "Benjamin" (make-person :name "Benjamin" :parent1 "Karen" :parent2 "Bill" :chilren nil) tree)
    (add-person "Alex" (make-person :name "Alex" :parent1 "Karen" :parent2 "Bill" :children nil) tree)
    ;;   ("Karen" "Bill" "Fred" "Mary" "Zebulon" "Zenobia")
    (ancestors "Alex" tree)))