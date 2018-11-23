(in-package :user)

(defstruct csp
  variables
  max_lim
  not_assigned
  assignments
  )

(defun csp-inicial(problema)
  (let ((var_values (make-hash-table)) (t_end NIL) (t_init NIL) (t_tarefa NIL) )
    (dotimes (i (list-length problema))
      (let  ((el (elt problema i))) 
      
      (let(
            (a (make-array '(1 4) :initial-contents (list el)))
 
           )
       ( setq t_end (aref a 0 3))
       (setq t_init (aref a 0 2))
       (setq t_tarefa (- t_end t_init))
       (setf (gethash a var_values) t_tarefa)
        )
      
      
        ))
  
  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) var_values)   
  
  (let ((ini
  (make-csp       :variables var_values
            :max_lim 480 
            :not_assigned problema
            :assignments ())))
                 
    ini)
   )
)
(defun operadores(estado)
  
  (let ((suc_all NIL))
    (dotimes (i (list-length csp-assignments estado))
      ;; Se nao comecar em L1
      (if (not(equal((car(elt 0 (estado i))) 'L1)))
          ()
      )
     
      
    )
         
        
    )  
  )
)


(defun custo(estado)
)

(defun no-overlap-constraints(estado)
    
  )

(defun objectivo(estado)
  )


(defun duracao_tarefa()
  )

(defun heuristica(estado)
  )

(defun faz-afectacao(problema)
)