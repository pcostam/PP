(in-package :user)

(defstruct csp
  variables
  max_lim
  assignments
  )

(defun csp-inicial(problema)
  (let ((var_values (make-hash-table)) (t_end NIL) (t_init NIL) (t_tarefa NIL) (tarefa NIL))
    (dotimes (i (list-length problema))
      (let  ((el (elt problema i))) 
      
      (let(
            (tarefa (make-array '(1 4) :initial-contents (list el)))
 
           )
       ( setq t_end (aref tarefa 0 3))
       (setq t_init (aref tarefa 0 2))
       (setq t_tarefa (- t_end t_init))
       (setf (gethash tarefa var_values) t_tarefa)
        )
      
      
        ))
  
  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) var_values)   
  
  (let ((ini
  (make-csp       :variables var_values
            :max_lim 480 
            :assignments (make-hash-table))))
                 
    ini)
   )
  )

(defun fazer-refeicao(tarefa)
  (let ((nova_tarefa NIL) (novo_t_end 0 ) (novo_t_inicio 0) (tarefas NIL) (t_init (elt tarefa 2))
        (t_tarefa 0)  (t_end (elt tarefa 3)))
                                                                                  
                                                                                  
    (setq t_tarefa (- t_end t_init))
    (setq novo_t_end (+ t_init 240))
    (setf nova_tarefa tarefa)
    (setf (elt nova_tarefa 3) novo_t_end)
    
    (setq tarefas (append tarefas nova_tarefa))
    (write nova_tarefa)
    
    (setq novo_t_inicio (+ novo_t_end 40))
    (setq novo_t_end (+ novo_t_inicio (- t_tarefa 4) ))
    (setf nova_tarefa tarefa)
    (setf (elt nova_tarefa 3) novo_t_end)
    (setf (elt nova_tarefa 2) novo_t_inicio)
    (write nova_tarefa)
    (setq tarefas (append tarefas nova_tarefa))
    
    

    )
  

)                   

(defun duracao_tarefa(tarefa)
  
  (setq t_end (aref a 0 3))
  (setq t_init (aref a 0 2))
  (setq t_tarefa (- t_end t_init))
  t_tarefa
      
)


(defun operadores(estado)
      (let ((suc_all NIL) (tarefa NIL) (custo_tarefa NIL) (turno NIL) )
        (dotimes (var_idx (list-length (csp-variables estado)))
         (setf tarefa (elt (csp-variable estado) var_idx))
         (loop for custo being the hash-values of (csp-assignments estado)
             using (hash-key turno)
               
             do (format t "~&~A -> ~A" key value
                  (if (equal(hash-table-size (csp-assignments estado)) 0 )
                  (
                
                   )))
               (if (< custo 8)
              (
               (setf (gethash turno (csp-assignments estado)) custo_tarefa)
               (setf (gethash turno (csp-assignments estado)) (gethash turno (csp-assignments estado))     
               (remhash turno (csp-assignments estado))
                 )
               
               (  ;; Se nao comecar em L1
        
                   (setf custo_tarefa (duracao_tarefa tarefa))
                   (if (not(equal(elt (elt (csp-variable estado) var_idx) 0) 'L1))
                   (   
                        (setf custo_tarefa (+ custo_tarefa 40))
                        (setf (gethash tarefa (csp-assignments estado)) custo_tarefa))) 
                 
                 
           ))
          
            
        
            
          )
     
     )
      
     
  
       
      
    )
      

   
      
 
       
      
)
         



(defun custo(estado)
)


(defun objectivo(estado)
  )



(defun heuristica(estado)
  )

(defun faz-afectacao(problema)
)