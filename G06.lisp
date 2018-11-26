(in-package :user)


(defun qsort (L)
  (cond
    ((null L) nil)
    (t
      (append
        (qsort (list< (car L) (cdr L)))
        (cons (car L) nil) 
        (qsort (list>= (car L) (cdr L)))))))

(defun list< (a b)
  (cond
    ((or (null a) (null b)) nil)
    ((< (elt a 2) (elt (car b) 2)) (list< a (cdr b)))
    (t (cons (car b) (list< a (cdr b))))))

(defun list>= (a b)
  (cond
    ((or (null a) (null b)) nil)
    ((>= (elt a 2) (elt (car b) 2)) (list>= a (cdr b)))
   (t (cons (car b) (list>= a (cdr b))))))


(defstruct csp
  variables
  max_lim
  assignments
  )

(defun csp-inicial(problema)
  (setf problema (qsort problema))
  (let ((var_values NIL) (t_end NIL) (t_init NIL) (t_tarefa NIL) (tarefa NIL)) 
    
    (dotimes (i (list-length problema))      
      (let(
            (tarefa (elt problema i))
 
           )
       (setq t_end (elt tarefa 3))
       (setq t_init (elt tarefa 2))
       (setq t_tarefa (- t_end t_init))
       (print tarefa)
       (setf var_values (append var_values (list (cons tarefa (list t_tarefa))))
        )
      
      
        ))
    
  (let ((ini
  (make-csp       :variables var_values
            :max_lim 480 
            :assignments (make-hash-table))))
                 
    ini)
   )
  )


(defun duracao_tarefa(tarefa)
  
  (setq t_end (aref a 0 3))
  (setq t_init (aref a 0 2))
  (setq t_tarefa (- t_end t_init))
  t_tarefa
      
)


(defun custo(estado)
  (let ((custo 0))
    (loop for custo_turno being the hash-values of (csp-assignments estado)
               do (setq custo (+ custo custo_turno))
   ) 
custo )  )


(defun objectivo(estado) 
  (if (equal (csp-variables estado) NIL) T)
  NIL
)
  
(defun estado(estado_inicial estado_final)
  (equalp estado_inicial estado_final))


(defun heuristica(estado)
  )

(defun faz-afectacao(problema)
)