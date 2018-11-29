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
  assignments
  )

(defun csp-inicial(problema)
  (setf problema (qsort problema))

  (let ((ini
  (make-csp :variables problema
            :assignments nil)))
                 
    ini)
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
  (equalp estado_inicial estado_final)
)

;;; successor generation
(defun successors(state)
	(let ((children nil) (task (nth 0 (csp-variables state))) (vars (csp-variables state)) (shifts (csp-assignments state)))
		(cond ((eq shifts nil)
				(let* ((new_state (make-csp :variables (remove task (copy-list vars))
										    :assignments (list (list (copy-list task)))))
					   (valid (constrictions new_state))
					  )

					(cond ( (eq valid T) (setf children (list new_state)) ) )
				)
			  )
			  (t
				(let* ((new_shifts (copy-list shifts))
					   (new_shift (append (nth (- (list-length new_shifts) 1) new_shifts) (list (copy-list task))))
					  )
					(setf (nth (- (list-length new_shifts) 1) new_shifts) new_shift)
					(let* ((new_state (make-csp :variables (remove task (copy-list vars))
												:assignments new_shifts))
						   (valid (constrictions new_state))
						  )
						(cond ( (and (eq valid T) (eq children nil)) (setf children (list new_state)) )
							  ( (and (eq valid T) (not (eq children nil))) (nconc children (list new_state)) )
						)
					)
				)
				(let* ((new_shifts (append (copy-list shifts) (list (list(copy-list task)))))
					   (new_state (make-csp :variables (remove task (copy-list vars))
											:assignments new_shifts))
					   (valid (constrictions new_state))
					  )

					(cond ( (and (eq valid T) (eq children nil)) (setf children (list new_state)) )
						  ( (and (eq valid T) (not (eq children nil))) (nconc children (list new_state)) )
					)
				)
			  )
		)
		(print children)
		(print 'cry)
		children
	)
)

;;; constriction checks
 ;;shift is 8 hours long at most;
 ;;there is an obligatory meal break at the 4-hour mark at the latest, and no more than one (40 mins long);
 ;;there is a transportation block (40 mins) where it is necessary to move to another station for the next task.
(defun shift-validity(shift)
	(let ((time-count 0) (start-task (nth 0 shift)) (last-task (nth (- (list-length shift) 1) shift)) (valid T))
		(cond ( (not (eq 'L1 (nth 0 start-task))) (setf time-count (+ time-count 40)) ) )
		(setf time-count (+ time-count (- (nth 3 start-task) (nth 2 start-task))))
		(dotimes (i (- (list-length shift) 1))
			(let ((task (nth i shift)) (next-task (nth (+ i 1) shift)) (old-time time-count))
				(setf time-count (+ time-count (- (nth 3 next-task) (nth 2 next-task))))
				(cond ((not (eq (nth 1 task) (nth 0 next-task)))
							(cond ( (< (- (nth 2 next-task) (nth 3 task)) 40) (setf valid nil) ) )
							(cond ((and (<= old-time 240) (> time-count 240))
										(cond ( (< (- (nth 2 next-task) (nth 3 task)) 80) (setf valid nil) ) )
								  )
							)
					  )
					  (t
							(cond ((and (<= old-time 240) (> time-count 240))
										(cond ( (< (- (nth 2 next-task) (nth 3 task)) 40) (setf valid nil) ) )
								  )
							)
					  )
				)
				(setf time-count (+ time-count (- (nth 2 next-task) (nth 3 task))))
				(cond ( (> time-count 480) (setf valid nil) ) )
			)
		)
		(cond ( (not (eq 'L1 (nth 1 last-task))) (setf time-count (+ time-count 40)) ) )
		(cond ( (> time-count 480) (setf valid nil) ) )
		valid
	)
)

(defun constrictions(state)
	(let ((valid T)
		  (shifts (csp-assignments state))
		 )
		(dotimes (i (list-length shifts))
			(setf valid (shift-validity (nth i shifts)))
		)
		valid
	)
)

(defun heuristica(estado)
  (list-length (csp-assignments estado))
)

(defun faz-afectacao(problema tipo-procura)
  (let ((csp NIL) (problema NIL))
    (setf csp (csp-inicial problema))
    (setf problema (cria-problema csp (list #'successors) :custo #'custo :heuristica #'heuristica)) 
    (procura problema tipo-procura)
  )
)

(setf a (csp-inicial '((L2 L1 1 25) (L1 L2 34 60) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 565))))
(setf b (successors a))
(setf c (nth 0 b))
(setf d (successors c))
(setf c (nth 0 d))
(setf d (successors c))
(setf c (nth 0 d))
(setf d (successors c))
(setf c (nth 0 d))
(setf d (successors c))
(setf o_state (make-csp :variables () :assignments '(((L2 L1 1 25) (L1 L2 34 60)) ((L5 L1 408 447) (L1 L1 448 551)) ((L1 L1 474
565)))))