;;;(require 'procura)
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
  no-service-counter
  shift-counter
  cost
  max-vars
  new_shift
  duracao_total
  num_vars
 )

(defun csp-inicial(problema)
  (setf problema (qsort problema))
  (let ((duracao 0))
  (dolist (tarefa problema)
	(setq duracao (+ duracao (duracao_tarefa tarefa)))
  )
  
  (let ((ini
  (make-csp :variables problema
            :assignments nil
			:no-service-counter 0
            :shift-counter 0
            :cost 0
			:max-vars (list-length problema)
			:new_shift 0
			:duracao_total duracao
			:num_vars 0)))
                 
    ini))
)

(defun custo(estado)
   (print 'CUSTO)
   (print (csp-cost estado))
   (csp-cost estado)
)


(defun objectivo(estado) 
  (let ((is_objective NIL))
    (if (equal (csp-variables estado) NIL) (setf is_objective T))
  is_objective
  )
)
  
(defun estado(estado_inicial estado_final)
  (equalp estado_inicial estado_final)
)

;;; successor generation
(defun successors(state)
	(let ((children nil) (task (nth 0 (csp-variables state))) (vars (csp-variables state)) (shifts (csp-assignments state)))
		(cond ((eq shifts nil)
				(let* ((new_state (make-csp :variables (remove task (copy-list vars))
										    :assignments (list (list (copy-list task)))
											:no-service-counter 0
											:shift-counter 1
											:cost (csp-cost state)
											:max-vars (csp-max-vars state)
											:new_shift 1
											:duracao_total (csp-duracao_total state)
											:num_vars (+ (csp-num_vars state) 1)))
					   (valid (constrictions new_state))
					  )

					(cond ( (eq valid T) (setf children (list new_state)) ) )
				)
			  )
			  (t
					(dotimes (i (csp-shift-counter state))
						(let* ((new_shifts (copy-list shifts))
							   (new_shift (append (nth i new_shifts) (list (copy-list task))))
							  )
							(setf (nth i new_shifts) new_shift)
							(let* ((new_state (make-csp :variables (remove task (copy-list vars))
														:assignments new_shifts
														:no-service-counter 0
														:shift-counter (csp-shift-counter state)
														:cost (csp-cost state)
														:max-vars (csp-max-vars state)
														:new_shift 0
														:duracao_total (csp-duracao_total state)
														:num_vars (+ (csp-num_vars state) 1)))
								   (valid (constrictions new_state))
								  )
								(cond ( (and (eq valid T) (eq children nil)) (setf children (list new_state)) )
									  ( (and (eq valid T) (not (eq children nil))) (nconc children (list new_state)) )
								)
							)
						)
					)
				(let* ((new_shifts (append (copy-list shifts) (list (list(copy-list task)))))
					   (new_state (make-csp :variables (remove task (copy-list vars))
											:assignments new_shifts
											:no-service-counter 0
											:shift-counter (+ (csp-shift-counter state) 1)
											:cost (csp-cost state)
											:max-vars (csp-max-vars state)
											:new_shift 1
											:duracao_total (csp-duracao_total state)
											:num_vars (+ (csp-num_vars state) 1)))
					   (valid (constrictions new_state))
					  )
					(cond ( (and (eq valid T) (eq children nil)) (setf children (list new_state)) )
						  ( (and (eq valid T) (not (eq children nil))) (nconc children (list new_state)) )
					)
				)
			  )
		)
		children
	)
)

;;; constriction checks
 ;;shift is 8 hours long at most;
 ;;there is an obligatory meal break at the 4-hour mark at the latest, and no more than one (40 mins long);
 ;;there is a transportation block (40 mins) where it is necessary to move to another station for the next task.
(defun shift-validity(shift state)
	(let ((time-count 0) (start-task (nth 0 shift)) (last-task (nth (- (list-length shift) 1) shift)) (valid T))
		(cond ((not (eq 'L1 (nth 0 start-task)))
                (setf time-count (+ time-count 40))
				(setf (csp-no-service-counter state) (+ (csp-no-service-counter state) 1))
			  )
		)

        (setf time-count (+ time-count (- (nth 3 start-task) (nth 2 start-task))))
		(dotimes (i (- (list-length shift) 1))
			(let ((task (nth i shift)) (next-task (nth (+ i 1) shift)) (old-time time-count))
                (cond ((< (- (nth 2 next-task) (nth 3 task)) 0) (setf valid nil)))
				(setf time-count (+ time-count (- (nth 3 next-task) (nth 2 next-task))))
				(cond ((not (eq (nth 1 task) (nth 0 next-task)))
							(cond ( (< (- (nth 2 next-task) (nth 3 task)) 40) (setf valid nil) ) )
							(setf (csp-no-service-counter state) (+ (csp-no-service-counter state) 1))
							(cond ((and (<= old-time 240) (> time-count 240))
										(cond ( (< (- (nth 2 next-task) (nth 3 task)) 80) (setf valid nil) ) )
										(setf (csp-no-service-counter state) (+ (csp-no-service-counter state) 1))
								  )
							)
					  )
					  (t
							(cond ((and (<= old-time 240) (> time-count 240))
										(cond ( (< (- (nth 2 next-task) (nth 3 task)) 40) (setf valid nil) ) )
										(setf (csp-no-service-counter state) (+ (csp-no-service-counter state) 1))
								  )
							)
					  )
				)
                (setf time-count (+ time-count (- (nth 2 next-task) (nth 3 task))))
				(cond ( (> time-count 480) (setf valid nil) ) )
			)
		)

		(cond ((not (eq 'L1 (nth 1 last-task)))
                                (setf time-count (+ time-count 40))
				(setf (csp-no-service-counter state) (+ (csp-no-service-counter state) 1))
			  )
		)

		(cond ( (> time-count 480) (setf valid nil) ) )
		(cond ((equal valid T) (setf (csp-cost state) (max (+ (csp-cost state) time-count) 360))))
		valid
   )
)

(defun constrictions(state)
	(let ((valid T)
		  (shifts (csp-assignments state))
		 )
		(dotimes (i (csp-shift-counter state))
			(setf valid (and valid (shift-validity (nth i shifts) state)))
		)
		valid
	)
)

(defun heuristica_1(estado)
	(cond ((< (csp-cost estado) 10) (setf this (list-length (csp-variables estado))))
		  ((> (list-length (csp-variables estado)) 1) (setf this (- (list-length (csp-variables estado)) (csp-cost estado))))
		  (t (setf this (* (list-length (csp-variables estado)) (expt 10 (log 10 (csp-cost estado))))))
	)
	(print (+ this (custo estado)))
	this
)

(defun heuristica_2(estado)
  (* (- (list-length (csp-variables estado)) (csp-shift-counter estado)) (csp-new_shift estado))
)

(defun heuristica_3(estado)
  (setf a (+ (list-length (csp-variables estado)) (* (csp-shift-counter estado) (csp-new_shift estado))))
  (print a)
  a
)

(defun heuristica_4(estado)
  (setf a (* (csp-shift-counter estado) (list-length (csp-variables estado))))
  (print a)
  a
)

(defun heuristica_5(estado)
	(let ((time 0)) 
	(dolist (shift (csp-assignments estado))
		(let ((time_shift 0))
		(dolist (tarefa shift)
			(setq time_shift (+ time_shift (duracao_tarefa tarefa)))
		)
		(setq time (+ time (- 8 time_shift)))
	)

	)
		(print time)
		time
))

(defun duracao_tarefa(tarefa)
  (let ((t_end 0) (t_init 0) (t_tarefa 0))
  (setq t_end (elt tarefa 3))
  (setq t_init (elt tarefa 2))
  (setq t_tarefa (- t_end t_init))
  t_tarefa
  )
)

(defun heuristica_6(estado)
	(let ((ideal 0) (real 0) (res 0))
	(dolist (shift (csp-assignments estado))
		
			(dolist (tarefa shift)
				(setq ideal (+ ideal (duracao_tarefa tarefa)))
				
			)
				

		
	)
	(setq real (custo estado))
	(setq res (- real ideal))
	)		
)

(defun heuristica_7(estado)
	(cond ((< (csp-cost estado) 10) (list-length (csp-variables estado)))
		  (t (* (list-length (csp-variables estado)) (expt 10 (log 10 (csp-cost estado))) (random 1000)))
	)
)

(defun heuristica_8(estado)
	(print (+ (list-length (csp-variables estado)) (custo estado)))
	(list-length (csp-variables estado))
)

(defun heuristica_9(estado)
	(cond ((> (list-length (csp-variables estado)) (* 0.75 (csp-max-vars estado)))
			(setf sigh (csp-cost estado)) )
		  (t (setf sigh (list-length (csp-variables estado))))
	)
	sigh
)

(defun heuristica_10(estado)
	(setf this (* (csp-cost estado) (list-length (csp-variables estado))))
	(print this)
	this
)

(defun heuristica_11(estado)
	(print (csp-cost estado))
	(csp-cost estado)
)

(defun heuristica_12(estado)
	(* (csp-shift-counter estado) (csp-new_shift estado))
)

(defun heuristica_13(estado)
	1
)

(defun heuristica_14(estado)
    (print estado)
	(let ((ideal 0) (real 0) (res 0))
	(dolist (shift (csp-assignments estado))
		
			(dolist (tarefa shift)
				(setq ideal (+ ideal (duracao_tarefa tarefa)))
				
			)
				

		
	)
	(print 'IDEAL)
	(print ideal)
	(print (csp-duracao_total estado))
	(setq x (- (csp-duracao_total estado) ideal))
    (print 'HEURISTICA)
	(print x)

	 x
	
	
	)
)

;;;
;;;            Improved Limited-Discrepancy Search
;;;
(defun ILDSProbe(problema estado k rDepth prof-actual profundidade-maxima)
    (let* 	((objectivo? (problema-objectivo? problema)))
	
	(block procura-ILDSProb
	(cond ((funcall objectivo? estado) (list estado))
		  ((= prof-actual profundidade-maxima) nil)
	 (t
	(let ((sucs (problema-gera-sucessores problema estado)) (melhor_suc_h NIL)
	(min_heur 0)
	(heur_actual 0)
	(heur (problema-heuristica problema)))
	  (	let ((i 0))
	(dolist (suc sucs)
 
		(cond ((= i 0)
				     (setf min_heur (funcall heur suc))
					 (setf melhor_suc_h suc)
					 (setf i (+ i 1))
					 )
				  (t
			      (setf heur_actual (funcall heur suc))
				  (cond ((< heur_actual min_heur) (setf min_heur heur_actual) (setf melhor_suc_h suc)))
				  (setf i (+ i 1))
	))))
	(remove melhor_suc_h sucs)
	(when(> rDepth k) (return-from procura-ILDSProb (ILDSProbe problema melhor_suc_h k (- rDepth 1) (+ prof-actual 1) profundidade-maxima)))
	(when(> k 0) 
		(return-from procura-ILDSProb (ILDSProbe problema (car sucs) (- k 1) (- rDepth 1) (+ prof-actual 1) profundidade-maxima))
			   
)))))))

(defun ILDS (problema profundidade-maxima)
  (let* ((prob (problema-estado-inicial problema))
	(n (list-length (csp-variables prob))))
	(block procura-ILDS
	(loop for k from 0 to n
				do (
				let((rDepth n))
				(return-from procura-ILDS (ILDSProbe problema (problema-estado-inicial problema) k rDepth 0 profundidade-maxima))
				)
	
))))

;;;
;;;             Depth-Bounded Discrepancy Search
;;;
(defun DDSProbe(problema estado k prof-actual profundidade-maxima)
	(let* ((objectivo? (problema-objectivo? problema)))
	(block procura-DDSProbe
	(cond ((funcall objectivo? estado) (list estado))
		  ((= prof-actual profundidade-maxima) nil)
	 (t
	(let ((sucs (problema-gera-sucessores problema estado)) (melhor_suc_h NIL)
	(min_heur 0)
	(heur_actual 0)
	(heur (problema-heuristica problema)))
	  (	let ((i 0))
	(dolist (suc sucs)
 
		(cond ((= i 0)
				     (setf min_heur (funcall heur suc))
					 (setf melhor_suc_h suc)
					 (setf i (+ i 1))
					 )
				  (t
			      (setf heur_actual (funcall heur suc))
				  (cond ((< heur_actual min_heur) (setf min_heur heur_actual) (setf melhor_suc_h suc)))
				  (setf i (+ i 1))
	))))
	(remove melhor_suc_h sucs)
	(when(= k 0) (return-from procura-DDSProbe (DDSProbe problema melhor_suc_h 0 (+ prof-actual 1) profundidade-maxima)))
	(when(= k 1) 
	(dolist (suc sucs)
		(return-from procura-DDSProbe (DDSProbe problema suc 0 (+ prof-actual 1) profundidade-maxima))
	))
	(when(> k 1) 
	(let(( solucao (DDSProbe problema melhor_suc_h (- k 1) (+ prof-actual 1) profundidade-maxima)))
	(when solucao 
		(cond ((funcall objectivo? estado) (return-from  procura-DDSProbe solucao))
		
		(t
		;;; se solucao nao for um estado objetivo, tentar ir contra a heuristica 
			(return-from procura-DDSProbe (DDSProbe problema (car sucs) 0 (+ prof-actual 1) profundidade-maxima))
		
		))
	
	)))))
))))

(defun DDS (problema profundidade-maxima)
  (let* ((prob (problema-estado-inicial problema))
	(n (list-length (csp-variables prob))))
	(block procura-DDS
	(loop for k from 0 to n
				do (return-from procura-DDS (DDSProbe problema (problema-estado-inicial problema) k 0 profundidade-maxima)		
)))))

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))
  
;;;
;;;              Sondagem iterativa
;;;
(defun sondagem.iterativa (problema profundidade-maxima)
  (let ((estado= (problema-estado= problema)) 
	(objectivo? (problema-objectivo? problema))
	(solucao_optima NIL) (custo_optimo 0))
  (dotimes (n 100000)
    (labels ((esta-no-caminho? (estado caminho)
	       (member estado caminho :test estado=))
	     
	     (procura-aleatoria (estado caminho prof-actual)
	       (block procura-aleatoria
		 (cond ((funcall objectivo? estado) (list estado))
		       ((= prof-actual profundidade-maxima) nil)
		       ((esta-no-caminho? estado caminho) nil)
		       (t 
		
								   
				(let((sucs (problema-gera-sucessores problema estado))
				  (idx 0)  (suc NIL))
				(setf idx (random-from-range 0 (- (list-length sucs) 1)))
				(setf suc (elt sucs idx))

			  (let ((solucao (procura-aleatoria suc 
						       (cons estado caminho)
						       (1+ prof-actual))))
			    (when solucao
				  (print 'SOLUCAO)
				  (print solucao)
				  (print 'IDEA)
				  (print (custo (nth 0 solucao)))
				  (setq custo_optimo (custo (nth 0 solucao)))
				  (cond ((eq solucao_optima NIL)(setf solucao_optima solucao))
				  ((> (custo (nth 0 solucao_optima)) custo_optimo)(setf solucao_optima solucao)))
				 
				
				  
				  ))))))))
      
      (procura-aleatoria (problema-estado-inicial problema) nil 0)))
	  solucao_optima
  )
)


(defun procura-alternativas (problema tipo-procura
		&key (profundidade-maxima most-positive-fixnum))

  (flet ((faz-a-procura (problema tipo-procura 
			 profundidade-maxima)
	   (cond ((string-equal tipo-procura "ILDS")
				(ILDS problema profundidade-maxima))
			 ((string-equal tipo-procura "abordagem.alternativa")
				(DDS problema profundidade-maxima))
			 ((string-equal tipo-procura "sondagem.iterativa")
				(sondagem.iterativa problema profundidade-maxima)))))

    (let ((*nos-gerados* 0)
	  (*nos-expandidos* 0)
	  (tempo-inicio (get-internal-run-time)))
      (let ((solucao (faz-a-procura problema tipo-procura
				    profundidade-maxima)))
	(list solucao 
	      (- (get-internal-run-time) tempo-inicio)
	      *nos-expandidos*
	      *nos-gerados*)))))

(defun faz-afectacao(tarefas tipo-procura &key (profundidade-maxima most-positive-fixnum))
  (let ((csp NIL) (problema NIL) (solucao NIL))
    (setf csp (csp-inicial tarefas))
    (setf problema (cria-problema csp (list #'successors) :objectivo? #'objectivo :custo #'custo :heuristica #'heuristica_14 :estado= #'estado   ))
    
		
	(cond ((or (string-equal tipo-procura "ILDS") (string-equal tipo-procura "abordagem.alternativa") (string-equal tipo-procura "sondagem.iterativa"))
				(setf solucao (procura-alternativas problema tipo-procura))
		  )
		  (t
				(setf solucao (procura problema tipo-procura :espaco-em-arvore? T))
		  )
	)

	(let* ((seq (nth 0 solucao)) (last_index (- (list-length seq) 1)) (goal_state (nth last_index seq)) (time_spent (/ (nth 1 solucao) internal-time-units-per-second 1.0)) (nos_exp (nth 2 solucao)) (nos_ger (nth 3 solucao)))
		(csp-assignments goal_state)
		(print "GOAL STATE: ")
		(print goal_state)
		(print "")
		(print "TIME SPENT (s): ")
		(print time_spent)
		(print "")
		(print "EXPANDED NODES: ")
		(print nos_exp)
		(print "")
		(print "GENERATED NODES: ")
		(print nos_ger)
	)
  )
)