;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
;This function returns t if there is a box in this row, nil otherwise
(defun findBox (row)
  (cond ((null row) nil)
      ((isBox (first row)) t)
      (t (findBox (rest row)))
    );end cond
  );end defun
;This function checks if any box is left alone in the current state
(defun goal-test (s)
    (cond ((null s) t)
        ((findBox (first s)) nil)
        (t (goal-test (rest s)))
      );end cond
  );end defun
; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;next-states list all possible moves the keeper can do in the current state
;if the keeper can move in any direction, a resulting state will be added to the list
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
   (x (car pos))
   (y (cadr pos))
   ;x and y are now the coordinate of the keeper in s.
   (UP 1)
   (RIGHT 2)
   (DOWN 3)
   (LEFT 4)
   (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
   )
    (cleanUpList result);end
   );end let
  );
;get the content of (r,c), if (r,c) is beyond the scope, return it's wall
(defun get-square (s c r)
  (let* ((width (length (first s)))
         (height (length s)))
    (cond 
        ((or (< r 0) (< c 0) (>= r height) (>= c width)) wall)
        (t (first (nthcdr c (first (nthcdr r s)))))
      );end cond
    );end let
  );end defun 
;set the new_content to the specific square, the remaining rows should keep the same,
;other square in the same row remain the same too.
(defun set-square(s c r new_content)
  (let ((width (length (first s)))
        (height (length s)))
    (cond 
        ((or (< r 0) (< c 0) (>= r height) (>= c width)) s)
        (t (let* ((change_row (first (nthcdr r s)))
                  (new_row (append (butlast change_row (- width c)) (list new_content) (nthcdr (+ c 1) change_row))))
          (append (butlast s (- height r)) (list new_row) (nthcdr (+ r 1) s))))
        );end cond
    );end let
  );end defun

;if the intended square is blank or star, it is considered a valid square, and the keeper can move
;Only two squares are going to be changed, the current square and the next square
;If the intended square is box or boxstar, it depends on what the content of the next-next-square.
;If that one is blank or star, the keeper can move in that direction.
;We also need to check whether the curret state is keeper or keeperstar 
(defun try-move (s dir)
  (let* (
         (old_pos (getKeeperPosition s 0))
         (UP 1)
         (RIGHT 2)
         (DOWN 3)
         (LEFT 4)
         (col (first old_pos))
         (row (first (rest old_pos)))
         (next_pos (cond ((= dir UP) (list col (- row 1)))
                         ((= dir RIGHT) (list (+ col 1) row ))
                         ((= dir DOWN) (list col (+ row 1)))
                         ((= dir LEFT) (list (- col 1) row))))
         (next_next_pos (cond ((= dir UP) (list col (- row 2)))
                              ((= dir RIGHT) (list (+ col 2) row))
                              ((= dir DOWN) (list col (+ row 2)))
                              ((= dir LEFT) (list (- col 2)  row))))
         (cur_content (get-square s col row))
         (next_content (get-square s (first next_pos) (first (rest next_pos))))
         (next_next_content (get-square s (first next_next_pos) (first (rest next_next_pos))))
         (on_star (isKeeperStar cur_content))) 

         (cond
                ((isWall next_content) nil)
         
                ((and (or (isBox next_content) (isBoxStar next_content))
                  (or (isBox next_next_content) (isWall next_next_content) (isBoxStar next_next_content))) nil)                
                ((and on_star (isBlank next_content))  
                  (set-square (set-square s col row star) (first next_pos) (first (rest next_pos)) keeper))
                ((and on_star (isStar next_content))  
                  (set-square (set-square s col row star) (first next_pos) (first (rest next_pos)) keeperstar))
                ((and (not on_star) (isBlank next_content))
                  (set-square (set-square s col row blank)  (first next_pos) (first (rest next_pos)) keeper))
                ((and (not on_star) (isStar next_content)) 
                  (set-square (set-square s col row blank)  (first next_pos) (first (rest next_pos)) keeperstar))
                 ((and on_star (isBox next_content) (isBlank next_next_content))
                  (set-square (set-square (set-square s col row star) (first next_pos) (first (rest next_pos)) keeper)
                    (first next_next_pos) (first (rest next_next_pos)) box))
                 ((and on_star (isBox next_content) (isStar next_next_content))
                  (set-square (set-square (set-square s col row star) (first next_pos) (first (rest next_pos)) keeper)
                    (first next_next_pos) (first (rest next_next_pos)) boxstar))
                 ((and on_star (isBoxStar next_content) (isBlank next_next_content))
                  (set-square (set-square (set-square s col row star) (first next_pos) (first (rest next_pos)) keeperstar)
                    (first next_next_pos) (first (rest next_next_pos)) box))
                ((and on_star (isBoxStar next_content) (isStar next_next_content))
                  (set-square (set-square (set-square s col row star) (first next_pos) (first (rest next_pos)) keeperstar)
                    (first next_next_pos) (first (rest next_next_pos)) boxstar))
                ((and (not on_star) (isBox next_content) (isBlank next_next_content))
                  (set-square (set-square (set-square s col row blank) (first next_pos) (first (rest next_pos)) keeper)
                    (first next_next_pos) (first (rest next_next_pos)) box))
                ((and (not on_star) (isBox next_content) (isStar next_next_content))
                  (set-square (set-square (set-square s col row blank) (first next_pos) (first (rest next_pos)) keeper)
                    (first next_next_pos) (first (rest next_next_pos)) boxstar))

                ((and (not on_star) (isBoxStar next_content) (isBlank next_next_content))
                  (set-square (set-square (set-square s col row blank) (first next_pos) (first (rest next_pos)) keeperstar)
                    (first next_next_pos) (first (rest next_next_pos)) box))
                ((and (not on_star) (isBoxStar next_content) (isStar next_next_content))
                  (set-square (set-square (set-square s col row blank) (first next_pos) (first (rest next_pos)) keeperstar)
                    (first next_next_pos) (first (rest next_next_pos)) boxstar))
                (t nil)
                )))
 
; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0 
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;

;This is admissible. If exists is a box, it needs at least one move to reach a star, which is based
;on the assumption that the keeper is next to it. So, there still requires at least n steps, in which
;n represents the number of boxes in the grid. (Which is not BoxStar)
(defun h1 (s)
  (count-box s 0)
  )
;count how many boxes exist in the grid
(defun count-box (s sofar)
  (cond ((null s) sofar)
  (t (count-box (rest s) (+ sofar (count box (first s)))))))
;find the largest distance between the keeper and any boxes
(defun keeper-to-box-max(pos boxes)          
    (cond ((null boxes) 0)
          (t (- (max-value (manhantan-dis pos (first boxes)) (keeper-to-box-max pos (rest boxes))) 1))
      ))
;find the maximum value of two values
(defun max-value(a b)
  (cond ((and (null a) (null b)) 0)
        ((null b) a)
        ((null a) b)
        ((> a b) a)
        (t b)
    )
  )
;This heuristic is a combination of three parts
;1) The sum of minimum distances between each box and stars, those stars can be calculated more than
;once. It will be more efficient to use bipartite matching, but I can't figure out how to do that
;2) The maximum distance between the keeper to boxes. The keeper will ultimately get to the farthest
;box
;3) If there is any stuch box, this state should have a higher heuristic. But in my check stuch function;
; I just checked four most common scenarios, it will be more efficient if we check the box is in the 
;second row, and the first row is covered by wall, and there is no star in the second level, this box 
;should also be stuck
(defun h304683898(s)
  
    (let ((stuck_score (cond ((find-stuck-box s (find-all-box s 0 0)) 999)
                            (t 0))))
          (+ (min-total (find-all-box s 0 0) (find-all-star s 0 0)) (+ (keeper-to-box-max (getKeeperPosition s 0) (find-all-box s 0 0)) stuck_score))
  
  ))
;This function returns a list of coordinates of all boxes
(defun find-all-box(s c r)
  (cond ((null s) nil)
        ((and (atom (first s)) (isBox (first s))) (append (list (list c r)) (find-all-box (rest s) (+ 1 c) r)))
        ((atom (first s)) (find-all-box (rest s) (+ 1 c) r))
        ((listp (first s)) (append (find-all-box (first s) c r) (find-all-box (rest s) c (+ 1 r))))

    );end cond
  )
;This function returns a list of coordinates of all stars
(defun find-all-star(s c r)
  (cond ((null s) nil)
        ((and (atom (first s)) (or (isStar (first s)) (isKeeperStar (first s)))) (append (list (list c r)) (find-all-star (rest s) (+ 1 c) r)))
        ((atom (first s)) (find-all-star (rest s) (+ 1 c) r))
        ((listp (first s)) (append (find-all-star (first s) c r) (find-all-star (rest s) c (+ 1 r))))

    );end cond
  )
;This function return the absolute value of the difference of two values
(defun absolute-dif (a b)
      (cond ((< (- a b) 0) (- b a))
              (t (- a b))
              );end cond
  );end defun
;This function defines the manhantan distance between two coordinates
(defun manhantan-dis(from to)
  (+ (absolute-dif (first from) (first to)) (absolute-dif (first (rest from)) (first (rest to))))
  )
(defun min-value(a b)
  (cond ((and (null a) (null b)) 0)
        ((null b) a)
        ((null a) b)
        ((< a b) a)
        (t b)
    )
  )
;This function returns the minimum distance between a box and stars
(defun min-dis (box stars)
        (cond ((null stars) 9999)
              (t (min-value (manhantan-dis box (first stars)) (min-dis box (rest stars)))))
              

    )
;This function sums all the minimum distance of boxes to its closest star
(defun min-total(boxes stars)
    (cond ((null boxes) 0)
          (t (+ (min-dis (first boxes) stars) (min-total (rest boxes) stars)))
      )
  )
;This checks if the content of a square can block a box or not
(defun wall-box-boxstar(v)
    (or (isWall v) (isBox v) (isBoxStar v))
  )
;This checks if a box is stuck in the current state
(defun check-stuck-box(s box)
      (let* ((col (first box))
          (row (first (rest box)))
          (up (get-square s col (- row 1)))
          (down (get-square s col (+ row 1)))
          (left (get-square s (- col 1) row))
          (right (get-square s (+ col 1) row)))
          (or (and (wall-box-boxstar up) (wall-box-boxstar left))
              (and (wall-box-boxstar up) (wall-box-boxstar right))
              (and (wall-box-boxstar down) (wall-box-boxstar left))
              (and (wall-box-boxstar down) (wall-box-boxstar right))
            )  
  ))
;This checks if any box is stuck in the current state
(defun find-stuck-box (s boxes)     
    (cond ((null boxes) nil)
            ((or (check-stuck-box s (first boxes)) (find-stuck-box s (rest boxes))) t)
            (t nil)
            );end cond
      );end defun


         

         
                
                
                
                
                
               
                
                
          
      

                  
                  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
           (1 0 6 4 0 1)
           (1 0 0 0 0 1)
           (1 1 2 1 1 1)
           (1 0 0 0 5 1)
           (1 0 0 0 4 1)
           (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
     (1 0 0 0 0 0 1) 
     (1 0 0 0 0 0 1) 
     (1 0 0 2 1 4 1) 
     (1 3 0 0 1 0 1)
     (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
     (1 0 0 0 1 0 0 0 1)
     (1 0 0 0 2 0 3 4 1)
     (1 0 0 0 1 0 0 0 1)
     (1 0 0 0 1 0 0 0 1)
     (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
     (0 0 0 0 0 1 4)
     (0 0 0 0 0 0 0)
     (0 0 1 1 1 0 0)
     (0 0 1 0 0 0 0)
     (0 2 1 0 0 0 0)
     (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
     (1 1 0 0 1 1)
     (1 0 0 0 0 1)
     (1 4 2 2 4 1)
     (1 0 0 0 0 1)
     (1 1 3 1 1 1)
     (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
     (1 0 0 0 0 0 4 1)
     (1 0 0 0 2 2 3 1)
     (1 0 0 1 0 0 4 1)
     (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
     (0 0 1 1 1 1 0 0 0 3)
     (0 0 0 0 0 1 0 0 0 0)
     (0 0 0 0 0 1 0 0 1 0)
     (0 0 1 0 0 1 0 0 1 0)
     (0 2 1 0 0 0 0 0 1 0)
     (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
     (1 4 0 0 4 1)
     (1 0 2 2 0 1)
     (1 2 0 1 0 1)
     (1 3 0 0 4 1)
     (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
     (1 1 1 0 0 1 1 1 1) 
     (1 0 0 0 0 0 2 0 1) 
     (1 0 1 0 0 1 2 0 1) 
     (1 0 4 0 4 1 3 0 1) 
     (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
      (1 0 0 0 1 1 0)
      (1 3 2 0 0 1 1)
      (1 1 0 2 0 0 1)
      (0 1 1 0 2 0 1)
      (0 0 1 1 0 0 1)
      (0 0 0 1 1 4 1)
      (0 0 0 0 1 4 1)
      (0 0 0 0 1 4 1)
      (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
      (1 4 0 0 0 4 1)
      (1 0 2 2 1 0 1)
      (1 0 2 0 1 3 1)
      (1 1 2 0 1 0 1)
      (1 4 0 0 4 0 1)
      (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
      (1 1 1 1 1 0 0 0 1 1 1 1)
      (1 0 0 0 2 0 0 0 0 0 0 1)
      (1 3 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 2 1 1 1 0 0 0 1)
      (1 0 0 0 0 1 0 1 4 0 4 1)
      (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
      (1 4 0 0 0 0 0 2 0 1)
      (1 0 2 0 0 0 0 0 4 1)
      (1 0 3 0 0 0 0 0 2 1)
      (1 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 4 1)
      (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
      (0 2 1 4 0 0 0)
      (0 2 0 4 0 0 0)
      (3 2 1 1 1 0 0)
      (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
      (1 0 0 0 0 0 1)
      (1 0 0 2 2 0 1)
      (1 0 2 0 2 3 1)
      (1 4 4 1 1 1 1)
      (1 4 4 1 0 0 0)
      (1 1 1 1 0 0 0)
      ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
      (1 0 0 0 1 0 0 0)
      (1 2 1 0 1 1 1 1)
      (1 4 0 0 0 0 0 1)
      (1 0 0 5 0 5 0 1)
      (1 0 5 0 1 0 1 1)
      (1 1 1 0 3 0 1 0)
      (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
      (1 3 0 0 1 0 0 0 4 1)
      (1 0 2 0 2 0 0 4 4 1)
      (1 0 2 2 2 1 1 4 4 1)
      (1 0 0 0 0 1 1 4 4 1)
      (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
      (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
      (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
      (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
      (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
      (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
      (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
      ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
      (0 0 0 1 0 0 0 0 1 0 0 0)
      (0 0 0 1 0 0 0 0 1 0 0 0)
      (1 1 1 1 0 0 0 0 1 1 1 1)
      (0 0 0 0 1 0 0 1 0 0 0 0)
      (0 0 0 0 0 0 3 0 0 0 2 0)
      (0 0 0 0 1 0 0 1 0 0 0 4)
      (1 1 1 1 0 0 0 0 1 1 1 1)
      (0 0 0 1 0 0 0 0 1 0 0 0)
      (0 0 0 1 0 0 0 0 1 0 0 0)
      (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
      (1 1 1 1 0 0 1 1 0)
      (1 0 0 0 2 0 0 1 0)
      (1 0 0 5 5 5 0 1 0)
      (1 0 0 4 0 4 0 1 1)
      (1 1 0 5 0 5 0 0 1)
      (0 1 1 5 5 5 0 0 1)
      (0 0 1 0 2 0 1 1 1)
      (0 0 1 0 3 0 1 0 0)
      (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
      (1 1 1 0 0 1 1 1 1 0)
      (1 0 0 2 0 0 0 1 1 0)
      (1 3 2 0 2 0 0 0 1 0)
      (1 1 0 2 0 2 0 0 1 0)
      (0 1 1 0 2 0 2 0 1 0)
      (0 0 1 1 0 2 0 0 1 0)
      (0 0 0 1 1 1 1 0 1 0)
      (0 0 0 0 1 4 1 0 0 1)
      (0 0 0 0 1 4 4 4 0 1)
      (0 0 0 0 1 0 1 4 0 1)
      (0 0 0 0 1 4 4 4 0 1)
      (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
      (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
      (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
      (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
      (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
      (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
      (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
      (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
      (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
   (k2 (getKeeperPosition s2 0))
   (deltaX (- (car k2) (car k1)))
   (deltaY (- (cadr k2) (cadr k1)))
   )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
    (t (if (> deltaX 0) 'RIGHT 'LEFT))
    );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
  ((= 1 (length m)) (list 'END))
  (t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
  );end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
  ((= s wall) (format t "#"))
  ((= s box) (format t "$"))
  ((= s keeper) (format t "@"))
  ((= s star) (format t "."))
  ((= s boxstar) (format t "*"))
  ((= s keeperstar) (format t "+"))
  (t (format t "|"))
  );end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun