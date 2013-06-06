;; ------ structures ------

(defstruct pair
  fst
  snd)
(defstruct inl
  val)
(defstruct inr
  val)
(defstruct axiom)

;; ------ functions ------

(defun fix (f) (funcall f (lambda () (fix f))))
