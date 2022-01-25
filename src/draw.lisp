(in-package #:weave)

(defgeneric draw-node (draw-fn node x y &key &allow-other-keys)
  (:documentation "calls DRAW-FN, which is implemented by frontend:
(draw string x y &key relative-x relative-y color) -> relative-x relative-y"))

(defun theme-lookup (type)
  "TODO"
  (case type
    (typename #((42 161 152) ()))
    (string-literal #((131 148 150) ()))
    (number-literal #((133 153 0) ()))
    (t ; default color
     #((147 161 161) (0 43 54)))))

(defun fg (color)
  (svref color 0))

(defun bg (color)
  (svref color 1))

(defmethod draw-node (draw-fn node x y &key)
  (funcall draw-fn (value node) x y :color (theme-lookup (type-of node))))
