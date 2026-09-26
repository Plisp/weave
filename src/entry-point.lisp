(in-package #:weave-tui)

(defun ancestor-asd-files (file)
  (loop for directory = (uiop:pathname-directory-pathname (truename file)) then parent
        for parent = (uiop:pathname-parent-directory-pathname directory)
        append (directory (make-pathname :name :wild :type "asd" :defaults directory))
        until (uiop:pathname-equal directory parent)))

(defun source-components (system)
  (let ((result nil))
    (labels ((walk (component)
               (when (typep component 'asdf:cl-source-file)
                 (push component result))
               (when (typep component 'asdf:parent-component)
                 (map nil #'walk (asdf:component-children component)))))
      (walk system))
    result))

(defun system-source-components (asd)
  (let ((result nil))
    (asdf:map-systems
     (lambda (system)
       (when (and (asdf:system-source-file system)
                  (uiop:pathname-equal asd (asdf:system-source-file system)))
         (dolist (component (source-components system))
           (push (cons system component) result)))))
    result))

(defun system-for-file (file)
  "Returns the ASDF system and source component corresponding to `file'."
  (let ((target (truename file)))
    (dolist (asd (ancestor-asd-files target))
      (unless (asdf:component-loaded-p (pathname-name asd))
        (asdf:load-asd asd))
      (let ((matches
              (remove-if-not
               (lambda (entry)
                 (when-let (component-file
                             (probe-file (asdf:component-pathname (cdr entry))))
                   (uiop:pathname-equal target component-file)))
               (system-source-components asd))))
        (when matches
          (unless (= 1 (length matches))
            (error "~a belongs to multiple ASDF systems: ~{~a~^, ~}"
                   target
                   (mapcar (lambda (entry) (asdf:component-name (car entry))) matches)))
          (return-from system-for-file
            (values (car (first matches)) (cdr (first matches)))))))
    (error "No ASDF source component owns ~a" target)))

(defun in-package-form-p (ast)
  (and (typep ast 'parse:macro-call)
       (eq 'cl:in-package (parse:resolve (parse:op ast)))))

(defun parse-source (source)
  "Parses all top-level forms in `source', applying package changes between reads."
  (let ((client (parse:make-client source))
        (start 0)
        (forms nil)
        (*package* *package*))
    (loop
      (multiple-value-bind (ast end)
          (parse:parse-from-string client :start start)
        (unless ast
          (return (values (or (nreverse forms) (list (hole))) end)))
        (push ast forms)
        (when (in-package-form-p ast)
          (eval (read-from-string source t nil :start start)))
        (setf start end)))))

(defun parse-file-loading-system (file)
  "Loads `file's owning asdf system before parsing"
  (multiple-value-bind (system component)
      (system-for-file file)
    (unless (asdf:component-loaded-p system)
      (asdf:load-system system))
    (let ((source (alexandria:read-file-into-string file)))
      (multiple-value-bind (forms end)
          (parse-source source)
        (values forms system component end (length source))))))

(defun demo-ast ()
  (let ((source "(lambda (a &key (b a supplied-p))
                   (loop for i from 1 to 10 do (print i)))"))
    (parse-source source)))

(defun tui-main (&optional (ast nil astp))
  (let ((ast-list (if astp ast (demo-ast))))
    (check-type ast-list cons)
    (let* ((root-loc (make-location :node 'undefined))
           (tui (make-instance 'ui :ast ast-list
                                   :stack (list (make-location :node ast-list
                                                               :id 0)
                                                root-loc))))
      (setf *state* tui)
      (setf (location-node root-loc) tui)
      ;; set default background to black and foreground to pure white (xterm extension)
      (format *terminal-io* "~c]10;#ffffff~c" #\esc (code-char 7))
      (format *terminal-io* "~c]11;#000000~c" #\esc (code-char 7))
      (unwind-protect
           (tui:run tui :redisplay-on-input t)
        (slog *log-stop*)))))

(defun main (&optional (file (first (uiop:command-line-arguments))))
  (let ((ast (if file (parse-file-loading-system file) (demo-ast))))
    (if (interactive-stream-p *standard-output*)
        (tui-main ast)
        (progn
          (bt:make-thread (lambda () (tui-main ast))
                          :initial-bindings `((*package* . ,*package*)))
          (loop :for (form . value) = (sb-concurrency:receive-message *log*)
                :until (eq value *log-stop*)
                :do (if form
                        (format t "~a~%|> ~s~%" form value)
                        (format t "~a~%" value))
                    (force-output))))))
