(in-package :lgj-fall25)

(defun stop-thread (thread &key (timeout 5))
  (let ((thread-name (bt2:thread-name thread)))
    ;; try to finish naturally
    (handler-case
        (bt2:with-timeout (timeout)
          (bt2:join-thread thread))
      (bt2:timeout ()
        ;;(error (e)
        (format t "Thread didn't stop gracefully, destroy...~%")
        (bt2:destroy-thread thread)))
    (format t "Stopped thread: ~A~%" thread-name)
    t))

(defun stop-thread-by-name (thread-name &key (timeout 5))
  "Try to stop gracefully first"
  (let ((thread (find thread-name (bt2:all-threads)
                      :key #'bt2:thread-name
                      :test #'string=)))
    (if thread
        (stop-thread thread :timeout timeout)
        (progn
          (format t "Thread not found: ~A~%" thread-name)
          nil))))
