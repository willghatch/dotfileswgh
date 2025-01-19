;; -*- lexical-binding: t; -*-

(setq this-command-all-keys--current-key-groups nil)
;; TODO - make configurable size
(setq this-command-all-keys--ring (make-ring 1000))

(defun this-command-all-keys--read-key-sequence-advice (&rest args)
  ;; TODO - should this use this-single-command-keys, this-single-command-raw-keys, this-command-keys, or what?
  (setq this-command-all-keys--current-key-groups
        (cons (this-single-command-keys)
              this-command-all-keys--current-key-groups)))


(defun this-command-all-keys--post-command ()
  (let ((data (reverse
               (cons (this-single-command-keys)
                     this-command-all-keys--current-key-groups))))
    (setq this-command-all-keys--current-key-groups nil)
    (ring-insert this-command-all-keys--ring
                 (reverse data))))

(defun this-command-all-keys (&optional as-groups)
  "Return a vector of keys used for this command, including any uses of `read-key-sequence', at least up to the point where this function is used.  Returns a vector, or if AS-GROUPS is 't', return a list of vectors where each vector represents a different call to `read-key-sequence' or a command loop read."
  (let ((data (reverse
               (cons (this-single-command-keys)
                     this-command-all-keys--current-key-groups))))
    (if as-groups
        data
      (apply 'seq-concatenate 'vector data))))

(defun this-command-all-keys-ring-get (index &optional as-groups)
  "Returns a vector of keys used for command at INDEX in this-command-all-keys ring."
  (let ((data (ring-ref this-command-all-keys--ring index)))
    (if as-groups
        data
      (apply 'seq-concatenate 'vector data))))


(advice-add 'read-key-sequence
            :before
            #'this-command-all-keys--read-key-sequence-advice)
(advice-add 'read-key-sequence-vector
            :before
            #'this-command-all-keys--read-key-sequence-advice)
;; TODO - allow depth configuration.  The post-command hook clears the current value such that this-command-all-keys no longer works, and the value goes into the history ring.
(add-hook 'post-command-hook 'this-command-all-keys--post-command 99 nil)



;; TODO - make a minor mode that initializes everything, but also disables the advice etc when turned off.
(provide 'this-command-all-keys)
