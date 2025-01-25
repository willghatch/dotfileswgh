;;; -*- lexical-binding: t; -*-

(load-file "~/dotfileswgh/emacs/package-conf.el")

(message "")
(message "Beginning upgrade...")
(message "Answering YES to all packaging queries!")
(message "")

(cl-flet ((yes-or-no-p (&rest args) t)
          (y-or-n-p (&rest args) t))
  (package-upgrade))

(message "Upgrading complete!")

