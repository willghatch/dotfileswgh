;;; -*- lexical-binding: t; -*-

(setq tempel-path
      (mapcar (lambda (dfdir) (concat dfdir "/emacs/tempel/*.eld"))
              dotfileswgh-list))
(require 'tempel)

;; TODO - need to bind tempel-next, maybe tempel-previous, maybe tempel-complete, tempel-abort.  But maybe need to detect when tempel is active?  Do I want insert mode bindings, or do I want a tempel object for composiphrase?  Does template/snippet hole progression mirror anything in any other kind of completion?  Also tempel can activate on a region to use the region as an argument in the template, with tempel-insert.

(provide 'tempel-conf)
