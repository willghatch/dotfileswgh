;;; wgh-theme.el --- my ad-hoc theme                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015 William Hatch

;; I hereby grant an eternal license to anyone to do with this file as
;; they wish, including redistributing in whole or in part, modified
;; or unmodified, etc.

;;; Commentary:

;;; Code:

(deftheme wgh "my ad-hoc theme")

(custom-theme-set-faces
 'wgh

 '(default ((t (:inherit nil :stipple nil :background "#050505" :foreground "#909090" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 99 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(bold-italic ((t (:slant italic :weight bold))))
 '(col-highlight ((t (:background "#000035"))))
 '(custom-comment-tag ((t (:foreground "#0087ff"))))
 '(diff-added ((t (:background "#001500"))))
 '(diff-changed ((t (:background "#202000"))))
 '(diff-context ((t (:foreground "#707070"))))
 '(diff-file-header ((t (:background "#202020"))))
 '(diff-header ((t (:background "#101010"))))
 '(diff-removed ((t (:background "#150000"))))
 '(evil-ex-lazy-highlight ((t (:inherit lazy-highlight))))
 '(evil-ex-substitute-matches ((t (:background "#5f0000"))))
 '(evil-search-highlight-persist-highlight-face ((t (:background "#00006f"))))
 '(flycheck-error ((t (:inherit error))))
 '(flycheck-info ((t (:inherit success))))
 '(flycheck-warning ((t (:inherit warning))))
 '(flyspell-duplicate ((t (:inherit warning))))
 '(font-lock-builtin-face ((t (:foreground "#476cab"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#683d8b"))))
 '(font-lock-comment-face ((t (:foreground "#685d9b" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#679cab"))))
 '(font-lock-doc-face ((t (:foreground "#ffffff"))))
 '(font-lock-function-name-face ((t (:foreground "#40af60"))))
 '(font-lock-keyword-face ((t (:foreground "#1f5f6f"))))
 '(font-lock-negation-char-face ((t (:foreground "#5f3f6f"))))
 '(font-lock-preprocessor-face ((t (:foreground "#bf4030"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#8f4f5f"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#8f4f8f"))))
 '(font-lock-string-face ((t (:foreground "#a420c3"))))
 '(font-lock-type-face ((t (:foreground "#d700ff"))))
 '(font-lock-variable-name-face ((t (:foreground "#7fffd4"))))
 '(font-lock-warning-face ((t (:foreground "#ffb90f"))))
 '(fringe ((t (:foreground "#00ffff"))))
 '(hc-tab ((t (:background "#000019"))))
 '(hc-trailing-whitespace ((t (:background "#252525"))))
 '(header-line ((t (:inherit mode-line))))
 '(helm-selection ((t (:foreground "#000000" :background "#50f0b0"))))
 '(helm-selection-line ((t (:foreground "#000000" :background "#90b090"))))
 '(highlight ((t (:background "#005f00" :foreground "#0000ff"))))
 '(hl-line ((t (:background "#000035"))))
 '(hl-todo ((t (:foreground "#f59035" :background "#500000" :weight bold))))
 '(hydra-face-blue ((t (:foreground "#2646ff"))))
 '(ido-first-match ((t (:foreground "#fd0060"))))
 '(ido-subdir ((t (:foreground "#3050f0"))))
 '(isearch-fail ((t (:background "#cd0000"))))
 '(italic ((t (:slant italic))))
 '(js2-warning ((t (:slant italic))))
 '(lazy-highlight ((t (:background "#0087ff" :foreground "#000000"))))
 '(linum ((t (:inherit mode-line :foreground "#309f9f" :underline nil :weight normal))))
 '(magit-diff-none ((t (:inherit diff-header))))
 '(magit-item-highlight ((t (:background "#232323"))))
 '(match ((t (:background "#cdcd00" :foreground "#00005f"))))
 '(menu ((t (:background "#003070" :foreground "#70c0ff"))))
 '(minibuffer-prompt ((t (:foreground "#ff00ff"))))
 '(mode-line ((t (:background "#161616" :foreground "#00afd7"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#161616" :foreground "#004747" :box (:line-width -1 :color "#303030") :weight light))))
 '(next-error ((t (:inherit region :foreground "#00cd00"))))
 '(org-hide ((t (:foreground "#252525"))))
 '(outline-1 ((t (:foreground "#ff00ff"))))
 '(outline-2 ((t (:foreground "#9d007d"))))
 '(outline-3 ((t (:foreground "#cf0000"))))
 '(outline-4 ((t (:foreground "#d75f00"))))
 '(outline-5 ((t (:foreground "#d7ff00"))))
 '(outline-6 ((t (:foreground "#00ff00"))))
 '(outline-7 ((t (:foreground "#00ffff"))))
 '(outline-8 ((t (:foreground "#5c5cff"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#ff00ff"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#00ffff"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#ff0000"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#7f1780"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#d7ff00"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#5c5cff"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#cd00cd"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#d75f00"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#00ff00"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "#6f0000"))))
 '(region ((t (:background "#0000d7" :foreground "#ff00ff"))))
 '(secondary-selection ((t (:background "#5d2d50" :foreground "#20cf20"))))
 '(sp-show-pair-match-face ((t (:background "#363636"))))
 '(tool-bar ((t (:foreground "#00ff00" :box (:line-width 1 :style released-button)))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#c5a535"))))
 '(yafolding-ellipsis-face ((t (:background "#ff00ff"))) t)
 '(yas-field-highlight-face ((t (:background "#001d00"))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'wgh)

;;; wgh-theme.el ends here