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

 '(default ((((background dark)) (:inherit nil :stipple nil :background "#050505" :foreground "#909090" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 99 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))
            (((background light)) (:inherit nil :stipple nil :background "#f0f0f0" :foreground "#2e3435" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 99 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(bold-italic ((((background dark)) (:slant italic :weight bold))))
 '(col-highlight ((((background dark)) (:background "#000035"))))
 '(custom-comment-tag ((((background dark)) (:foreground "#0087ff"))))
 '(diff-added ((((background dark)) (:background "#001500"))))
 '(diff-changed ((((background dark)) (:background "#202000"))))
 '(diff-context ((((background dark)) (:foreground "#707070"))))
 '(diff-file-header ((((background dark)) (:background "#202020"))))
 '(diff-header ((((background dark)) (:background "#101010"))))
 '(diff-removed ((((background dark)) (:background "#150000"))))
 '(evil-ex-lazy-highlight ((((background dark)) (:inherit lazy-highlight))))
 '(evil-ex-substitute-matches ((((background dark)) (:background "#5f0000"))))
 '(evil-search-highlight-persist-highlight-face ((((background dark)) (:foreground "#b9c900" :background "#00006f"))))
 '(flycheck-error ((((background dark)) (:inherit error))))
 '(flycheck-info ((((background dark)) (:foreground "#369636"))))
 '(flycheck-warning ((((background dark)) (:inherit warning))))
 '(flymake-note ((((background dark)) (:inherit font-lock-comment-face))))
 '(flyspell-duplicate ((((background dark)) (:inherit warning))))
 '(font-lock-builtin-face ((((background dark)) (:foreground "#476cab"))))
 '(font-lock-comment-delimiter-face ((((background dark)) (:foreground "#683d8b"))))
 '(font-lock-comment-face ((((background dark)) (:foreground "#685d9b" :slant italic))))
 '(font-lock-constant-face ((((background dark)) (:foreground "#679cab"))))
 '(font-lock-doc-face ((((background dark)) (:foreground "#ffffff"))))
 '(font-lock-function-name-face ((((background dark)) (:foreground "#40af60"))))
 '(font-lock-keyword-face ((((background dark)) (:foreground "#1f5f6f"))))
 '(font-lock-negation-char-face ((((background dark)) (:foreground "#5f3f6f"))))
 '(font-lock-preprocessor-face ((((background dark)) (:foreground "#bf4030"))))
 '(font-lock-regexp-grouping-backslash ((((background dark)) (:foreground "#8f4f5f"))))
 '(font-lock-regexp-grouping-construct ((((background dark)) (:foreground "#8f4f8f"))))
 '(font-lock-string-face ((((background dark)) (:foreground "#a420c3"))))
 '(font-lock-type-face ((((background dark)) (:foreground "#d700ff"))))
 '(font-lock-variable-name-face ((((background dark)) (:foreground "#7fffd4"))))
 '(font-lock-warning-face ((((background dark)) (:foreground "#ffb90f"))))
 '(fringe ((((background dark)) (:foreground "#00ffff"))))
 '(hc-tab ((((background dark)) (:background "#000019"))))
 '(hc-trailing-whitespace ((((background dark)) (:background "#252525"))))
 '(header-line ((((background dark)) (:inherit mode-line))))
 '(helm-selection ((((background dark)) (:foreground "#000000" :background "#50f0b0"))))
 '(helm-selection-line ((((background dark)) (:foreground "#000000" :background "#90b090"))))
 '(highlight ((((background dark)) (:background "#002f00" :foreground "#ffb030"))))
 '(hi-lock ((((background dark)) (:background "#301f00" :foreground "#6060ff"))))
 '(hl-line ((((background dark)) (:background "#000035"))))
 '(hl-todo ((((background dark)) (:foreground "#f59035" :background "#500000" :weight bold))))
 '(hydra-face-blue ((((background dark)) (:foreground "#2646ff"))))
 '(ido-first-match ((((background dark)) (:foreground "#fd0060"))))
 '(ido-subdir ((((background dark)) (:foreground "#3050f0"))))
 '(isearch-fail ((((background dark)) (:background "#cd0000"))))
 '(italic ((((background dark)) (:slant italic))))
 '(js2-warning ((((background dark)) (:slant italic))))
 '(lazy-highlight ((((background dark)) (:background "#0087ff" :foreground "#000000"))))
 '(line-number ((((background dark)) (:inherit mode-line :foreground "#309f9f" :underline nil :weight normal))))
 '(line-number-current-line ((((background dark)) (:inherit line-number :background "#309f9f" :foreground "#161616" :underline nil :weight normal))))
 '(linum ((((background dark)) (:inherit mode-line :foreground "#309f9f" :underline nil :weight normal))))
 '(lsp-face-highlight-write ((((background dark)) (:foreground "#30ff9f"))))
 '(lsp-face-highlight-write ((((background dark)) (:foreground "#30ff9f"))))
 '(magit-branch-current ((((background dark)) (:inherit magit-branch-local :background "#053505" :weight bold))))
 '(magit-branch-local ((((background dark)) (:foreground "#9999ff"))))
 '(magit-diff-none ((((background dark)) (:inherit diff-header))))
 '(magit-head ((((background dark)) (:inherit magit-branch-current))))
 '(magit-item-highlight ((((background dark)) (:background "#232323"))))
 '(markchars-heavy ((((background dark)) (:foreground "#ffffff"))))
 '(markchars-light ((((background dark)) (:foreground "#9f9f9f"))))
 '(markchars-white ((((background dark)) (:foreground "#ffffff"))))
 '(match ((((background dark)) (:background "#4d9d00" :foreground "#30005f"))))
 '(menu ((((background dark)) (:background "#003070" :foreground "#70c0ff"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "#ff00ff"))))
 '(mode-line ((((background dark)) (:background "#161616" :foreground "#00afd7"))))
 '(mode-line-inactive ((((background dark)) (:inherit mode-line :background "#161616" :foreground "#004747" :box (:line-width -1 :color "#303030") :weight light))))
 '(next-error ((((background dark)) (:inherit region :foreground "#00cd00"))))
 '(org-hide ((((background dark)) (:foreground "#353535"))))
 '(outline-1 ((((background dark)) (:foreground "#5fffcf"))))
 '(outline-2 ((((background dark)) (:foreground "#00cfff"))))
 '(outline-3 ((((background dark)) (:foreground "#e040ff"))))
 '(outline-4 ((((background dark)) (:foreground "#e0209f"))))
 '(outline-5 ((((background dark)) (:foreground "#c70057"))))
 '(outline-6 ((((background dark)) (:foreground "#c11019"))))
 '(outline-7 ((((background dark)) (:foreground "#a01003"))))
 '(outline-8 ((((background dark)) (:foreground "#904000"))))
 '(racket-xp-def-face ((((background dark)) (:inherit match))))
 '(racket-xp-error-face ((((background dark)) (:background "#190505"))))
 '(rainbow-delimiters-depth-1-face ((((background dark)) (:foreground "#ff00ff"))
                                    (((background light)) (:foreground "#502090"))))
 '(rainbow-delimiters-depth-2-face ((((background dark)) (:foreground "#00ffff"))
                                    (((background light)) (:foreground "#007090"))))
 '(rainbow-delimiters-depth-3-face ((((background dark)) (:foreground "#ff0000"))
                                    (((background light)) (:foreground "#700070"))))
 '(rainbow-delimiters-depth-4-face ((((background dark)) (:foreground "#7f1780"))
                                    (((background light)) (:foreground "#903050"))))
 '(rainbow-delimiters-depth-5-face ((((background dark)) (:foreground "#d7ff00"))
                                    (((background light)) (:foreground "#006060"))))
 '(rainbow-delimiters-depth-6-face ((((background dark)) (:foreground "#5c5cff"))
                                    (((background light)) (:foreground "#800020"))))
 '(rainbow-delimiters-depth-7-face ((((background dark)) (:foreground "#cd00cd"))
                                    (((background light)) (:foreground "#509000"))))
 '(rainbow-delimiters-depth-8-face ((((background dark)) (:foreground "#d75f00"))
                                    (((background light)) (:foreground "#006090"))))
 '(rainbow-delimiters-depth-9-face ((((background dark)) (:foreground "#00ff00"))
                                    (((background light)) (:foreground "#305070"))))
 '(rainbow-delimiters-unmatched-face ((((background dark)) (:background "#6f0000"))))
 '(region ((((background dark)) (:background "#0000d7" :foreground "#ff00ff"))))
 '(secondary-selection ((((background dark)) (:background "#5d2d50" :foreground "#20cf20"))))
 '(smerge-mine ((((background dark)) (:background "#200000"))))
 '(smerge-other ((((background dark)) (:background "#002000"))))
 '(smerge-refined-added ((((background dark)) (:background "#103010"))))
 '(smerge-refined-removed ((((background dark)) (:background "#301010"))))
 '(sp-show-pair-match-face ((((background dark)) (:background "#363636"))))
 '(tool-bar ((((background dark)) (:foreground "#00ff00" :box (:line-width 1 :style released-button)))))
 '(web-mode-html-tag-bracket-face ((((background dark)) (:foreground "#c5a535"))))
 '(whitespace-tab ((((background dark)) (:background "#000019"))))
 '(whitespace-final-newline ((((background dark)) (:background "#252595" :foreground "#c5a3c5"))))
 '(whitespace-trailing ((((background dark)) (:background "#252525"))))
 '(yafolding-ellipsis-face ((((background dark)) (:background "#ff00ff"))) t)
 '(yas-field-highlight-face ((((background dark)) (:background "#001d00"))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'wgh)

;;; wgh-theme.el ends here
