;;; config.scm

;; ---------------------------------------------------------------------------
;; Control socket
;; ---------------------------------------------------------------------------
;; Floatile's login session sets FLOATILE_CONTROL_SOCKET, which the eww bar and
;; floatilectl use.

;; The bar's power menu logs out with `floatilectl quit'.
(register-command! 'quit (lambda () (floatile-quit)))

;; ---------------------------------------------------------------------------
;; Appearance (kept minimal; matching gaps_in/gaps_out/border_size only)
;; ---------------------------------------------------------------------------

(set! border-width 4)
(set! border-color-focused '(0.4 0.6 1.0))
(set! border-color-unfocused '(0.35 0.35 0.35))
(set! outer-gap 0)
(set! inner-gap 0)
(set! titlebar-height 24)
(set! titlebar-color-focused '(0.3 0.45 0.75))
(set! titlebar-color-unfocused '(0.25 0.25 0.25))

(floatile-set-default-layout 'balanced-groups)

;; input { follow_mouse = 1 }, but only between tiled windows, so floating
;; windows never gain or lose focus by pointing.
(set! focus-policy "follow-mouse-tiled")

;; Scale every physical display when it connects.  The default is 1.
;; (set! default-pmonitor-scale 2)
(auto-vmonitors)

;; ---------------------------------------------------------------------------
;; Client drawing (see "Client drawing" in code/instructions.md)
;; ---------------------------------------------------------------------------

;; Let OpenGL and Vulkan clients hand over GPU buffers.  #f makes them draw on
;; the CPU, which only helps to rule out a GPU driver problem.
(set! linux-dmabuf-enabled #t)
;; Covered or off-screen windows are told to draw once a second instead of
;; every frame; 0 tells them every frame.
(set! hidden-surface-frame-interval 1000)
;; Tell clients the fractional scale of their display, so they draw sharp at
;; scales such as 1.5 instead of drawing at 2 and being scaled down.
(set! send-preferred-fractional-scale #t)
;; Draw each display when something changed, paced by its refresh rate.
;; "timer" draws every render-timer-interval milliseconds instead.
(set! frame-scheduling "vblank")
(set! render-timer-interval 16)

;; ---------------------------------------------------------------------------
;; Window rules and decoration
;; ---------------------------------------------------------------------------

;; Floating windows wear a title bar; tiled windows do not.  This is the
;; default `window-decoration-policy!'; replace it with set! to change it.

(define floating-window-matches
  '(((app-id . "^bashrun$"))
    ((title . "^the-unicoder$"))
    ((title . "^Surge XT$"))))

(define (apply-window-rules! event)
  (let ((window-id (event-field event 'window-id)))
    (do-window-match* window-id
      ('((title . "foot")) (set-window-swallowable! window-id #t))
      ('((title . "wezterm")) (set-window-swallowable! window-id #t)))
    (when (any (lambda (match) (window-matches? window-id match))
               floating-window-matches)
      (set-window-floating! window-id #t))
    (window-decoration-policy! window-id (window-effectively-floating? window-id))))

(define (decorate-for-presentation! event)
  (window-decoration-policy! (event-field event 'window-id)
                             (event-field event 'effectively-floating)))

(floatile-set-event-handler!
  (on-events '(window-created) apply-window-rules!
    (on-events '(window-floating-changed window-tiling-changed) decorate-for-presentation!
      floatile-default-event-handler)))

;; ---------------------------------------------------------------------------

(floatile-set-spawn-environment! "_JAVA_AWT_WM_NONREPARENTING" "1")
(floatile-set-spawn-environment! "GDK_BACKEND" "wayland,x11,*")
(floatile-set-spawn-environment! "SDL_VIDEODRIVER" "wayland")
(floatile-set-spawn-environment! "CLUTTER_BACKEND" "wayland")
(floatile-set-spawn-environment! "QT_AUTO_SCREEN_SCALE_FACTOR" "1")
(floatile-set-spawn-environment! "QT_QPA_PLATFORM" "wayland;xcb")
(floatile-set-spawn-environment! "QT_WAYLAND_DISABLE_WINDOWDECORATION" "1")
(floatile-set-spawn-environment! "QT_QPA_PLATFORMTHEME" "gtk3")
(floatile-set-spawn-environment! "ELECTRON_OZONE_PLATFORM_HINT" "auto")

;; ---------------------------------------------------------------------------
;; Startup programs
;; ---------------------------------------------------------------------------
(set! startup-commands
  (list (enable-xwayland-satellite!)
        '("floatile-startup-programs.sh")
        ;; This does nothing for floatile, but I do need something LIKE it to watch theme change and set theme colors appropriately.
        ;;'("bash" "/home/wgh/dotfileswgh/config/hypr/lightdark-update")
        ))

;; ---------------------------------------------------------------------------
;; Mouse bindings
;; ---------------------------------------------------------------------------

(bind-mouse-both '(super) 272 'drag floatile-mouse-move)
(bind-mouse-both '(super) 273 'drag floatile-mouse-resize)

;; Scrolling is bindable too, and a bound scroll never reaches the
;; application.  Nothing here uses it; an example of the shape:
;;   (bind-mouse-scroll-both '(super) 'up   (lambda (event) (floatile-next-workspace)))
;;   (bind-mouse-scroll-both '(super) 'down (lambda (event) (floatile-prev-workspace)))

;; ---------------------------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------------------------
;; Shift is folded into the key name.
;;  Modifier symbols: 'super, 'alt, 'ctrl, and hyper below.
;;
;; The hatchak keymap puts hyper on Mod2, which this layout uses as an
;; ordinary modifier rather than NumLock -- the number pad always behaves as
;; though NumLock were on -- so Mod2 is left out of the ignore list the
;; shipped configuration sets, and named here instead.
(floatile-set-xkb-file!
 "/rootgit/mooncrater-input.rootgit/examples/xkb-layout_qwerty-mod-mooncrater_compiled.xkb")
(set-ignored-modifiers! '())
(define-modifier-alias! 'hyper 'mod2)

(define main-keymap (make-keymap-with-parent pass-through-keymap))
(set! *current-keymap* main-keymap)

;; -- Windows --
(bind-key main-keymap '(super) "j"      (lambda () (floatile-focus-next)))
(bind-key main-keymap '(super) "k"      (lambda () (floatile-focus-prev)))
(bind-key main-keymap '(alt super) "j"  (lambda () (floatile-swap-next)))
(bind-key main-keymap '(alt super) "k"  (lambda () (floatile-swap-prev)))
(bind-key main-keymap '(super) "h"      (lambda () (floatile-resize-main-minus)))
(bind-key main-keymap '(super) "l"      (lambda () (floatile-resize-main-plus)))

(bind-key main-keymap '(alt super) "h"
  (lambda ()
    (case (floatile-current-layout)
      ((balanced-groups) (floatile-adjust-group-count-minus))
      ((column-split) (floatile-adjust-columns-minus))
      ((master-stack) (floatile-adjust-master-count-minus)))))
(bind-key main-keymap '(alt super) "l"
  (lambda ()
    (case (floatile-current-layout)
      ((balanced-groups) (floatile-adjust-group-count-plus))
      ((column-split) (floatile-adjust-columns-plus))
      ((master-stack) (floatile-adjust-master-count-plus)))))

(floatile-set-layout-cycle '(balanced-groups floating))
(bind-key main-keymap '(super) "space"  (lambda () (floatile-cycle-layout)))

(bind-key main-keymap '(super) "c"      (lambda () (floatile-close-window)))
(bind-key main-keymap '(super) "f"      (lambda () (floatile-toggle-maximize)))

(bind-key main-keymap '(super) "F"      (lambda () (floatile-toggle-fullscreen)))

;; -- Workspaces --
(bind-key main-keymap '(super) "g"      (lambda () (floatile-create-workspace)))

(bind-key main-keymap '(alt super) "g"
          (lambda ()
            (let* ((window-id (floatile-focused-window-id))
                   (index (floatile-create-workspace)))
              (and window-id index
                   (floatile-move-window-to-workspace window-id index)))))

;; Delete the workspace, moving its windows to the next one, which is shown.
(bind-key main-keymap '(alt ctrl super) "c" (lambda () (floatile-delete-workspace)))

(bind-key main-keymap '(super) "w"      (lambda () (floatile-next-workspace)))
(bind-key main-keymap '(super) "b"      (lambda () (floatile-prev-workspace)))
;; Reorder: move the current workspace one place right or left, wrapping.
(bind-key main-keymap '(ctrl super) "w" (lambda () (floatile-shift-workspace-next)))
(bind-key main-keymap '(ctrl super) "b" (lambda () (floatile-shift-workspace-prev)))
;; Move the focused window to the next or previous workspace, and follow it.
(bind-key main-keymap '(alt super) "w"
          (lambda () (and (floatile-move-to-next-workspace) (floatile-next-workspace))))
(bind-key main-keymap '(alt super) "b"
          (lambda () (and (floatile-move-to-prev-workspace) (floatile-prev-workspace))))

;; -- Screens --
(bind-key main-keymap '(super) "n"      (lambda () (floatile-focus-next-vmonitor)))
(bind-key main-keymap '(super) "p"      (lambda () (floatile-focus-prev-vmonitor)))
(bind-key main-keymap '(alt super) "n"  (lambda () (floatile-move-window-to-next-vmonitor)))
(bind-key main-keymap '(alt super) "p"  (lambda () (floatile-move-window-to-prev-vmonitor)))
(bind-key main-keymap '(ctrl super) "n" (lambda () (floatile-move-workspace-to-next-vmonitor)))
(bind-key main-keymap '(ctrl super) "p" (lambda () (floatile-move-workspace-to-prev-vmonitor)))

;; -- Launch programs --
(bind-key main-keymap '(super) "Return" (lambda () (floatile-spawn "vlaunch" "terminal")))
(bind-key main-keymap '(super) "v"      (lambda () (floatile-spawn "vlaunch" "terminal")))
(bind-key main-keymap '(alt super) "v"  (lambda () (floatile-spawn "vlaunch" "terminal2")))
(bind-key main-keymap '(ctrl super) "v" (lambda () (floatile-spawn "vlaunch" "terminal3")))
(bind-key main-keymap '(super) "r"      (lambda () (floatile-spawn "vlaunch" "launcher")))
(bind-key main-keymap '(super) "u"      (lambda () (floatile-spawn "vlaunch" "unicode")))
(bind-key main-keymap '() "XF86DOS"     (lambda () (floatile-spawn "vlaunch" "unicode")))

;; -- Session --
(bind-key main-keymap '(alt super) "q"  (lambda () (floatile-quit)))              ; exit
(bind-key main-keymap '(super) "q"      (lambda () (floatile-spawn "swaylock-configured")))

;; Not yet implemented: reloading the configuration at runtime
(bind-key main-keymap '(alt super) "r" (lambda () (floatile-reload-config)))

;; -- $hyp (MOD2) utilities --
(bind-key main-keymap '(hyper) "l" (lambda () (floatile-spawn "lightdark-status" "toggle")))
(bind-key main-keymap '(hyper) "s" (lambda () (floatile-spawn "vlaunch" "screenshot")))
(bind-key main-keymap '(hyper) "t" (lambda () (floatile-spawn "dunstctl" "close-all")))
(bind-key main-keymap '(hyper) "y" (lambda () (floatile-spawn "dunstctl" "context")))
(bind-key main-keymap '(hyper) "h" (lambda () (floatile-spawn "dunstctl" "history-pop")))
(bind-key main-keymap '(hyper) "m" (lambda () (floatile-spawn "state" "mute" "toggle")))
(bind-key main-keymap '(hyper) "u" (lambda () (floatile-spawn "state" "volume" "inc")))
(bind-key main-keymap '(hyper) "d" (lambda () (floatile-spawn "state" "volume" "dec")))
(bind-key main-keymap '(hyper super) "m" (lambda () (floatile-spawn "mpcc" "toggle")))
(bind-key main-keymap '(hyper super) "t" (lambda () (floatile-spawn "vlaunch" "musictoggle")))
(bind-key main-keymap '(hyper super) "s" (lambda () (floatile-spawn "vlaunch" "musicpauseall")))
(bind-key main-keymap '(hyper super) "n" (lambda () (floatile-spawn "vlaunch" "musicnext")))
(bind-key main-keymap '(hyper super) "p" (lambda () (floatile-spawn "vlaunch" "musicprev")))
(bind-key main-keymap '(hyper super) "d" (lambda () (floatile-spawn "audio-output-toggle.py")))
(bind-key main-keymap '(hyper super) "b" (lambda () (floatile-spawn "toggle-floatile-eww-bar")))
(bind-key main-keymap '(ctrl hyper super) "n" (lambda () (floatile-spawn "vlaunch" "media_next_source")))
(bind-key main-keymap '(ctrl hyper super) "p" (lambda () (floatile-spawn "vlaunch" "media_prev_source")))

;; -- Media keys (the unmodified half of the $hyp bindings above) --
(bind-key main-keymap '() "XF86AudioMute"        (lambda () (floatile-spawn "state" "mute" "toggle")))
(bind-key main-keymap '() "XF86AudioRaiseVolume" (lambda () (floatile-spawn "state" "volume" "inc")))
(bind-key main-keymap '() "XF86AudioLowerVolume" (lambda () (floatile-spawn "state" "volume" "dec")))
(bind-key main-keymap '() "XF86AudioPlay"        (lambda () (floatile-spawn "vlaunch" "musictoggle")))
(bind-key main-keymap '() "XF86AudioPause"       (lambda () (floatile-spawn "vlaunch" "musicpauseall")))
(bind-key main-keymap '() "XF86AudioNext"        (lambda () (floatile-spawn "vlaunch" "musicnext")))
(bind-key main-keymap '() "XF86AudioPrev"        (lambda () (floatile-spawn "vlaunch" "musicprev")))
(bind-key main-keymap '() "XF86MonBrightnessDown" (lambda () (floatile-spawn "state" "backlight" "dec")))
(bind-key main-keymap '() "XF86MonBrightnessUp"   (lambda () (floatile-spawn "state" "backlight" "inc")))

;; -- VT switching --
(bind-key main-keymap '(alt ctrl) "F1"  (lambda () (floatile-switch-vt 1)))
(bind-key main-keymap '(alt ctrl) "F2"  (lambda () (floatile-switch-vt 2)))
(bind-key main-keymap '(alt ctrl) "F3"  (lambda () (floatile-switch-vt 3)))
(bind-key main-keymap '(alt ctrl) "F4"  (lambda () (floatile-switch-vt 4)))
(bind-key main-keymap '(alt ctrl) "F5"  (lambda () (floatile-switch-vt 5)))
(bind-key main-keymap '(alt ctrl) "F6"  (lambda () (floatile-switch-vt 6)))
(bind-key main-keymap '(alt ctrl) "F7"  (lambda () (floatile-switch-vt 7)))
(bind-key main-keymap '(alt ctrl) "F8"  (lambda () (floatile-switch-vt 8)))
(bind-key main-keymap '(alt ctrl) "F9"  (lambda () (floatile-switch-vt 9)))
(bind-key main-keymap '(alt ctrl) "F10" (lambda () (floatile-switch-vt 10)))
(bind-key main-keymap '(alt ctrl) "F11" (lambda () (floatile-switch-vt 11)))
(bind-key main-keymap '(alt ctrl) "F12" (lambda () (floatile-switch-vt 12)))

;; ---------------------------------------------------------------------------
;; Per-machine extensions
;; ---------------------------------------------------------------------------
;; Load every floatile/config.scm found in $XDG_CONFIG_DIRS (default /etc/xdg),
;; in path order, each directory once.  They load last, so they can override
;; anything above.  floatile-startup-programs.sh searches the same way for
;; floatile/floatile-startup-programs.sh.
;;
;; Relative entries are ignored, as the XDG base directory spec says.  An
;; extension that raises stops only itself; the rest still load, and the
;; errors are then raised together so they are reported with the others.

(let* ((dirs-var (getenv "XDG_CONFIG_DIRS"))
       (dirs (string-split (if (and dirs-var (not (string-null? dirs-var)))
                               dirs-var
                               "/etc/xdg")
                           #\:))
       (errors '()))
  (define (strip-trailing-slashes dir)
    (let ((end (string-skip-right dir #\/)))
      (if end (substring dir 0 (1+ end)) "/")))
  (define (search-path)
    (let loop ((dirs dirs) (seen '()))
      (cond ((null? dirs) (reverse seen))
            ((not (string-prefix? "/" (car dirs))) (loop (cdr dirs) seen))
            (else (let ((dir (strip-trailing-slashes (car dirs))))
                    (loop (cdr dirs)
                          (if (member dir seen) seen (cons dir seen))))))))
  (for-each
   (lambda (dir)
     (let ((file (string-append dir "/floatile/config.scm")))
       (when (file-exists? file)
         (catch #t
           (lambda () (primitive-load file))
           (lambda (key . args)
             (set! errors (cons (format #f "~a: ~a ~s" file key args) errors)))))))
   (search-path))
  (unless (null? errors)
    (error "configuration extensions failed:" (reverse errors))))
