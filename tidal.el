;; tidal.el - (c) alex@slab.org, 20012, based heavily on...
;; hsc3.el - (c) rohan drape, 2006-2008

;; notes from hsc3:
;; This mode is implemented as a derivation of `haskell' mode,
;; indentation and font locking is courtesy that mode.  The
;; inter-process communication is courtesy `comint'.  The symbol at
;; point acquisition is courtesy `thingatpt'.  The directory search
;; facilities are courtesy `find-lisp'.

(require 'scheme)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)

(defun tidal-highlight-tail-make-new-overlay (start-point
                                            end-point
                                            )
  "Make new highlight in the current point."
  (let* ((point-face-bgcolor-hex nil))
    ;; remove any highlight-tail's overlays at point
    (let ((overlays-at-start-point (highlight-tail-overlays-at start-point))
          highlight-tail-overlay)
      (mapcar '(lambda (overlay)
                 (when (highlight-tail-overlay-get overlay 'highlight-tail)
                   (setq highlight-tail-overlay overlay)))
              overlays-at-start-point)
      (when highlight-tail-overlay
        (add-to-list 'highlight-tail-deleted-overlays-list
                     highlight-tail-overlay)
        (remhash highlight-tail-overlay highlight-tail-overlays-hash)
        (highlight-tail-delete-overlay highlight-tail-overlay)))
    ;; do we need to fade out to default color or any other
    (setq point-face-bgcolor-hex (highlight-tail-get-bgcolor-hex start-point))
    ;; add the overlay with good ending color
    (let (highlight-tail-overlay)
      (if (> (length highlight-tail-deleted-overlays-list) 0)
          ;; get overlay from list of deleted overlays
          (progn
            (setq highlight-tail-overlay
                  (highlight-tail-move-overlay
                   (car highlight-tail-deleted-overlays-list)
                   start-point end-point
                   (current-buffer))) ; Xemacs needs it (or will highlight in
                                        ; other buffer)
            (setq highlight-tail-deleted-overlays-list
                  (cdr highlight-tail-deleted-overlays-list)))
        ;; make new overlay
        (setq highlight-tail-overlay
              (highlight-tail-make-overlay start-point end-point))
        (highlight-tail-overlay-put
         highlight-tail-overlay 'evaporate t)
        (highlight-tail-overlay-put
         highlight-tail-overlay 'highlight-tail t))
      (puthash highlight-tail-overlay
               (list point-face-bgcolor-hex
                     1)             ; first step in fading-out
               highlight-tail-overlays-hash)
      (highlight-tail-overlay-put
       highlight-tail-overlay 'face
       (intern
        (concat "highlight-tail-face-"
                (format "%s" point-face-bgcolor-hex)
                "-1")))))) ; first step in fading out


(defvar tidal-buffer
  "*tidal*"
  "*The name of the tidal process buffer (default=*tidal*).")

(defvar tidal-interpreter
  "ghci"
  "*The haskell interpeter to use (default=ghci).")

(defvar tidal-interpreter-arguments
  (list "-XOverloadedStrings"
        )
  "*Arguments to the haskell interpreter (default=none).")

(defvar tidal-run-control
  "~/.tidal.hs"
  "*Run control file (default=~/.tidal.hs)")

(defvar tidal-modules
  (list "import Control.Concurrent"
        "import Control.Monad"
        "import Data.List"
        "import Control.Applicative"
        "import Parse"
        "import Pattern"
        "import Stream"
        "import Dirt"
        )
  "*List of modules (possibly qualified) to bring into interpreter context.")

(defvar tidal-literate-p
  t
  "*Flag to indicate if we are in literate mode (default=t).")

(make-variable-buffer-local 'tidal-literate-p)

(defun tidal-unlit (s)
  "Remove bird literate marks"
  (replace-regexp-in-string "^> " "" s))

(defun tidal-intersperse (e l)
  (if (null l)
      '()
    (cons e (cons (car l) (tidal-intersperse e (cdr l))))))

(defun tidal-write-default-run-control ()
  "Write default run control file if no file exists."
  (if (not (file-exists-p tidal-run-control))
      (with-temp-file
          tidal-run-control
        (mapc
         (lambda (s)
           (insert (concat s "\n")))
         tidal-modules))))

(defun tidal-start-haskell ()
  "Start haskell."
  (interactive)
  (if (comint-check-proc tidal-buffer)
      (error "A tidal process is already running")
    (apply
     'make-comint
     "tidal"
     tidal-interpreter
     nil
     tidal-interpreter-arguments)
    (tidal-see-output))
  (tidal-write-default-run-control)
  (tidal-send-string (concat ":l " tidal-run-control))
  (tidal-send-string ":set prompt \"tidal> \""))

(defun tidal-see-output ()
  "Show haskell output."
  (interactive)
  (when (comint-check-proc tidal-buffer)
    (delete-other-windows)
    (split-window-vertically)
    (with-current-buffer tidal-buffer
      (let ((window (display-buffer (current-buffer))))
	(goto-char (point-max))
	(save-selected-window
	  (set-window-point window (point-max)))))))

(defun tidal-quit-haskell ()
  "Quit haskell."
  (interactive)
  (kill-buffer tidal-buffer)
  (delete-other-windows))

(defun tidal-help ()
  "Lookup up the name at point in the Help files."
  (interactive)
  (mapc (lambda (filename)
	  (find-file-other-window filename))
	(find-lisp-find-files tidal-help-directory
			      (concat "^"
				      (thing-at-point 'symbol)
				      "\\.help\\.lhs"))))

(defun chunk-string (n s)
  "Split a string into chunks of 'n' characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (chunk-string n (substring s n))))))

(defun tidal-send-string (s)
  (if (comint-check-proc tidal-buffer)
      (let ((cs (chunk-string 64 (concat s "\n"))))
        (mapcar (lambda (c) (comint-send-string tidal-buffer c)) cs))
    (error "no tidal process running?")))

(defun tidal-transform-and-store (f s)
  "Transform example text into compilable form."
  (with-temp-file f
    (mapc (lambda (module)
	    (insert (concat module "\n")))
	  tidal-modules)
    (insert "main = do\n")
    (insert (if tidal-literate-p (tidal-unlit s) s))))

(defun tidal-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  (let* ((s (buffer-substring (line-beginning-position)
			      (line-end-position)))
	 (s* (if tidal-literate-p
		 (tidal-unlit s)
	       s)))
    (tidal-send-string s*))
  (tidal-highlight-tail-make-new-overlay (point-at-bol) (point-at-eol))
  (next-line)
  )

(defun tidal-run-multiple-lines ()
  "Send the current region to the interpreter as a single line."
  (interactive)
  (save-excursion
   (mark-paragraph)
   (let* ((s (buffer-substring-no-properties (region-beginning)
                                             (region-end)))
          (s* (if tidal-literate-p
                  (tidal-unlit s)
                s)))
     (tidal-send-string ":{")
     (tidal-send-string s*)
     (tidal-send-string ":}")
     (mark-paragraph)
     (tidal-highlight-tail-make-new-overlay (mark) (point))
     )
    ;(tidal-send-string (replace-regexp-in-string "\n" " " s*))
   )
  )

(defun tidal-run-region ()
  "Place the region in a do block and compile."
  (interactive)
  (tidal-transform-and-store
   "/tmp/tidal.hs"
   (buffer-substring-no-properties (region-beginning) (region-end)))
  (tidal-send-string ":load \"/tmp/tidal.hs\"")
  (tidal-send-string "main"))

(defun tidal-load-buffer ()
  "Load the current buffer."
  (interactive)
  (save-buffer)
  (tidal-send-string (format ":load \"%s\"" buffer-file-name)))

(defun tidal-run-main ()
  "Run current main."
  (interactive)
  (tidal-send-string "main"))

(defun tidal-interrupt-haskell ()
  (interactive)
  (if (comint-check-proc tidal-buffer)
      (with-current-buffer tidal-buffer
	(interrupt-process (get-buffer-process (current-buffer))))
    (error "no tidal process running?")))

(defvar tidal-mode-map nil
  "Tidal keymap.")

(defun tidal-mode-keybindings (map)
  "Haskell Tidal keybindings."
  (define-key map [?\C-c ?\C-s] 'tidal-start-haskell)
  (define-key map [?\C-c ?\C-v] 'tidal-see-output)
  (define-key map [?\C-c ?\C-q] 'tidal-quit-haskell)
  (define-key map [?\C-c ?\C-c] 'tidal-run-line)
  (define-key map [?\C-c ?\C-e] 'tidal-run-multiple-lines)
  (define-key map [?\C-c ?\C-r] 'tidal-run-region)
  (define-key map [?\C-c ?\C-l] 'tidal-load-buffer)
  (define-key map [?\C-c ?\C-i] 'tidal-interrupt-haskell)
  (define-key map [?\C-c ?\C-m] 'tidal-run-main)
  (define-key map [?\C-c ?\C-h] 'tidal-help))

(defun turn-on-tidal-keybindings ()
  "Haskell Tidal keybindings in the local map."
  (local-set-key [?\C-c ?\C-s] 'tidal-start-haskell)
  (local-set-key [?\C-c ?\C-v] 'tidal-see-output)
  (local-set-key [?\C-c ?\C-q] 'tidal-quit-haskell)
  (local-set-key [?\C-c ?\C-c] 'tidal-run-line)
  (local-set-key [?\C-c ?\C-e] 'tidal-run-multiple-lines)
  (local-set-key [?\C-c ?\C-r] 'tidal-run-region)
  (local-set-key [?\C-c ?\C-l] 'tidal-load-buffer)
  (local-set-key [?\C-c ?\C-i] 'tidal-interrupt-haskell)
  (local-set-key [?\C-c ?\C-m] 'tidal-run-main)
  (local-set-key [?\C-c ?\C-h] 'tidal-help))

(defun tidal-mode-menu (map)
  "Haskell Tidal menu."
  (define-key map [menu-bar tidal]
    (cons "Haskell-Tidal" (make-sparse-keymap "Haskell-Tidal")))
  (define-key map [menu-bar tidal help]
    (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar tidal expression]
    (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar tidal expression load-buffer]
    '("Load buffer" . tidal-load-buffer))
  (define-key map [menu-bar tidal expression run-main]
    '("Run main" . tidal-run-main))
  (define-key map [menu-bar tidal expression run-region]
    '("Run region" . tidal-run-region))
  (define-key map [menu-bar tidal expression run-multiple-lines]
    '("Run multiple lines" . tidal-run-multiple-lines))
  (define-key map [menu-bar tidal expression run-line]
    '("Run line" . tidal-run-line))
  (define-key map [menu-bar tidal haskell]
    (cons "Haskell" (make-sparse-keymap "Haskell")))
  (define-key map [menu-bar tidal haskell quit-haskell]
    '("Quit haskell" . tidal-quit-haskell))
  (define-key map [menu-bar tidal haskell see-output]
    '("See output" . tidal-see-output))
  (define-key map [menu-bar tidal haskell start-haskell]
    '("Start haskell" . tidal-start-haskell)))

(if tidal-mode-map
    ()
  (let ((map (make-sparse-keymap "Haskell-Tidal")))
    (tidal-mode-keybindings map)
    (tidal-mode-menu map)
    (setq tidal-mode-map map)))

(define-derived-mode
  literate-tidal-mode
  tidal-mode
  "Literate Haskell Tidal"
  "Major mode for interacting with an inferior haskell process."
  (setq tidal-literate-p t)
  (setq haskell-literate 'bird)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.ltidal$" . literate-tidal-mode))

(define-derived-mode
  tidal-mode
  haskell-mode
  "Haskell Tidal"
  "Major mode for interacting with an inferior haskell process."
  (setq tidal-literate-p nil)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.tidal$" . tidal-mode))

(provide 'tidal)
