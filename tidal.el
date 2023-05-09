;;; tidal.el --- Interact with TidalCycles for live coding patterns  -*- lexical-binding: t; -*-

;; Copyright (C) 2012  alex@slab.org
;; Copyright (C) 2006-2008  rohan drape (hsc3.el)

;; Author: alex@slab.org
;; Homepage: https://github.com/tidalcycles/Tidal
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((haskell-mode "16") (emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; notes from hsc3:
;; This mode is implemented as a derivation of `haskell' mode,
;; indentation and font locking is courtesy that mode.  The
;; inter-process communication is courtesy `comint'.  The symbol at
;; point acquisition is courtesy `thingatpt'.  The directory search
;; facilities are courtesy `find-lisp'.

;;; Code:

(require 'scheme)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)
(require 'pulse)
(require 'haskell-mode)
(require 'subr-x)

(defvar tidal-buffer
  "*tidal*"
  "*The name of the tidal process buffer (default=*tidal*).")

(defvar tidal-interpreter
  "ghci"
  "*The haskell interpeter to use (default=ghci).")

(defvar tidal-interpreter-version
  (substring (shell-command-to-string (concat tidal-interpreter " --numeric-version")) 0 -1)
  "*The version of tidal interpreter as a string.")

(defvar tidal-interpreter-arguments
  ()
  "*Arguments to the haskell interpreter (default=none).")

(defvar tidal-boot-script-path
  (let ((filepath
         (cond
          ((string-equal system-type "windows-nt")
           '(("path" . "echo off && for /f %a in ('ghc-pkg latest tidal') do (for /f \"tokens=2\" %i in ('ghc-pkg describe %a ^| findstr data-dir') do (echo %i))")
             ("separator" . "\\")))
          ((or (string-equal system-type "darwin") (string-equal system-type "gnu/linux"))
           '(("path" . "echo -n data-dir: && ghc -e 'import Paths_tidal' -e 'getDataDir>>=putStr' 2>/dev/null")
             ("separator" . "/") )))))
    (concat
     (string-trim (cadr (split-string
                         (shell-command-to-string (cdr (assoc "path" filepath))) ":")))
     (cdr (assoc "separator" filepath))
     "BootTidal.hs"))
  "*Full path to BootTidal.hs (inferred by introspecting ghc-pkg package db).")

(defvar tidal-literate-p
  t
  "*Flag to indicate if we are in literate mode (default=t).")

(defvar tidal-modules nil
  "Additional module imports.  See `tidal-run-region'.")

(make-variable-buffer-local 'tidal-literate-p)

(defun tidal-unlit (s)
  "Remove bird literate markup in S."
  (replace-regexp-in-string "^> " "" s))

(defun tidal-intersperse (e l)
  "Insert E between every element of list L."
  (when l
    (cons e (cons (car l) (tidal-intersperse e (cdr l))))))

;;;###autoload
(defun tidal-start-haskell ()
  "Start haskell."
  (interactive)
  (save-window-excursion
    (if (comint-check-proc tidal-buffer)
        (when (yes-or-no-p
               "A tidal process is already running.  Do you want to restart it? ")
          (tidal-restart-haskell))
      (apply
       'make-comint-in-buffer
       "tidal"
       tidal-buffer
       tidal-interpreter
       nil
       tidal-interpreter-arguments)
      (tidal-see-output)
      (tidal-send-string (concat ":script " tidal-boot-script-path))))
  (switch-to-buffer-other-window tidal-buffer))

;;;###autoload
(defalias 'run-tidal #'tidal-start-haskell
  "Start tidal in the haskell interpreter.")

(defun tidal-see-output ()
  "Show haskell output."
  (interactive)
  (when (comint-check-proc tidal-buffer)
    (delete-other-windows)
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

(defun tidal-restart-haskell ()
  "Restart haskell."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (tidal-quit-haskell))
  (tidal-start-haskell))

(defun tidal-chunk-string (n s)
  "Split a string S into chunks of N characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (tidal-chunk-string n (substring s n))))))

(defun tidal-send-string (s)
  "Send string S to tidal."
  (if (comint-check-proc tidal-buffer)
      (let ((cs (tidal-chunk-string 64 (concat s "\n"))))
        (mapcar (lambda (c) (comint-send-string tidal-buffer c)) cs))
    (error "No tidal process running?")))

(defun tidal-transform-and-store (f s)
  "Transform text into compilable form (Using file F and string S)."
  (with-temp-file f
    (mapc (lambda (module)
            (insert (concat module "\n")))
          tidal-modules)
    (insert "main = do\n")
    (insert (if tidal-literate-p (tidal-unlit s) s))))


(defun tidal-get-now ()
  "Store the current cycle position in a tidal variable called `now'."
  (interactive)
  (tidal-send-string "now' <- getNow")
  (tidal-send-string "let now = nextSam now'")
  (tidal-send-string "let retrig = (now `rotR`)")
  (tidal-send-string "let fadeOut n = spread' (_degradeBy) (retrig $ slow n $ envL)")
  (tidal-send-string "let fadeIn n = spread' (_degradeBy) (retrig $ slow n $ (1-) <$> envL)"))

(defun tidal-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  ;;(tidal-get-now)
  (let* ((s (buffer-substring (line-beginning-position)
                              (line-end-position)))
         (s* (if tidal-literate-p
                 (tidal-unlit s)
               s)))
    (tidal-send-string s*))
  (pulse-momentary-highlight-one-line (point))
  (forward-line))

(defun tidal-eval-multiple-lines ()
  "Eval the current region in the interpreter as a single line."
  ;;(tidal-get-now)
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
    (pulse-momentary-highlight-region (mark) (point))))

(defun tidal-run-multiple-lines ()
  "Send the current region to the interpreter as a single line."
  (interactive)
  (if (>= emacs-major-version 25)
      (save-mark-and-excursion
        (tidal-eval-multiple-lines))
    (save-excursion
      (tidal-eval-multiple-lines))))

(defmacro tidal-create-runner-run (name)
  "Macro to generate `d1' style pattern runners with NAME."
  (let ((run-fname (intern (concat "tidal-run-"  name))))
    `(defun ,run-fname ()
       ,(format "Send the %s interpreter as a single line." name)
       (interactive)
       (goto-char 0)
       (search-forward ,name nil nil 1)
       (tidal-run-multiple-lines))))

(defmacro tidal-create-runner-stop (name)
  "Macro to generate `d1' style pattern runners with NAME."
  (let ((stop-fname (intern (concat "tidal-stop-"  name))))
    `(defun ,stop-fname ()
       ,(format "Send %s $ silence as a single line." name)
       (interactive)
       (tidal-send-string ":{")
       (tidal-send-string (concat " mapM_ ($ silence) [" ,name "]"))
       (tidal-send-string ":}"))))

(defun tidal-create-runner (name)
  "Generate `d1' style pattern runners with NAME.
Two functions will be created, `tidal-run-NAME' and `tidal-stop-NAME'"
  (eval `(tidal-create-runner-run ,name))
  (eval `(tidal-create-runner-stop ,name)))

;; Generate the functions `tidal-run-d1' and `tidal-stop-d1'
(tidal-create-runner "d1")

;; This generates tidal-run-* and tidal-stop-* functions for d1 to d10.
(mapc #'tidal-create-runner
      '("d1" "d2" "d3" "d4" "d5" "d6" "d7" "d8" "d9" "d10"))

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

(defun tidal-hush ()
  "Stop all the patterns currently running."
  (interactive)
  (tidal-send-string "hush"))

(defun tidal-interrupt-haskell ()
  "Interrupt running process."
  (interactive)
  (if (comint-check-proc tidal-buffer)
      (with-current-buffer tidal-buffer
        (interrupt-process (get-buffer-process (current-buffer))))
    (error "No tidal process running?")))

(defvar tidal-mode-map nil
  "Tidal keymap.")

(defun tidal-mode-keybindings (map)
  "Haskell Tidal keybindings MAP."
  (define-key map [?\C-c ?\C-s] 'tidal-start-haskell)
  (define-key map [?\C-c ?\C-v] 'tidal-see-output)
  (define-key map [?\C-c ?\C-q] 'tidal-quit-haskell)
  (define-key map [?\C-c ?\C-c] 'tidal-run-line)
  (define-key map [?\C-c ?\C-e] 'tidal-run-multiple-lines)
  (define-key map (kbd "<C-return>") 'tidal-run-multiple-lines)
  (define-key map [?\C-c ?\C-r] 'tidal-run-region)
  (define-key map [?\C-c ?\C-l] 'tidal-load-buffer)
  (define-key map [?\C-c ?\C-i] 'tidal-interrupt-haskell)
  (define-key map [?\C-c ?\C-m] 'tidal-run-main)
  (define-key map [?\C-c ?\C-h] 'tidal-hush)
  (define-key map [?\C-c ?\C-1] 'tidal-run-d1)
  (define-key map [?\C-c ?\C-2] 'tidal-run-d2)
  (define-key map [?\C-c ?\C-3] 'tidal-run-d3)
  (define-key map [?\C-c ?\C-4] 'tidal-run-d4)
  (define-key map [?\C-c ?\C-5] 'tidal-run-d5)
  (define-key map [?\C-c ?\C-6] 'tidal-run-d6)
  (define-key map [?\C-c ?\C-7] 'tidal-run-d7)
  (define-key map [?\C-c ?\C-8] 'tidal-run-d8)
  (define-key map [?\C-c ?\C-9] 'tidal-run-d9)
  (define-key map [?\C-c ?\C-0] 'tidal-run-d10)
  (define-key map [?\C-v ?\C-1] 'tidal-stop-d1)
  (define-key map [?\C-v ?\C-2] 'tidal-stop-d2)
  (define-key map [?\C-v ?\C-3] 'tidal-stop-d3)
  (define-key map [?\C-v ?\C-4] 'tidal-stop-d4)
  (define-key map [?\C-v ?\C-5] 'tidal-stop-d5)
  (define-key map [?\C-v ?\C-6] 'tidal-stop-d6)
  (define-key map [?\C-v ?\C-7] 'tidal-stop-d7)
  (define-key map [?\C-v ?\C-8] 'tidal-stop-d8)
  (define-key map [?\C-v ?\C-9] 'tidal-stop-d9)
  (define-key map [?\C-v ?\C-0] 'tidal-stop-d10))

(defun tidal-turn-on-keybindings ()
  "Haskell Tidal keybindings in the local map."
  (local-set-key [?\C-c ?\C-s] 'tidal-start-haskell)
  (local-set-key [?\C-c ?\C-v] 'tidal-see-output)
  (local-set-key [?\C-c ?\C-q] 'tidal-quit-haskell)
  (local-set-key [?\C-c ?\C-c] 'tidal-run-line)
  (local-set-key [?\C-c ?\C-e] 'tidal-run-multiple-lines)
  (local-set-key (kbd "<C-return>") 'tidal-run-multiple-lines)
  (local-set-key [?\C-c ?\C-r] 'tidal-run-region)
  (local-set-key [?\C-c ?\C-l] 'tidal-load-buffer)
  (local-set-key [?\C-c ?\C-i] 'tidal-interrupt-haskell)
  (local-set-key [?\C-c ?\C-m] 'tidal-run-main)
  (local-set-key [?\C-c ?\C-h] 'tidal-hush)
  (local-set-key [?\C-c ?\C-1] 'tidal-run-d1)
  (local-set-key [?\C-c ?\C-2] 'tidal-run-d2)
  (local-set-key [?\C-c ?\C-3] 'tidal-run-d3)
  (local-set-key [?\C-c ?\C-4] 'tidal-run-d4)
  (local-set-key [?\C-c ?\C-5] 'tidal-run-d5)
  (local-set-key [?\C-c ?\C-6] 'tidal-run-d6)
  (local-set-key [?\C-c ?\C-7] 'tidal-run-d7)
  (local-set-key [?\C-c ?\C-8] 'tidal-run-d8)
  (local-set-key [?\C-c ?\C-9] 'tidal-run-d9)
  (local-set-key [?\C-c ?\C-0] 'tidal-run-d10)
  (local-set-key [?\C-v ?\C-1] 'tidal-stop-d1)
  (local-set-key [?\C-v ?\C-2] 'tidal-stop-d2)
  (local-set-key [?\C-v ?\C-3] 'tidal-stop-d3)
  (local-set-key [?\C-v ?\C-4] 'tidal-stop-d4)
  (local-set-key [?\C-v ?\C-5] 'tidal-stop-d5)
  (local-set-key [?\C-v ?\C-6] 'tidal-stop-d6)
  (local-set-key [?\C-v ?\C-7] 'tidal-stop-d7)
  (local-set-key [?\C-v ?\C-8] 'tidal-stop-d8)
  (local-set-key [?\C-v ?\C-9] 'tidal-stop-d9)
  (local-set-key [?\C-v ?\C-0] 'tidal-stop-d10))

(defun tidal-mode-menu (map)
  "Haskell Tidal menu MAP."
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

(unless tidal-mode-map
  (let ((map (make-sparse-keymap "Haskell-Tidal")))
    (tidal-mode-keybindings map)
    (tidal-mode-menu map)
    (setq tidal-mode-map map)))

;;;###autoload
(define-derived-mode
  literate-tidal-mode
  tidal-mode
  "Literate Haskell Tidal"
  "Major mode for interacting with an inferior haskell process."
  (set (make-local-variable 'paragraph-start) "\f\\|[ \t]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$")
  (setq tidal-literate-p t)
  (setq haskell-literate 'bird)
  (turn-on-font-lock))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ltidal\\'" . literate-tidal-mode))

;;;###autoload
(define-derived-mode
  tidal-mode
  haskell-mode
  "Haskell Tidal"
  "Major mode for interacting with an inferior haskell process."
  (set (make-local-variable 'paragraph-start) "\f\\|[ \t]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$")
  (setq tidal-literate-p nil)
  (turn-on-font-lock))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tidal\\'" . tidal-mode))

(provide 'tidal)
;;; tidal.el ends here
