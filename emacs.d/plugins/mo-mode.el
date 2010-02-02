;;; mo-mode.el --- view and edit gettext .mo message files

;; Copyright 2006, 2007, 2008, 2009 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 6
;; Keywords: data
;; URL: http://user42.tuxfamily.org/mo-mode/index.html
;; EmacsWiki: MoMode

;; mo-mode.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; mo-mode.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code adds decoding and viewing of .mo and .gmo message catalogue
;; files from Gettext (http://www.gnu.org/software/gettext/).
;;
;; A format-alist entry does the conversion between binary MO data and PO
;; style text in a buffer, using the msgfmt and msfunfmt programs.  The
;; `mo-mode' function then merely ensures that transformation has been
;; applied, and switches to either po-mode or text-mode.
;;
;; In Emacs 22, once format-alist is setup it actually works to have
;; `po-mode' directly for .mo files, instead of the helper `mo-mode'.  But
;; `mo-mode' makes it easier to autoload the code here, it ensures decoding
;; works from tar-mode and archive-mode, and it fixes multibyteness when
;; visiting in emacs 21.
;;
;; Note that `tar-mode' (as of Emacs 22) doesn't follow `buffer-file-format'
;; when saving so if you rewrite a .mo inside a .tar you get the PO text.
;; This afflicts all file format things, including the builtin
;; `enriched-mode'.  So don't do that.  `archive-mode' saving is ok.

;;; Install:

;; Put mo-mode.el in one of your `load-path' directories and the following
;; in your .emacs
;;
;;     (autoload 'mo-mode "mo-mode" nil t)
;;     (modify-coding-system-alist 'file "\\.g?mo\\'" 'raw-text-unix)
;;     (add-to-list 'auto-mode-alist '("\\.g?mo\\'" . mo-mode))
;;
;; There's autoload tags below for this, if you like to use
;; update-file-autoloads and friends.

;;; History:

;; Version 1 - the first version.
;; Version 2 - work in xemacs, note need raw-text-unix in install above.
;; Version 3 - go through format-alist, to make rewrite work.
;; Version 4 - `display-warning' not available in emacs 21.
;; Version 5 - namespace clean on xemacs bits
;; Version 6 - eval-when-compile on the emacs version variations

;;; Emacsen:

;; Designed for Emacs 21 and 22, works in XEmacs 21.


;;; Code:

;;;###autoload
(modify-coding-system-alist 'file "\\.g?mo\\'" 'raw-text-unix)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.g?mo\\'" . mo-mode))


;; xemacs incompatibilities
(defalias 'mo-mode-make-temp-file
  (if (eval-when-compile (fboundp 'make-temp-file))
      'make-temp-file   ;; emacs
    ;; xemacs21
    (autoload 'mm-make-temp-file "mm-util") ;; from gnus
    'mm-make-temp-file))
(defalias 'mo-mode-set-buffer-multibyte
  (if (eval-when-compile (fboundp 'set-buffer-multibyte))
      'set-buffer-multibyte  ;; emacs
    'identity))              ;; not applicable in xemacs21


(add-to-list 'format-alist
             `(gettext-mo
               "msgfmt encoded Gettext .mo or .gmo message catalogue."
               ;; Magic intros per "MO Files" node in the gettext manual.
               ;; The two alternatives are big or little endian (msgunfmt
               ;; can read both no matter what the native host endianness).
               ,(concat "\\`\\(" (string #xDE #x12 #x04 #x95)
                        "\\|"    (string #x95 #x04 #x12 #xDE) "\\)")
               mo-mode-decode
               mo-mode-encode
               t
               nil))

(defun mo-mode-decode (beg end)
  "Run msgunfmt on raw MO bytes in the current buffer.
This function is for use from `format-alist'.

The buffer should be unibyte and contain raw MO bytes, as per a
`raw-text-unix' read.  Those bytes are put through msgunfmt to
get the message text, and the buffer switched to multibyte.
Character encoding is taken from the Content-Type in the file
intro.

An error is thrown if msgunfmt can't be run or the buffer
contents are invalid."

  (save-excursion
    (save-restriction
      (narrow-to-region beg end)

      (mo-mode-run-fmt "msgunfmt" 'raw-text-unix nil)
      (let ((coding (mo-mode-buffer-coding nil)))
        (when coding
          (decode-coding-region (point-min) (point-max) coding)
          (mo-mode-set-buffer-multibyte t)))

      (point-max))))

(defun mo-mode-encode (beg end buffer)
  "Run msgfmt on PO text in the current buffer.
This function is for use from `format-alist'.

The buffer text is put through msgfmt to produce binary
data (`raw-text-unix'), which replaces the text, and the buffer
is switched to unibyte.

The coding system for the text is taken from the Content-Type in
the intro msgstr.  An error is thrown if msgfmt can't be run or
the buffer contents are invalid."

  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (mo-mode-run-fmt "msgfmt" (mo-mode-buffer-coding t) buffer)
      (point-max))))

(defvar mo-mode-originating-buffer nil
  "Originating PO text buffer for *mo-mode-errors*.")
(make-variable-buffer-local 'mo-mode-originating-buffer)

(defadvice compilation-find-file (around mo-mode activate)
  "Use `mo-mode-originating-buffer' for mo-mode errors."
  (if (and (equal filename "<stdin>")
           mo-mode-originating-buffer)
      (setq ad-return-value mo-mode-originating-buffer)
    ad-do-it))

(defun mo-mode-run-fmt (command write-coding originating-buffer)
  "Run msgfmt or msgunfmt on the current buffer.
COMMAND is a string \"msgfmt\" or \"msgunfmt\", the program to
run.  Program output replaces the buffer contents.

WRITE-CODING is the coding system for writing the buffer
contents.  The read back is `raw-text-unix', which means the
buffer is forced to unibyte.

ORIGINATING-BUFFER is the source buffer for when running msgfmt.
This might be different from the current buffer.  It's recorded
as the place to go for error lookups."

  (let ((errfile (mo-mode-make-temp-file "mo-mode-")))
    (unwind-protect
        (let ((status (let ((coding-system-for-write write-coding)
                            (coding-system-for-read  'raw-text-unix))
                        (call-process-region
                         (point-min) (point-max) command
                         t              ;; delete old
                         (list t        ;; stdout here
                               errfile) ;; stderr to file
                         nil            ;; no redisplay
                         "-"          ;; read stdin
                         "-o" "-")))) ;; write stdout

          (unless (eq 0 status)
            (switch-to-buffer "*mo-mode-errors*")
            (setq buffer-read-only nil)
            (erase-buffer)
            (insert-file-contents errfile)
            (if originating-buffer
                (compilation-mode))
            (setq mo-mode-originating-buffer originating-buffer)
            (error "%s error, see *mo-mode-errors* buffer" command)))

      (delete-file errfile))))

(defun mo-mode-buffer-coding (must-find)
  "Return the Emacs coding system for PO buffer text.
The current buffer is the output of msgunfmt and the Content-Type
header is found and used to select a coding system.

If MUST-FIND is nil, then if no coding system can be found a
warning is shown and the return is nil.  If MUST-FIND is non-nil
then an error is thrown instead."

  (save-excursion
    (goto-char (point-min))
    (if (not (re-search-forward
              "^\"Content-Type: text/plain;[ \t]*charset=\\(.*\\)\\\\n\""
              nil t))
        (progn
          (if must-find
              (error "MO Content-Type charset not found"))
          (if (eval-when-compile (fboundp 'display-warning)) ;; not in emacs21
              (display-warning 'mo-mode "MO Content-Type charset not found")
            (message "MO Content-Type charset not found"))
          nil)

      (let* ((charset (match-string 1))
             (coding  (mo-mode-charset-to-coding-system charset)))
        (unless coding
          (if must-find
              (error "MO Content-Type charset unknown: %s" charset))
          (if (eval-when-compile (fboundp 'lwarn)) ;; not in emacs21
              (lwarn 'mo-mode 'warning
                     "MO Content-Type charset unknown: %s" charset)
            (message "MO Content-Type charset unknown: %s" charset)))
        coding))))

(defun mo-mode-charset-to-coding-system (charset)
  "Return an Emacs coding system for the given po file CHARSET.
CHARSET is a string name and is normally in libc style the same
as nl_langinfo(CODESET) gives or iconv_open() accepts.

In Emacs 22 this is simply `locale-charset-to-coding-system'.
For earlier versions it tries `po-content-type-charset-alist' and
`mm-charset-to-coding-system'."

  ;; there's no easy way to re-use `po-find-file-coding-system' because the
  ;; code there insists on doing insert-file-contents :-(
  ;; in any case would like to work when that's not available (emacs 21
  ;; without the emacs bits of gettext installed)

  (or
   ;; locale-charset-to-coding-system is pretty much designed exactly for
   ;; this, if it's available
   (and (eval-when-compile
          (fboundp 'locale-charset-to-coding-system)) ;; emacs 22
        (locale-charset-to-coding-system charset))

   ;; po-content-type-charset-alist, if available
   ;; po.el is from emacs 22
   ;; po-compat.el is from standalone gettext
   (and (or (condition-case nil (require 'po)        (error nil))
            (condition-case nil (require 'po-compat) (error nil)))
        (boundp 'po-content-type-charset-alist)
        (cdr (assoc (upcase charset) po-content-type-charset-alist)))

   ;; otherwise try the gnus mime-ish lookup, it can mung names suitably
   (progn (eval-and-compile ;; quieten byte compiler
            (require 'mm-util))
          (let ((coding (mm-charset-to-coding-system charset)))
            ;; the return is 'ascii for for ascii or us-ascii, but that's not
            ;; a coding system, go undecided instead
            (if (and (eq coding 'ascii)
                     (not (coding-system-p coding)))
                (setq coding 'undecided))
            coding))))

;;;###autoload
(defun mo-mode ()
  "Decode and view gettext .mo or .gmo message files.
The raw MO bytes should have been decoded by the `format-alist'
mechanism already, but if not then it's done here explicitly.
Then major mode `po-mode' from Gettext is used if available, or
`text-mode' if not.

The mo-mode home page is
URL `http://user42.tuxfamily.org/mo-mode/index.html'"
  (interactive)

  (if (memq 'gettext-mo buffer-file-format)
      ;; in emacs 21 the set-buffer-multibyte within mo-mode-decode is
      ;; somehow not preserved by the format system, force it now
      (mo-mode-set-buffer-multibyte t)

    ;; in emacs 21 and 22 tar-mode and archive-mode, formats are not applied
    ;; to files extracted from archives, force it now
    (let ((buffer-read-only nil))
      (format-decode-buffer 'gettext-mo)))

  (if (fboundp 'po-mode) ;; if loaded or set for autoload
      (po-mode)
    (text-mode)))

(provide 'mo-mode)

;;; mo-mode.el ends here
