;;; org-ref-vis.el --- org-ref-vis.el  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-ref-ok/org-ref-vis.el
;; Version: 0.1.1
;; Keywords: convenience
;; Package-Requires: ((emacs "30.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; WIP.
;;
;;; Code:

(require 'org-ref)
(require 'citeproc)
(require 's)
(require 'seq)

(defcustom org-ref-vis-csl-dir
  "/usr/share/citation-style-language"
  "The CSL directory path.
On Debian, you need the following packages:

  - citation-style-language-locales
  - citation-style-language-styles

to fill the necessary files under this directory tree."
  :type 'directory
  :group 'org-ref)

(defcustom org-ref-vis-style-getter
  "chicago-note-bibliography.csl"
  "A function that takes COMMAND LANG."
  :type '(choice (function :tag "Function")
                 (file :tag "Path to CSL style file"))
  :group 'org-ref)

(defvar org-ref-vis-bib-files nil)
(defvar org-ref-vis-itemgetter nil)
(defvar org-ref-vis-types nil)
(defvar org-ref-vis-citeproc-modes
  '(author-only
    bib-entry
    locator-only
    nil
    suppress-author
    textual
    title-only
    year-only))

(defcustom org-ref-vis-command-alist
  '((("cite" "Cite" "parencite" "Parencite") . ((nil) "${nil}"))
    (("citeauthor" "citeauthor*") . ((author-only) "${author-only}"))
    (("citetitle" "citetitle*" "citeurl") . ((title-only) "${title-only}"))
    (("citeyear" "citeyear*") . ((year-only) "${year-only}"))
    ;; (("footfullcite") . ((nil) "${nil}"))
    (("fullcite") . ((bib-entry) "${bib-entry}"))
    ;; (("textcite") . ((author-only year-only) "${author-only} (${year-only})"))
    ;; (("textcite-bare") . ((author-only year-only) "${author-only} ${year-only}"))
    )
  "Mapping of citation commands to a list of (MODES FORMAT-STRING)."
  :type 'alist
  :set (lambda (sym val)
         (set-default sym val)
         (setq org-ref-vis-types
               (seq-mapcat #'car org-ref-vis-command-alist)))
  :group 'org-ref)

(defvar org-ref-vis-links-generator #'org-dynamic-links-default-generator)

;;;###autoload
(define-minor-mode org-ref-vis-mode
  "Minor mode for org-ref-vis support."
  :lighter " orvis"
  :group 'org-ref
  (pcase org-ref-vis-mode
    ('t (org-ref-vis-mode--on))
    (_ (org-ref-vis-mode--off))))

(defun org-ref-vis-mode--on ()
  "Activate `org-ref-vis-mode'."
  (advice-add #'bibtex-completion-clear-cache :after
              #'org-ref-vis--cache-clear)
  (advice-add #'citar-cache--update-bibliography :after
              #'org-ref-vis--cache-clear)
  (advice-add #'org-activate-links :around
              #'org-ref-vis--activate-links-ad)
  ;; Ensure font-lock natively tracks and cleans up the 'display property when redrawing
  (make-local-variable 'font-lock-extra-managed-props)
  (add-to-list 'font-lock-extra-managed-props 'display)
  (when (derived-mode-p 'org-mode)
    (org-restart-font-lock)))

(defun org-ref-vis-mode--off ()
  "Deactivate `org-ref-vis-mode'."
  (advice-remove #'org-activate-links
                 #'org-ref-vis--activate-links-ad)
  (setq font-lock-extra-managed-props
        (remove 'display font-lock-extra-managed-props))
  (when (derived-mode-p 'org-mode)
    (org-restart-font-lock))
  (advice-remove #'bibtex-completion-clear-cache #'org-ref-vis--cache-clear))

(defun org-ref-vis--cache-clear (&rest _args)
  "Reset item getter."
  (setq org-ref-vis-itemgetter nil))

(defun org-ref-vis-csl-locale-getter ()
  "Return CSL locale getter function.
In Debian, the directory is installed with the citation-style-language-locales
package."
  (let ((dir (file-name-concat org-ref-vis-csl-dir "locales")))
    (if (file-directory-p dir)
        (citeproc-locale-getter-from-dir dir)
      (error "CSL locales directory not found: %s" dir))))

(defun org-ref-vis-get-itemgetter (bib-files)
  "Return itemgetter function for BIB-FILES.
If the item getter function exists for BIB-FILES, it will be reused. If not, the
function will be created and cached to `org-ref-vis-itemgetter'."
  (if (and org-ref-vis-itemgetter
           (equal bib-files org-ref-vis-bib-files))
      org-ref-vis-itemgetter
    (setq org-ref-vis-bib-files bib-files
          org-ref-vis-itemgetter (citeproc-hash-itemgetter-from-any bib-files))))

(defun org-ref-vis-item-lang (item)
  "TBD."
  (or (cdr (assoc "language" item))
      (cdr (assoc 'language item))
      "en-US"))

(defun org-ref-vis-csl-file (command lang)
  "TBD."
  (cond ((functionp org-ref-vis-style-getter)
         (funcall org-ref-vis-style-getter command lang))
        ((stringp org-ref-vis-style-getter)
         (if (file-name-absolute-p org-ref-vis-style-getter)
             org-ref-vis-style-getter
           (file-name-concat org-ref-vis-csl-dir
                             "styles"
                             org-ref-vis-style-getter)))))

(defun org-ref-vis--citeproc-create (it-getter lang command)
  "Create CSL processor for LANG and COMMAND."
  (if-let* ((csl-style (org-ref-vis-csl-file command lang)))
      (let* ((loc-getter (org-ref-vis-csl-locale-getter)))
        (citeproc-create csl-style it-getter loc-getter lang t))
    (error "No CSL style found for %s, %s" command lang)))

(defun org-ref-vis--get-command-spec (command)
  "Look up COMMAND in `org-ref-vis-command-alist'.
Returns a list containing (MODES FORMAT-STRING). Defaults to ((nil) \"%s\")."
  (let ((entry (seq-find (lambda (x) (member command (car x)))
                         org-ref-vis-command-alist)))
    (if entry
        (cdr entry)
      '((nil) "%s"))))

(defun org-ref-vis-find-bibliography ()
  (or (org-ref-find-bibliography)
      citar-bibliography))

(defun org-ref-vis-render (citekey command &optional _output-fmt)
  "Render CITEKEY according to COMMAND format."
  (pcase-let ((`(,modes ,fmt) (org-ref-vis--get-command-spec command)))
    (if-let* ((ig (org-ref-vis-get-itemgetter (org-ref-vis-find-bibliography)))
              (item (cdr (assoc citekey (funcall ig (list citekey)))))
              (lang (org-ref-vis-item-lang item))
              (proc (org-ref-vis--citeproc-create ig lang command))
              (citations (mapcar
                          (lambda (mode)
                            (when (member mode org-ref-vis-citeproc-modes)
                              (citeproc-citation-create
                               :cites (list (list (cons 'id citekey)))
                               :mode mode
                               :suppress-affixes t)))
                          modes)))
        (progn
          (citeproc-append-citations citations proc)
          (let* ((rendered (citeproc-render-citations
                            proc (or _output-fmt 'org) 'no-links))
                 (al (cl-pairlis modes rendered)))
            (s-format fmt
                      (lambda (key)
                        (setq key (intern key))
                        (if (member key org-ref-vis-citeproc-modes)
                            (alist-get key al)
                          (alist-get key item))))))
      (error "Item cannot be rendered (%s)" citekey))))

(defun org-ref-vis-render-isolated (citekey command &optional _output-fmt)
  "Render CITEKEY according to COMMAND format.
This version renders isolated references using `citeproc-create-style' and
`citeproc-render-item'."
  (if-let* ((ig (org-ref-vis-get-itemgetter (org-ref-vis-find-bibliography)))
            (item (cdr (assoc citekey (funcall ig (list citekey)))))
            (lang (org-ref-vis-item-lang item))
            (lg (org-ref-vis-csl-locale-getter))
            (style (citeproc-create-style (org-ref-vis-csl-file command lang) lg)))
      (citeproc-render-item item style 'bib (or _output-fmt 'org))
    (error "Item cannot be rendered (%s)" citekey)))

(defun org-ref-vis--propertize (str &optional face)
  "Convert plain Org text tokens in STR into proper face properties.
When given, FACE is applied additionally."
  (let* ((s str)

         ;; BUG(2026-06-05): Somehow, `citeproc-render-item' can produce
         ;; Org-rendered string including these HTML tags. Strip them here.
         (case-fold-search t)
         (s (replace-regexp-in-string
             "<Span Class=\"Nocase\">\\|</Span>" "" s))

         (s (with-temp-buffer
              (insert s)
              (org-mode)
              (font-lock-ensure)
              (buffer-string))))
    (when face
      (add-face-text-property 0 (length s) face t s))
    s))

(defun org-dynamic-links-default-generator (lnk)
  "A default fallback generator that prepends a helper icon to the PATH."
  (let ((re (regexp-opt (org-link-types) t)))
    (if (string-match (format "^%s:\\(.*\\)$" re) lnk)
        (let ((type (match-string 1 lnk))
              (path (match-string 2 lnk)))
          (cond
           ((member type org-ref-vis-types)
            (let* ((citekey (when (string-match "\\`&\\(.*\\)" path)
                              (match-string 1 path)))
                   ;; (args (list (org-ref-vis-render citekey type)
                   ;;             'org-ref-cite-face))
                   (args
                    (condition-case err
                        (list (org-ref-vis-render citekey type)
                              'org-ref-cite-face)
                      (error
                       (list (format "err:%s:%s:%s"
                                     type citekey
                                     (error-message-string err))
                             'org-ref-bad-cite-key-face))))
                   )
              (apply #'org-ref-vis--propertize args)))
           (t nil)))
      nil)))

(defun org-ref-vis--activate-links-ad (fun _limit)
  "Around-advice wrapper for FUN (`org-activate-links').
Intercepts font-lock execution to inject dynamic display strings."
  (if-let* ((start-pos (point))
            (ret (funcall fun _limit)))
      (if org-link-descriptive
          (catch :exit
            (save-excursion
              (goto-char start-pos)
              (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" _limit t)
                (when-let* ((beg (match-beginning 0))
                            (end (match-end 0))
                            (lnk (match-string 1))
                            (text (funcall org-ref-vis-links-generator lnk)))
                  (put-text-property beg end 'display text)
                  (throw :exit t))))
            ret)
        ret)))

(provide 'org-ref-vis)
;;; org-ref-vis.el ends here
