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

(defcustom org-ref-vis-style-getter nil
  "A function that takes COMMAND LANG.")

(defvar org-ref-vis-csl-locales-dir "/usr/share/citation-style-language/locales/")
(defvar org-ref-vis-bib-files nil)
(defvar org-ref-vis-itemgetter nil)
(defvar-local org-ref-vis--active-overlay nil)

;;;###autoload
(define-minor-mode org-ref-vis-mode
  "Minor mode for org-ref-vis support."
  :lighter " orvis"
  :group 'org-ref
  (pcase org-ref-vis-mode
    ('t (org-ref-vis-mode--on))
    (_ (org-ref-vis-mode--off))))

(defvar org-ref-vis-types
  '("cite"
    "citeauthor"
    "citetitle"
    "citeyear"
    "footfullcite"
    "fullcite"
    "parencite"
    "textcite"))

(defun org-ref-vis-mode--on ()
  "Activate `org-ref-vis-mode'."
  (dolist (type org-ref-vis-types)
    (org-link-set-parameters
     type :display 'org-link
     :activate-func
     (lambda (start end path _bracketp)
       (org-ref-vis-overlay--create start end type path _bracketp))))
  (add-hook 'post-command-hook #'org-ref-vis-update-cursor-state nil t))

(defun org-ref-vis-mode--off ()
  "Deactivate `org-ref-vis-mode'."
  (org-ref-vis-overlay-purge)
  (dolist (type org-ref-vis-types)
    (org-link-set-parameters type :activate-func nil)))

(defun org-ref-vis-csl-locale-getter ()
  "Return CSL locale getter function.
In Debian, the directory is installed with the citation-style-language-locales
package."
  (if (file-directory-p org-ref-vis-csl-locales-dir)
      (citeproc-locale-getter-from-dir org-ref-vis-csl-locales-dir)
    (error "CSL locales directory not found: %s" org-ref-vis-csl-locales-dir)))

(defun org-ref-vis-get-itemgetter (bib-files)
  "Return itemgetter function for BIB-FILES."
  (if (and org-ref-vis-itemgetter
           (equal bib-files org-ref-vis-bib-files))
      org-ref-vis-itemgetter
    (setq org-ref-vis-bib-files bib-files
          org-ref-vis-itemgetter (citeproc-hash-itemgetter-from-any bib-files))))

(defun org-ref-vis--citeproc-create (citekey command)
  "TBD."
  (if-let* ((bib-files (org-ref-find-bibliography)))
      (if-let* ((it-getter (org-ref-vis-get-itemgetter bib-files))
                (retrieved (funcall it-getter (list citekey)))
                (item-data (cdr (assoc citekey retrieved))))
          (let ((lang (or (cdr (assoc "language" item-data))
                          (cdr (assoc 'language item-data)))))
            (when (null lang)
              (message "Language not set for %s; defaults to en-US" citekey)
              (setq lang "en-US"))
            (if-let* ((csl-style (and org-ref-vis-style-getter
                                      (funcall org-ref-vis-style-getter command lang))))
                (let* ((loc-getter (org-ref-vis-csl-locale-getter)))
                  (citeproc-create csl-style it-getter loc-getter lang t))
              (error "No CSL style found for %s, %s" command lang)))
        (error "No item retrieved for %s" citekey))
    (error "No bibliography files found by `org-ref-find-bibliography'")))

(defun org-ref-vis-render (citekey command &optional output-fmt)
  "TBD."
  (let* ((output-fmt (or output-fmt 'org))
         (proc (org-ref-vis--citeproc-create citekey command))

         ;; MODE is either nil (for the default citation mode) or one of the
         ;; symbols `suppress-author', `textual', `author-only',
         ;; `year-only', `title-only', `bib-entry', `locator-only'
         (mode (pcase command
                 ((or "cite" "Cite") 'bib-entry)
                 ((or "parencite" "Parencite") 'bib-entry)
                 ((or "citeauthor" "citeauthor*") 'author-only)
                 ((or "citetitle" "citetitle*") 'title-only)
                 ((or "citeurl") 'title-only)
                 ((or "citeyear" "citeyear*") 'year-only)
                 ("fullcite" 'bib-entry)
                 ("footfullcite" nil)
                 (_ nil)))

         (citation (citeproc-citation-create
                    :cites (list (list (cons 'id citekey)))
                    :mode mode))
         (_ (citeproc-append-citations (list citation) proc))
         (rendered (citeproc-render-citations proc output-fmt 'no-links)))
    (car rendered)))

(defun org-ref-vis-render-author-year (citekey command &optional output-fmt)
  "TBD."
  (if-let* ((proc (org-ref-vis--citeproc-create citekey command))
            (c-author (citeproc-citation-create
                       :cites (list (list (cons 'id citekey)))
                       :mode 'author-only))
            (c-year (citeproc-citation-create
                     :cites (list (list (cons 'id citekey)))
                     :mode 'year-only)))
      (let* ((_ (citeproc-append-citations (list c-author c-year) proc))
             (rendered (citeproc-render-citations proc (or output-fmt 'org) 'no-links))
             (s-author (nth 0 rendered))
             (s-year (nth 1 rendered)))
        (pcase command
          ("textcite" (format "%s (%s)" s-author s-year))
          (_ (format "%s %s" s-author s-year))))
    (format "ERROR(%s)" citekey)))

(defun org-ref-vis--propertize-string (str)
  "Convert plain Org text tokens in STR into proper face properties."
  (let ((result str))
    ;; Italics:
    (while (string-match "/\\([^/\n]+\\)/" result)
      (let ((matched-text (match-string 1 result))
            (start (match-beginning 0))
            (end (match-end 0)))
        (put-text-property 0 (length matched-text) 'face 'italic matched-text)
        (setq result (concat (substring result 0 start)
                             matched-text
                             (substring result end)))))
    ;; Bold:
    (while (string-match "\\*\\([^*\n]+\\)\\*" result)
      (let ((matched-text (match-string 1 result))
            (start (match-beginning 0))
            (end (match-end 0)))
        (put-text-property 0 (length matched-text) 'face 'bold matched-text)
        (setq result (concat (substring result 0 start)
                             matched-text
                             (substring result end)))))
    result))

(defun org-ref-vis-update-cursor-state ()
  "Update cursor state."
  (if (not org-ref-vis-mode)
      (progn
        (org-ref-vis-overlay-purge)
        (remove-hook 'post-command-hook #'org-ref-vis-update-cursor-state t))
    (let ((pt (window-point)))
      (when (and org-ref-vis--active-overlay
                 (overlay-buffer org-ref-vis--active-overlay))
        (unless (and (<= (overlay-start org-ref-vis--active-overlay) pt)
                     (<= pt (overlay-end org-ref-vis--active-overlay)))
          (overlay-put org-ref-vis--active-overlay 'display
                       (overlay-get org-ref-vis--active-overlay 'my-preview-string))
          (setq org-ref-vis--active-overlay nil)))
      (unless org-ref-vis--active-overlay
        (let ((ovs (overlays-at pt)))
          (catch 'found
            (dolist (ov ovs)
              (when (eq (overlay-get ov 'category) 'org-ref-vis-preview-overlay)
                (overlay-put ov 'display nil)
                (setq org-ref-vis--active-overlay ov)
                (throw 'found t)))))))))

(defun org-ref-vis-overlay--create (start end type path _bracketp)
  "Activation function for Org links."
  (remove-overlays start end 'category 'org-ref-vis-preview-overlay)
  (when-let*
      ((pt (window-point))
       (citekey (when (string-match "\\`&\\(.*\\)" path)
                  (match-string 1 path)))
       (raw-text (pcase type
                   ((or "cite" "textcite")
                    (org-ref-vis-render-author-year citekey type))
                   (_ (org-ref-vis-render citekey type)))
                 ;; (condition-case err
                 ;;     (pcase type
                 ;;       ((or "cite" "textcite")
                 ;;        (org-ref-vis-render-author-year path type))
                 ;;       (_ (org-ref-vis-render path type)))
                 ;;   (error
                 ;;    (format "[Error: %s (%S)]" path err)
                 ;;    (signal (car err) (cdr err))))
                 )
       (display-text (org-ref-vis--propertize-string raw-text)))
    (let* ((ov (make-overlay start end)))
      (overlay-put ov 'category 'org-ref-vis-preview-overlay)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'my-preview-string display-text)
      (if (and (<= start pt) (<= pt end))
          (progn
            (overlay-put ov 'display nil)
            (setq org-ref-vis--active-overlay ov))
        (overlay-put ov 'display display-text)))))

(defun org-ref-vis-overlay-purge ()
  (interactive)
  (save-restriction
    (widen)
    (let ((ovs (overlays-in (point-min) (point-max))))
      (dolist (ov ovs)
        (when (eq (overlay-get ov 'category) 'org-ref-vis-preview-overlay)
          (delete-overlay ov)))))
  (setq org-ref-vis--active-overlay nil))

(provide 'org-ref-vis)
;;; org-ref-vis.el ends here
