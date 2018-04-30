;;; yankee.el --- GFM / Org-mode source block yanker   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jake Romer

;; Author: Jake Romer <jkrmr@jakeromer.com>
;; Keywords: lisp, markdown, github-flavored markdown, org-mode
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides the following functions:
;; yankee/yank-as-gfm-code-block
;; yankee/yank-as-gfm-code-block-folded
;; yankee/yank-as-org-code-block

;;; Code:

;; Logic courtesy of projectile
(defun yankee--in-project-p ()
  "Check if we're in a project."
  (condition-case nil
      (yankee--project-root)
    (error nil)))

(defvar yankee--project-root-files
  '(".git" ".hg" ".bzr" "_darcs" ".projectile")
  "A list of files considered to mark the root of a project.")

(defun yankee--expand-file-name (file-name)
  "A thin wrapper around `expand-file-name' that handles nil.
Expand FILE-NAME using `default-directory'."
  (when file-name
    (expand-file-name file-name)))

(defcustom yankee--require-project-root t
  "Require the presence of a project root to operate when true.
Otherwise consider the current directory the project root."
  :group 'yankee
  :type 'boolean)

(defun yankee--project-root ()
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (let ((project-root
         (or (->> yankee--project-root-files
                  (--map (locate-dominating-file default-directory it))
                  (-remove #'null)
                  (car)
                  (yankee--expand-file-name))
             (if yankee--require-project-root
                 (error "You're not into a project")
               default-directory))))
    project-root))

(defun yankee--git-remote-url (remote-name)
  "Return the url for the remote named REMOTE-NAME."
  (shell-command-to-string
   (format "git remote -v |\
     grep %s |\
     awk '/fetch/{print $2}' |\
     sed -Ee 's#(git@|git://)#http://#' -e 's@com:@com/@' -e 's/\.git$//'" remote-name)))

(defun yankee--mode-string (modename)
  "Return the current buffer's major mode, MODENAME, as a string."
  (cond ((string= modename nil) "text-mode")
        ((string= modename "fundamental-mode") "text-mode")
        (t (format "%s" modename))))

(defun yankee--selected-lines (start end)
  "Return the selected line numbers bounded by START and END as a string.
Formats the returned line number or range of lines (e.g., 'L5', 'L5-L10')."
  (interactive "r")

  (let* (;; The line number at the start position, as an integer.
         (start-line (line-number-at-pos start))
         ;; The line number at the end position, as an integer.
         (end-line (line-number-at-pos (- end 1)))
         ;; The start line as a string. Example: 'L5'.
         (start-line-string (concat "L" (number-to-string start-line)))
         ;; The end line as a string. Example: 'L10'.
         (end-line-string (concat "L" (number-to-string end-line))))

    (if (= 0 (- end-line start-line))
        start-line-string
      (concat start-line-string "-" end-line-string))))

(defun yankee--path-relative-to-project-root ()
  "The current file's path relative to the project root."
  (interactive)
  (replace-regexp-in-string (yankee--project-root) ""
                            (expand-file-name (or (buffer-file-name) ""))))

(defun yankee--abbreviated-project-or-home-path-to-file ()
  "The path to the current buffer's file.
If in a project, the path is relative to the project root.
If not in a project, the path is an abbreviated absolute path."
  (interactive)
  (if (yankee--in-project-p)
      (yankee--path-relative-to-project-root)
    (abbreviate-file-name (or (buffer-file-name) ""))))

(defun yankee--yank-as-code-block (format start end)
  "In a FORMAT code fence, yank the visual selection bounded by START and END.
Includes a filename comment annotation."
  (interactive "r")

  (let* (;; The current buffer's file name.
         (file-name (yankee--abbreviated-project-or-home-path-to-file))
         ;; The current commit reference, if under version control.
         (commit-ref (yankee--current-commit-ref))
         ;; The selected line or line numbers ('L-prefixed).
         (selection-range (yankee--selected-lines start end))
         ;; The content of the selected line(s).
         (selected-lines (buffer-substring start end))
         ;; The current buffer's major mode.
         (mode-name (buffer-local-value 'major-mode (current-buffer)))
         ;; The current buffer's major mode as a string.
         (mode-string (yankee--mode-string mode-name))
         ;; The current buffer's major mode as an atom.
         (mode-atom (intern mode-string))
         ;; The language, as derived from the major mode.
         (language-mode (yankee--current-buffer-language mode-string))
         ;; A path for the selected code, including line numbers and SHA.
         (snippet-path (yankee--code-snippet-path commit-ref file-name selection-range))
         ;; A URL for the selected code, if a remote version exists.
         (snippet-url (yankee--code-snippet-url (yankee--current-commit-remote)
                                                commit-ref
                                                file-name
                                                selection-range))
         ;; Example: in tuareg-mode, 'tuareg-mode-hook' variable, as a symbol
         (mode-hook-atom (intern (format "%s-hook" mode-string)))
         ;; Store any mode hooks
         (original-mode-hooks (format "%s" (eval `,mode-hook-atom))))

    ;; disable any major mode hooks for the current mode
    (eval `(setq ,mode-hook-atom nil))

    (with-temp-buffer
      (funcall mode-atom)
      (insert file-name " " selection-range
              (if commit-ref (format " (%s)" commit-ref) ""))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))

      (cond ((equal format 'gfm)
             (yankee--gfm-code-fence language-mode selected-lines snippet-path snippet-url))
            ((equal format 'gfm-folded)
             (yankee--gfm-code-fence-folded language-mode selected-lines snippet-path snippet-url))
            ((equal format 'org)
             (yankee--org-code-fence language-mode selected-lines snippet-path snippet-url)))
      (clipboard-kill-ring-save (point-min) (point-max)))

    ;; re-enable the current major mode's hooks
    (eval `(setq ,mode-hook-atom ,original-mode-hooks))))

(defun yankee--current-buffer-language (mode-string)
  "The language used in the current buffer, inferred from the major MODE-STRING.
Intended for use in code block. Corner cases are mapped to strings GFM / Org can
understand."
  (let ((language (replace-regexp-in-string "-mode$" "" mode-string)))
    (cond ((string= language "tuareg") "ocaml")
          ((member language '("js2" "js" "js2-jsx" "js-jsx" "react")) "javascript")
          (t language))))

(defun yankee--current-commit-ref ()
  "The current commit's SHA, if under version control.
Currently only supports Git."
  (when (eq 'Git (vc-backend (buffer-file-name)))
    (substring (shell-command-to-string "git rev-parse HEAD") 0 10)))

(defun yankee--current-commit-remote ()
  "The current commit's remote URL, if under version control with a remote set.
Currently only supports Git."
  (when (eq 'Git (vc-backend (buffer-file-name)))
      (let ((remote-url (replace-regexp-in-string
                         "\n$" ""
                         (yankee--git-remote-url "origin"))))
        (and (string-match-p "http" remote-url) remote-url))))

(defun yankee--gfm-code-fence (language code path url)
  "Create a GFM code block with LANGUAGE block containing CODE, PATH, and URL."
  (goto-char (point-min))
  (insert "```" language "\n")
  (goto-char (point-max))
  (insert "\n\n" code "```\n")
  (and url (insert (format "<sup>\n  <a href=\"%s\">\n    %s\n  </a>\n</sup>\n<p></p>\n" url path))))

(defun yankee--gfm-code-fence-folded (language code path url)
  "Create a foldable GFM code block with LANGUAGE block containing CODE, PATH, and URL."
  (goto-char (point-min))
  (insert "<details>\n")
  (insert "<summary>" )
  (insert (read-string "Summary: "))
  (insert "</summary>\n\n")
  (insert "```" language "\n")
  (goto-char (point-max))
  (insert "\n\n" code "```\n")
  (and url (insert (format "<sup>\n  <a href=\"%s\">\n    %s\n  </a>\n</sup>\n<p></p>\n" url path)))
  (insert "</details>"))

(defun yankee--org-code-fence (language code path url)
  "Create an Org code block with LANGUAGE annotation containing CODE, PATH, and URL."
  (goto-char (point-min))
  (insert "#+BEGIN_SRC" " " language "\n")
  (goto-char (point-max))
  (insert "\n\n" code "#+END_SRC\n")
  (and url (insert (format "[[%s][%s]]" url path))))

(defun yankee--code-snippet-url (commit-remote commit-ref file-name selection-range)
  "Generate the snippet url from COMMIT-REMOTE, COMMIT-REF, FILE-NAME, and the SELECTION-RANGE."
  (and commit-remote
       commit-ref
       file-name
       selection-range
       (format "%s/blob/%s/%s#%s"
               commit-remote
               commit-ref
               file-name
               selection-range)))

(defun yankee--code-snippet-path (commit-ref file-name selection-range)
  "Generate the snippet path from COMMIT-REF, FILE-NAME, and the SELECTION-RANGE."
  (if commit-ref
      (format "%s#%s (%s)" file-name selection-range commit-ref)
    (format "%s#%s" file-name selection-range)))


(defun yankee/yank-as-gfm-code-block (start end)
  "In a GFM code fence, yank the selection bounded by START and END.
Includes a filename comment annotation."
  (interactive "r")
  (yankee--yank-as-code-block 'gfm start end))

(defun yankee/yank-as-gfm-code-block-folded (start end)
  "In a foldable GFM code fence, yank the selection bounded by START and END.
Includes a filename comment annotation."
  (interactive "r")
  (yankee--yank-as-code-block 'gfm-folded start end))

(defun yankee/yank-as-org-code-block (start end)
  "In an Org mode code fence, yank the selection bounded by START and END.
Includes a filename comment annotation."
  (interactive "r")
  (yankee--yank-as-code-block 'org start end))

(provide 'yankee)
;;; yankee.el ends here
