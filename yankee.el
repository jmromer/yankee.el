;;; yankee.el --- GFM / Org-mode source block yanker   -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jake Romer

;; Author: Jake Romer <mail@jakeromer.com>
;; Package-Version: 0.1.0
;; Keywords: lisp, markdown, github-flavored markdown, org-mode
;; URL: https://github.com/jmromer/yankee.el
;; Package-Requires: ((copy-as-format "0.0.8"))

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
;; yankee/yank

;;; Code:

;; Logic courtesy of projectile
(defun yankee--in-project-p ()
  "Check if we're in a project."
  (condition-case nil
      (yankee--project-root)
    (error nil)))

(defun yankee--expand-file-name (file-name)
  "A thin wrapper around `expand-file-name' that handles nil.
Expand FILE-NAME using `default-directory'."
  (when file-name
    (expand-file-name file-name)))

(defvar yankee--project-root-files
  '(".git" ".hg" ".bzr" "_darcs" ".projectile")
  "A list of files considered to mark the root of a project.")

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
                 (error "You're not in a project")
               default-directory))))
    project-root))

;; copy-as-format folded gfm format

(defun copy-as-format--github-folded (text multiline)
  "Create a foldable GFM code block with TEXT as body.
MULTILINE is a boolean indicating if the selected text spans multiple lines."
  (let ((summary (read-string "Summary: ")))
    (if multiline
        (format "<details>\n<summary>%s</summary>\n\n```%s\n%s\n```\n</details>\n"
                summary (copy-as-format--language) text)
      (copy-as-format--inline-markdown text))))

(with-eval-after-load 'copy-as-format
  (if (boundp 'copy-as-format-format-alist)
      (add-to-list 'copy-as-format-format-alist
                   '("github-folded" copy-as-format--github-folded))
    (error "`copy-as-format-format-alist' not defined")))

(defun copy-as-format--extract-text ()
  "Override function in `copy-as-format' to add filename comment."
  (if (not (use-region-p))
      (buffer-substring-no-properties (line-beginning-position) (line-end-position))
      ;; Avoid adding an extra blank line to the selection. This happens when
      ;; point or mark is at the start of the next line.
      ;;
      ;; When selection is from bottom to top, exchange point and mark
      ;; so that the `point' and `(region-end)' are the same.
      (when (< (point) (mark))
        (exchange-point-and-mark))

      (let (n min text (end (region-end)))
        (when (= end (line-beginning-position))
          (setq end (1- end)))

        ;; Let's trim unnecessary leading space from the region
        (setq text (buffer-substring-no-properties (region-beginning) end))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          ;; The length of the match (see below) determines how much leading
          ;; space to trim
          ;;
          ;; Without this only one space would be trimmed for each tab
          (untabify (point-min) (point-max))
          (while (search-forward-regexp "^\\([[:space:]]*\\)[^[:space:]]" nil t)
            (setq n (length (match-string 1)))
            (when (or (null min) (< n min))
              (setq min n)))

          (when (and (not (null min)) (> min 0))
            (indent-rigidly 1 (point-max) (- min)))
          (buffer-string)))))

(defun yankee/yank ()
  "Yank as code block, prompt for output format."
  (interactive)
  (setq current-prefix-arg '(4))
  (copy-as-format))

(defun yankee--selected-lines (format start-line end-line)
  "Return the selected line numbers as a string.
In the given FORMAT, generate the selection range string for the selection with
the given START-LINE and END-LINE (e.g., 'L5-L10', 'L5-10', or 'lines-5:10')."
  (cond ((equal format 'path)
         (if (equal start-line end-line)
             (format "L%s" (number-to-string start-line))
           (format "L%s-%s" (number-to-string start-line) (number-to-string end-line))))
        ((equal format 'github)
         (format "L%s-L%s" (number-to-string start-line) (number-to-string end-line)))
        ((equal format 'gitlab)
         (format "L%s-%s" (number-to-string start-line) (number-to-string end-line)))
        ((equal format 'bitbucket)
         (format "lines-%s:%s" (number-to-string start-line) (number-to-string end-line)))))

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

(defun yankee--clean-leading-whitepace (text)
  "Strip leading whitespace from each line of TEXT.
The amount stripped from each line is equal to the amount stripped from the
line with the left-most text."
  (defun chomp-trailing-newlines (string)
    (replace-regexp-in-string "\n\+\\'" "" string))

  (defun leading-whitespace-length (string)
    (let ((first-non-whitespace-char-posn
           (string-match-p "[^[:space:]]" string)))
      (or first-non-whitespace-char-posn 0)))

  (defun least-leading-whitespace-length (lines)
    (car (sort (mapcar #'leading-whitespace-length lines) '<)))

  (defun trim-leading-chars-and-join (num-chars-to-trim lines)
    (defun trim-line-unless-blank (line)
      (if (not (string= "" line))
          (substring line num-chars-to-trim)
        line))
    (mapconcat #'trim-line-unless-blank lines "\n"))

  (let* ((all-lines        ;; a list of all selected lines
          (split-string (chomp-trailing-newlines text) "\n"))
         (non-blank-lines  ;; only those with non-whitespace chars
          (seq-filter (lambda (line) (not (string= line ""))) all-lines))
         (start-index      ;; index of leftmost non-whitespace char
          (least-leading-whitespace-length non-blank-lines))
         (trimmed-text     ;; re-joined text, with leading whitespace text
          (trim-leading-chars-and-join start-index all-lines)))
    (concat trimmed-text "\n")))

(defun yankee--current-buffer-language (mode-string)
  "The language used in the current buffer, inferred from the major MODE-STRING.
Intended for use in code block. Corner cases are mapped to strings GFM / Org can
understand."
  (let ((language (replace-regexp-in-string "-mode$" "" mode-string)))
    (cond ((string= language "tuareg") "ocaml")
          ((member language '("js2" "js" "js2-jsx" "js-jsx" "react")) "javascript")
          (t language))))

(defun yankee--remote-url-git (remote-name)
  "Using Git, return the url for the remote named REMOTE-NAME."
  (shell-command-to-string
   (format "git remote -v |\
     grep %s |\
     awk '/fetch/{print $2}' |\
     sed -Ee 's#(git@|git://)#http://#' -e 's@com:@com/@' -e 's/\.git$//'" remote-name)))

(defun yankee--list-dirty-files-git ()
  "Using Git, list the repository's currently dirty files.
Includes files with modifications and new files not yet in the index."
  (replace-regexp-in-string
   "\n\\'" ""
   (shell-command-to-string
    "git status --porcelain --ignore-submodules | awk '{ print $2 }'")))

(defun yankee--current-commit-ref-git ()
  "Using Git, return the ref for the buffer file's current commit.
If dirty or untracked, return 'uncommitted'."
  (let ((filename (yankee--abbreviated-project-or-home-path-to-file))
        (uncommitted-files (yankee--list-dirty-files-git)))
    (if (string-match-p (format "^%s" filename) uncommitted-files)
        "uncommitted"
      (substring (shell-command-to-string "git rev-parse HEAD") 0 10))))

(defun yankee--current-commit-ref ()
  "The current commit's SHA, if under version control.
Currently only supports Git."
  (cond
   ((bound-and-true-p git-timemachine-mode)
    (substring (git-timemachine-kill-revision) 0 10))
   ((eq 'Git (vc-backend (buffer-file-name)))
    (yankee--current-commit-ref-git))))

(defun yankee--current-commit-remote ()
  "The current commit's remote URL, if under version control with a remote set.
Currently only supports Git."
  (when (eq 'Git (vc-backend (buffer-file-name)))
      (let ((remote-url (replace-regexp-in-string
                         "\n$" ""
                         (yankee--remote-url-git "origin"))))
        (and (string-match-p "http" remote-url) remote-url))))

(defun yankee--code-snippet-url (commit-remote commit-ref file-name start-line end-line)
  "Generate the snippet url in the appropriate format depending on the service.
Supports GitHub, GitLab, and Bitbucket.

Examples:
COMMIT-REMOTE: https://github.com/orgname/reponame/file.py
COMMIT-REF: 105561ec24
FILE-NAME: file.py
START-LINE: 4
END-LINE: 8"
  (and commit-remote commit-ref file-name start-line end-line
      (cond
       ;; GitHub URL format
       ((string-match-p "github.com" commit-remote)
        (format "%s/blob/%s/%s#%s"
                 commit-remote
                 commit-ref
                 file-name
                 (yankee--selected-lines 'github start-line end-line)))
       ;; GitLab URL format
       ((string-match-p "gitlab.com" commit-remote)
        (format "%s/blob/%s/%s#%s"
                 commit-remote
                 commit-ref
                 file-name
                 (yankee--selected-lines 'gitlab start-line end-line)))
       ;; BitBucket URL format
       ((string-match-p "bitbucket.org" commit-remote)
        (format "%s/src/%s/%s#%s"
                 commit-remote
                 commit-ref
                 file-name
                 (yankee--selected-lines 'bitbucket start-line end-line))))))

(defun yankee--hyperlink-to-patch (href-url text-path)
  "Generate the hyperlink to the yanked patch in the appropriate format.
Supports GitHub, GitLab, and Bitbucket. HREF-URL becomes the href attribute,
TEXT-PATH the anchor tag text."
  (cond
   ;; GitHub / GitLab: Use HTML, display smaller
   ((or (string-match-p "github.com" href-url)
        (string-match-p "gitlab.com" href-url))
    (format "<sup>\n  <a href=\"%s\">\n    %s\n  </a>\n</sup>\n<p></p>\n"
            href-url
            text-path))
   ;; BitBucket: Use Markdown
   ((string-match-p "bitbucket.org" href-url)
    (format "\n[%s](%s)"
            text-path
            href-url))))

(defun yankee--code-snippet-path (commit-ref file-name selection-range)
  "Generate the snippet path. Displayed as the patch's hyperlink text.
Examples:
COMMIT-REF: 105561ec24
FILE-NAME: appointments.py
SELECTION-RANGE: L4-L8."
  (if commit-ref
      (format "%s %s (%s)" file-name selection-range commit-ref)
    (format "%s %s" file-name selection-range)))

(provide 'yankee)
;;; yankee.el ends here
