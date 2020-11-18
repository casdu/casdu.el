;; functions for CASDU copy from org-roam

(setq casdu/org-roam-capture-templates
      '(("p" "casdu" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "casdu-brain/%<%Y%m%d%H%M%S>"
         :head "#+TITLE: ${title}\n#+CREATED:       %U\n#+LAST_MODIFIED: %U\n\n"
         :unnarrowed t)))

(defcustom casdu/org-roam-capture-immediate-template
  (append (car casdu/org-roam-capture-templates) '(:immediate-finish t))
  "Capture template to use for immediate captures in Org-roam.
This is a single template, so do not enclose it into a list.
See `org-roam-capture-templates' for details on templates."
  :group 'org-roam
  ;; Adapted from `org-capture-templates'
  :type
  '(list :tag "Template entry"
         :value ("d" "default" plain (function org-roam-capture--get-point)
                 "%?"
                 :file-name "%<%Y%m%d%H%M%S>-${slug}"
                 :head "#+title: ${title}\n"
                 :unnarrowed t
                 :immediate-finish t)
         (string :tag "Keys              ")
         (string :tag "Description       ")
         (const :format "" plain)
         (const :format "" (function org-roam-capture--get-point))
         (choice :tag "Template          "
                 (string :tag "String"
                         :format "String:\n            \
Template string   :\n%v")
                 (list :tag "File"
                       (const :format "" file)
                       (file :tag "Template file     "))
                 (list :tag "Function"
                       (const :format "" function)
                       (function :tag "Template function ")))
         (const :format "File name format  :" :file-name)
         (string :format " %v" :value "#+title: ${title}\n")
         (const :format "Header format     :" :head)
         (string :format "\n%v" :value "%<%Y%m%d%H%M%S>-${slug}")
         (const :format "" :unnarrowed) (const :format "" t)
         (const :format "" :immediate-finish) (const :format "" t)
         (plist :inline t
                :tag "Options"
                ;; Give the most common options as checkboxes
                :options
                (((const :format "%v " :prepend) (const t))
                 ((const :format "%v " :jump-to-captured) (const t))
                 ((const :format "%v " :empty-lines) (const 1))
                 ((const :format "%v " :empty-lines-before) (const 1))
                 ((const :format "%v " :empty-lines-after) (const 1))
                 ((const :format "%v " :clock-in) (const t))
                 ((const :format "%v " :clock-keep) (const t))
                 ((const :format "%v " :clock-resume) (const t))
                 ((const :format "%v " :time-prompt) (const t))
                 ((const :format "%v " :tree-type) (const week))
                 ((const :format "%v " :table-line-pos) (string))
                 ((const :format "%v " :kill-buffer) (const t))))))

(defun casdu/org-roam-find-file-immediate (arg &rest args)
  "Find and open an Org-roam file.
This variant of `org-roam-find-file' uses the template in
`org-roam-capture-immediate-template', avoiding the capture
process. The interactive ARG and ARGS are passed to
`org-roam-find-file'. See `org-roam-find-file' for details."
  (interactive "P")
  (let ((args (push arg args))
        (casdu/org-roam-capture-templates (list casdu/org-roam-capture-immediate-template)))
    (apply #'casdu/org-roam-find-file args)))

(defun casdu/org-roam-find-file (&optional initial-prompt completions filter-fn no-confirm)
  "Find and open an Org-roam file.
INITIAL-PROMPT is the initial title prompt.
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.  See
`org-roam--get-title-path-completions' for details.
If NO-CONFIRM, assume that the user does not want to modify the initial prompt."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (funcall (or filter-fn #'identity)
                               (or completions (org-roam--get-title-path-completions))))
         (title-with-tags (if no-confirm
                              initial-prompt
                            (org-roam-completion--completing-read "File: " completions
                                                                  :initial-input initial-prompt)))
         (res (cdr (assoc title-with-tags completions)))
         (file-path (plist-get res :path)))
    (if file-path
        (org-roam--find-file file-path)
      (let ((org-roam-capture--info `((title . ,title-with-tags)
                                      (slug  . ,(funcall org-roam-title-to-slug-function title-with-tags))))
            (org-roam-capture--context 'title))
        (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
        (casdu/org-roam-capture--capture)))))

(defun casdu/org-roam-capture--capture (&optional goto keys)
  "Create a new file, and return the path to the edited file.
The templates are defined at `org-roam-capture-templates'.  The
GOTO and KEYS argument have the same functionality as
`org-capture'."
  (let* ((org-capture-templates (mapcar #'org-roam-capture--convert-template casdu/org-roam-capture-templates))
         (one-template-p (= (length org-capture-templates) 1))
         org-capture-templates-contexts)
    (when one-template-p
      (setq keys (caar org-capture-templates)))
    (if (or one-template-p
            (eq org-roam-capture-function 'org-capture))
        (org-capture goto keys)
      (funcall-interactively org-roam-capture-function))))

(provide 'casdu)
