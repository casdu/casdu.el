;;;; functions for CASDU almost copy from org-roam v2
;;   It is convenient to capture ideas within CASDU instead of using
;;   multiple kbd, for capturing. This function is convenient for
;;   write notes sometime.
;;
;;   Usage: M-x casdu/org-roam-node-find
;;


(defun casdu/org-roam-node-find (&optional other-window initial-input filter-fn)
  "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window."
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (casdu/org-roam-capture-
       :node node
       :props '(:finalize find-file)))))

(cl-defun casdu/org-roam-capture- (&key goto keys node info props templates)
  "Main entry point.
GOTO and KEYS correspond to `org-capture' arguments.
INFO is an alist for filling up Org-roam's capture templates.
NODE is an `org-roam-node' construct containing information about the node.
PROPS is a plist containing additional Org-roam properties for each template.
TEMPLATES is a list of org-roam templates."
  (let* ((props (plist-put props :call-location (point-marker)))
         (org-capture-templates
          (mapcar (lambda (template)
                    (org-roam-capture--convert-template template props))
                  (or templates casdu/org-capture-templates)))
         (org-roam-capture--node node)
         (org-roam-capture--info info))
    (when (and (not keys)
               (= (length casdu/org-capture-templates) 1))
      (setq keys (caar casdu/org-capture-templates)))
    (org-capture goto keys)))

(defcustom casdu/org-capture-templates nil
  "Templates for the creation of new entries.

Each entry is a list with the following items:

keys         The keys that will select the template, as a string, characters
             only, for example \"a\" for a template to be selected with a
             single key, or \"bt\" for selection with two keys.  When using
             several keys, keys using the same prefix key must be together
             in the list and preceded by a 2-element entry explaining the
             prefix key, for example

                     (\"b\" \"Templates for marking stuff to buy\")

             The \"C\" key is used by default for quick access to the
             customization of the template variable.  But if you want to use
             that key for a template, you can.

description  A short string describing the template, will be shown during
             selection.

type         The type of entry.  Valid types are:
               entry       an Org node, with a headline.  Will be filed
                           as the child of the target entry or as a
                           top-level entry.  Its default template is:
                             \"* %?\n %a\"
               item        a plain list item, will be placed in the
                           first plain list at the target location.
                           Its default template is:
                             \"- %?\"
               checkitem   a checkbox item.  This differs from the
                           plain list item only in so far as it uses a
                           different default template.  Its default
                           template is:
                             \"- [ ] %?\"
               table-line  a new line in the first table at target location.
                           Its default template is:
                             \"| %? |\"
               plain       text to be inserted as it is.

target       Specification of where the captured item should be placed.
             In Org files, targets usually define a node.  Entries will
             become children of this node, other types will be added to the
             table or list in the body of this node.

             Most target specifications contain a file name.  If that file
             name is the empty string, it defaults to `org-default-notes-file'.
             A file can also be given as a variable or as a function called
             with no argument.  When an absolute path is not specified for a
             target, it is taken as relative to `org-directory'.

             Valid values are:

             (file \"path/to/file\")
                 Text will be placed at the beginning or end of that file

             (id \"id of existing Org entry\")
                 File as child of this entry, or in the body of the entry

             (file+headline \"path/to/file\" \"node headline\")
                 Fast configuration if the target heading is unique in the file

             (file+olp \"path/to/file\" \"Level 1 heading\" \"Level 2\" ...)
                 For non-unique headings, the full outline path is safer

             (file+regexp  \"path/to/file\" \"regexp to find location\")
                 File to the entry matching regexp

             (file+olp+datetree \"path/to/file\" \"Level 1 heading\" ...)
                 Will create a heading in a date tree for today's date.
                 If no heading is given, the tree will be on top level.
                 To prompt for date instead of using TODAY, use the
                 :time-prompt property.  To create a week-tree, use the
                 :tree-type property.

             (file+function \"path/to/file\" function-finding-location)
                 A function to find the right location in the file

             (clock)
                File to the entry that is currently being clocked

             (function function-finding-location)
                Most general way: write your own function which both visits
                the file and moves point to the right location

template     The template for creating the capture item.
             If it is an empty string or nil, a default template based on
             the entry type will be used (see the \"type\" section above).
             Instead of a string, this may also be one of:

                 (file \"/path/to/template-file\")
                 (function function-returning-the-template)

             in order to get a template from a file, or dynamically
             from a function.

The rest of the entry is a property list of additional options.  Recognized
properties are:

 :prepend            Normally newly captured information will be appended at
                     the target location (last child, last table line,
                     last list item...).  Setting this property will
                     change that.

 :immediate-finish   When set, do not offer to edit the information, just
                     file it away immediately.  This makes sense if the
                     template only needs information that can be added
                     automatically.

 :jump-to-captured   When set, jump to the captured entry when finished.

 :empty-lines        Set this to the number of lines that should be inserted
                     before and after the new item.  Default 0, only common
                     other value is 1.

 :empty-lines-before Set this to the number of lines that should be inserted
                     before the new item.  Overrides :empty-lines for the
                     number lines inserted before.

 :empty-lines-after  Set this to the number of lines that should be inserted
                     after the new item.  Overrides :empty-lines for the
                     number of lines inserted after.

 :clock-in           Start the clock in this item.

 :clock-keep         Keep the clock running when filing the captured entry.

 :clock-resume       Start the interrupted clock when finishing the capture.
                     Note that :clock-keep has precedence over :clock-resume.
                     When setting both to t, the current clock will run and
                     the previous one will not be resumed.

 :time-prompt        Prompt for a date/time to be used for date/week trees
                     and when filling the template.

 :tree-type          When `week', make a week tree instead of the month tree.

 :unnarrowed         Do not narrow the target buffer, simply show the
                     full buffer.  Default is to narrow it so that you
                     only see the new stuff.

 :table-line-pos     Specification of the location in the table where the
                     new line should be inserted.  It should be a string like
                     \"II-3\", meaning that the new line should become the
                     third line before the second horizontal separator line.

 :kill-buffer        If the target file was not yet visited by a buffer when
                     capture was invoked, kill the buffer again after capture
                     is finalized.

 :no-save            Do not save the target file after finishing the capture.

The template defines the text to be inserted.  Often this is an
Org mode entry (so the first line should start with a star) that
will be filed as a child of the target headline.  It can also be
freely formatted text.  Furthermore, the following %-escapes will
be replaced with content and expanded:

  %[pathname] Insert the contents of the file given by
              `pathname'.  These placeholders are expanded at the very
              beginning of the process so they can be used to extend the
              current template.
  %(sexp)     Evaluate elisp `(sexp)' and replace it with the results.
              Only placeholders pre-existing within the template, or
              introduced with %[pathname] are expanded this way.  Since this
              happens after expanding non-interactive %-escapes, those can
              be used to fill the expression.
  %<...>      The result of format-time-string on the ... format specification.
  %t          Time stamp, date only.  The time stamp is the current time,
              except when called from agendas with `\\[org-agenda-capture]' or
              with `org-capture-use-agenda-date' set.
  %T          Time stamp as above, with date and time.
  %u, %U      Like the above, but inactive time stamps.
  %i          Initial content, copied from the active region.  If
              there is text before %i on the same line, such as
              indentation, and %i is not inside a %(sexp), that prefix
              will be added before every line in the inserted text.
  %a          Annotation, normally the link created with `org-store-link'.
  %A          Like %a, but prompt for the description part.
  %l          Like %a, but only insert the literal link.
  %c          Current kill ring head.
  %x          Content of the X clipboard.
  %k          Title of currently clocked task.
  %K          Link to currently clocked task.
  %n          User name (taken from the variable `user-full-name').
  %f          File visited by current buffer when org-capture was called.
  %F          Full path of the file or directory visited by current buffer.
  %:keyword   Specific information for certain link types, see below.
  %^g         Prompt for tags, with completion on tags in target file.
  %^G         Prompt for tags, with completion on all tags in all agenda files.
  %^t         Like %t, but prompt for date.  Similarly %^T, %^u, %^U.
              You may define a prompt like: %^{Please specify birthday}t.
              The default date is that of %t, see above.
  %^C         Interactive selection of which kill or clip to use.
  %^L         Like %^C, but insert as link.
  %^{prop}p   Prompt the user for a value for property `prop'.
  %^{prompt}  Prompt the user for a string and replace this sequence with it.
              A default value and a completion table can be specified like this:
              %^{prompt|default|completion2|completion3|...}.
  %?          After completing the template, position cursor here.
  %\\1 ... %\\N Insert the text entered at the nth %^{prompt}, where N
              is a number, starting from 1.

Apart from these general escapes, you can access information specific to
the link type that is created.  For example, calling `org-capture' in emails
or in Gnus will record the author and the subject of the message, which you
can access with \"%:from\" and \"%:subject\", respectively.  Here is a
complete list of what is recorded for each link type.

Link type               |  Available information
------------------------+------------------------------------------------------
bbdb                    |  %:type %:name %:company
vm, wl, mh, mew, rmail, |  %:type %:subject %:message-id
gnus                    |  %:from %:fromname %:fromaddress
                        |  %:to   %:toname   %:toaddress
                        |  %:fromto (either \"to NAME\" or \"from NAME\")
                        |  %:date %:date-timestamp (as active timestamp)
                        |  %:date-timestamp-inactive (as inactive timestamp)
gnus                    |  %:group, for messages also all email fields
eww, w3, w3m            |  %:type %:url
info                    |  %:type %:file %:node
calendar                |  %:type %:date

When you need to insert a literal percent sign in the template,
you can escape ambiguous cases with a backward slash, e.g., \\%i."
  :group 'org-capture
  :version "24.1"
  :set (lambda (s v) (set s (org-capture-upgrade-templates v)))
  :type
  (let ((file-variants '(choice :tag "Filename       "
				(file :tag "Literal")
				(function :tag "Function")
				(variable :tag "Variable")
				(sexp :tag "Form"))))
  `(repeat
    (choice :value ("" "" entry (file "~/org/notes.org") "")
	    (list :tag "Multikey description"
		  (string :tag "Keys       ")
		  (string :tag "Description"))
	    (list :tag "Template entry"
		  (string :tag "Keys           ")
		  (string :tag "Description    ")
		  (choice :tag "Capture Type   " :value entry
			  (const :tag "Org entry" entry)
			  (const :tag "Plain list item" item)
			  (const :tag "Checkbox item" checkitem)
			  (const :tag "Plain text" plain)
			  (const :tag "Table line" table-line))
		  (choice :tag "Target location"
			  (list :tag "File"
				(const :format "" file)
				,file-variants)
			  (list :tag "ID"
				(const :format "" id)
				(string :tag "  ID"))
			  (list :tag "File & Headline"
				(const :format "" file+headline)
				,file-variants
				(string :tag "  Headline"))
			  (list :tag "File & Outline path"
				(const :format "" file+olp)
				,file-variants
				(repeat :tag "Outline path" :inline t
					(string :tag "Headline")))
			  (list :tag "File & Regexp"
				(const :format "" file+regexp)
				,file-variants
				(regexp :tag "  Regexp"))
			  (list :tag "File [ & Outline path ] & Date tree"
				(const :format "" file+olp+datetree)
				,file-variants
				(option (repeat :tag "Outline path" :inline t
						(string :tag "Headline"))))
			  (list :tag "File & function"
				(const :format "" file+function)
				,file-variants
				(sexp :tag "  Function"))
			  (list :tag "Current clocking task"
				(const :format "" clock))
			  (list :tag "Function"
				(const :format "" function)
				(sexp :tag "  Function")))
		  (choice :tag "Template       "
			  (string)
			  (list :tag "File"
				(const :format "" file)
				(file :tag "Template file"))
			  (list :tag "Function"
				(const :format "" function)
				(function :tag "Template function")))
		  (plist :inline t
			 ;; Give the most common options as checkboxes
			 :options (((const :format "%v " :prepend) (const t))
				   ((const :format "%v " :immediate-finish) (const t))
				   ((const :format "%v " :jump-to-captured) (const t))
				   ((const :format "%v " :empty-lines) (const 1))
				   ((const :format "%v " :empty-lines-before) (const 1))
				   ((const :format "%v " :empty-lines-after) (const 1))
				   ((const :format "%v " :clock-in) (const t))
				   ((const :format "%v " :clock-keep) (const t))
				   ((const :format "%v " :clock-resume) (const t))
				   ((const :format "%v " :time-prompt) (const t))
				   ((const :format "%v " :tree-type) (const week))
				   ((const :format "%v " :unnarrowed) (const t))
				   ((const :format "%v " :table-line-pos) (string))
				   ((const :format "%v " :kill-buffer) (const t)))))))))

(setq casdu/org-capture-templates
      '(("d" "default" plain "%?" :if-new
         (file+head "casdu-brain/pages/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+created:       %U\n#+last_modified: %U\n#+startup: showall\n#+filetags: casdu\n")
         :unnarrowed t)))

(provide 'casdu)
