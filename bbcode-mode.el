;;; bbcode-mode.el --- Major mode to edit bbcode files in Emacs
;;
;; Author: Jason F. McBrayer
;; Created: April, 2008
;; Last-Updated: Sun February 6 12:31:20 2012
;;           By: Bogdan Trach
;;
;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; This should be trivial and self explanatory.  It is for editing
;; bbcode, typically when emacs(client) is called as an external
;; editor from a web browser.
;;
(defgroup bbcode nil
  "Support for the BBcode."
  :group 'languages)

(defcustom bbcode-main-format "b"
  "Default tag for entry highlighting"
  :group 'bbcode
  :type 'string)

(defvar bbcode-tag-table
  (list
   '("url" :opt-accept "C-c u" 2 "Link")
   '("i" :no-opt "C-c i" 1 "Italic")
   '("b" :no-opt "C-c b" 1 "Bold")
   '("qoute" :opt-accept "C-c q" 2 "Quote")
   '("image" :opt-accept "C-c m" 2 "Image")
   '("color" :opt-accept "C-c c" 2 "Color")
   '("spoiler" :opt-accept "C-c s" 2 "Spoiler")
   '("r" :no-opt "C-c a r" 3 "Align right")
   '("center" :no-opt "C-c a c" 3 "Align center")
   '("list" :no-opt "C-c l" 5 "List")
   '("table" :no-opt "C-c t t" 4 "Table")
   '("tr" :no-opt "C-c t r" 4 "Table row")
   '("td" :no-opt "C-c t d" 4 "Table cell")))

(defvar bbcode-no-bind-tags
  '("size" "html" "attach" "*"))
;;; ----------------------------------------
;;; Font locking

(defconst bbcode-font-lock-keywords-1
  (list
   '("\\[B\\]\\[I\\].*?\\[/I\\]\\[/B\\]" . 'bold-italic)
   '("\\[I\\]\\[B\\].*?\\[/B\\]\\[/I\\]" . 'bold-italic)
   '("\\[I\\].*?\\[/I\\]" . 'italic)
   '("\\[B\\].*?\\[/B\\]" . 'bold)
   '("\\[/?\\(I\\|TABLE\\|TD\\|TR\\|R\\|SIZE\\|CENTER\\|HTML\\|B\\|URL\\|IMG\\|LIST\\|\*\\|QUOTE\\|SPOILER\\|COLOR\\|ATTACH\\)[^]]*\\]" .
     'font-lock-keyword-face)
   )
  "Minimal highlighting expressions for bbcode mode")

(defvar bbcode-font-lock-keywords bbcode-font-lock-keywords-1
  "Default highlighting expressions for bbcode.")

;;; ----------------------------------------
;;; Element insertion

;; From Jason Blevins markdown-model.el
(defun bbcode/wrap-or-insert (s1 s2)
 "Insert the strings s1 and s2 around the current region or just insert them
if there is no region selected."
 (if (and transient-mark-mode mark-active)
     (let ((a (region-beginning)) (b (region-end)))
       (kill-region a b)
       (insert s1)
       (yank)
       (insert s2))
   (insert s1 s2)))

(defun bbcode-make-open-tag (tagname)
  (concat "[" tagname "]"))

(defun bbcode-make-close-tag (tagname)
(let ((processed-tagname (car (split-string tagname "="))))
  (concat "[/" processed-tagname "]")))

(defun bbcode-line-empty-p ()
  (interactive)
  (equalp (line-end-position) (line-beginning-position)))

(defun bbcode/insert-at-beginning-moving-forward (s)
  (beginning-of-line)
  (insert s)
  (forward-line))

(defun bbcode/insert-line-beginning (s)
  "Insert the string s on the beginning of each line in the current region or
just insert it if there is no region selected."
  (save-excursion
    (if (and transient-mark-mode mark-active)
	(let ((a (region-beginning))
	      (b (line-number-at-pos (region-end))))
	  (goto-char a)
	  (while (< (line-number-at-pos (point)) b)
	    (if (bbcode-line-empty-p)
		(forward-line)
	      (bbcode/insert-at-beginning-moving-forward s))))
      (bbcode/insert-at-beginning-moving-forward s))))

 (defmacro bbcode-define-tag (tagname)
	 `(defun ,(intern (concat "bbcode-insert-" (eval tagname))) ()
	    (let ((var ,(eval tagname)))
	      (interactive)
	      (bbcode/wrap-or-insert (bbcode-make-open-tag var) 
				     (bbcode-make-close-tag var))
	      (backward-char (+ 3 (length var)))
	      )))

(defun bbcode-generic-input (tag)
  (interactive (list (read-from-minibuffer "Enter tag: ")) )
  (let* ((s1 (bbcode-make-open-tag tag))
	 (s2 (bbcode-make-close-tag tag))
	 (l (length s2)))
    (bbcode/wrap-or-insert s1 s2)
    (backward-char l)))

(defun bbcode-insert-italic ()
  "Insert italic tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[i]" "[/i]")
  (backward-char 4))
(defun bbcode-insert-bold ()
  "Insert bold tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[b]" "[/b]")
  (backward-char 4))
(defun bbcode-insert-link ()
  "Insert link tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[url=]" "[/url]")
  (backward-char 6))
(defun bbcode-insert-image ()
  "Insert image tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[img]" "[/img]")
  (backward-char 6))
(defun bbcode-insert-quote ()
  "Insert quote tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[quote=\"\"]" "[/quote]")
  (backward-char 8))
(defun bbcode-insert-spoiler ()
  "Insert spoiler tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[spoiler=\"\"]" "[/spoiler]")
  (backward-char 10))
(defun bbcode-insert-color ()
  "Insert color tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[color=\"\"]" "[/color]")
  (backward-char 8))
(defun bbcode-insert-list ()
  "Insert list tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[list]" "[/list]")
  (backward-char 7))
(defun bbcode-insert-right ()
  "Insert list tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[R]" "[/R]")
  (backward-char 4))
(defun bbcode-insert-center ()
  "Insert list tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[center]" "[/center]")
  (backward-char 9))
(defun bbcode-table-new ()
  "Insert list tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[table]" "[/table]")
  (backward-char 8))
(defun bbcode-table-row ()
  "Insert list tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[tr]" "[/tr]")
  (backward-char 5))
(defun bbcode-table-cell ()
  "Insert list tags, around the region if it exists."
  (interactive)
  (bbcode/wrap-or-insert "[td]" "[/td]")
  (backward-char 5))
(defun bbcode-mark-list ()
  "Insert list element tags, on each line in the region if it exists."
  (interactive)
  (bbcode/insert-line-beginning "[*]"))

(defun bbcode-delete-tag ()
  (interactive)
  (let ((delete-start (search-backward "["))
	(delete-end (search-forward "]")))
    (delete-region delete-start delete-end)))

(defun bbcode-format-desc-field ()
  (interactive)
  (beginning-of-line)
  (insert (bbcode-make-open-tag bbcode-main-format))
  (search-forward ": ")
  (insert (bbcode-make-close-tag bbcode-main-format)))

;;; ----------------------------------------
;;; Mode definition
(define-derived-mode bbcode-mode text-mode "bbcode"
  "Major mode for editing bbcode
\\{bbcode-mode-map}"
  (kill-all-local-variables)
  (use-local-map bbcode-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(bbcode-font-lock-keywords nil t))
  (set (make-local-variable 'font-lock-multiline) t)
  (setq mode-name "bbcode")
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (font-lock-change-mode))

;;; ----------------------------------------
;;; keymaps
(define-key bbcode-mode-map (kbd "C-c i") 'bbcode-insert-italic)
(define-key bbcode-mode-map (kbd "C-c b") 'bbcode-insert-bold)
(define-key bbcode-mode-map (kbd "C-c u") 'bbcode-insert-link)
(define-key bbcode-mode-map (kbd "C-c q") 'bbcode-insert-quote)
(define-key bbcode-mode-map (kbd "C-c m") 'bbcode-insert-image)
(define-key bbcode-mode-map (kbd "C-c s") 'bbcode-insert-spoiler)
(define-key bbcode-mode-map (kbd "C-c c") 'bbcode-insert-color)
(define-key bbcode-mode-map (kbd "C-c l") 'bbcode-insert-list)
(define-key bbcode-mode-map (kbd "C-c p") 'bbcode-mark-list)
(define-key bbcode-mode-map (kbd "C-c d") 'bbcode-delete-tag)
(define-key bbcode-mode-map (kbd "C-c C-c") 'bbcode-generic-input)
(define-key bbcode-mode-map (kbd "C-c a r") 'bbcode-insert-right)
(define-key bbcode-mode-map (kbd "C-c a c") 'bbcode-insert-center)
(define-key bbcode-mode-map (kbd "C-c t t") 'bbcode-table-new)
(define-key bbcode-mode-map (kbd "C-c t r") 'bbcode-table-row)
(define-key bbcode-mode-map (kbd "C-c t c") 'bbcode-table-cell)
(define-key bbcode-mode-map (kbd "C-c C-e f") 'bbcode-format-desc-field)

(easy-menu-define my-menu bbcode-mode-map "DOC:bbcode menu map"
		      '("BBCode"
			["Bold" bbcode-insert-bold]
			["Italic" bbcode-insert-italic]
			"---"
			["Generic tag" bbcode-generic-input]
			["Link" bbcode-insert-link]
			["Quote" bbcode-insert-quote]
			["Image" bbcode-insert-image]
			["Spoiler" bbcode-insert-spoiler]
			["Color" bbcode-insert-color]
			"---"
			["Align center" bbcode-insert-center]
			["Align right" bbcode-insert-right]
			"---"
			["Table" bbcode-table-new]
			["Table row" bbcode-table-row]
			["Table cell" bbcode-table-cell]
			"---"
			["List" bbcode-insert-list]
			["List Items" bbcode-mark-list]
			))

(add-to-list 'auto-mode-alist '("\\.bbc\\(ode\\)?\\'" . bbcode-mode))
(provide 'bbcode-mode)
