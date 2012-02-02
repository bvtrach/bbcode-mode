;;; bbcode-mode.el --- Major mode to edit bbcode files in Emacs
;;
;; Author: Jason F. McBrayer
;; Created: April, 2008
;; Last-Updated: Sun June 5 12:31:20 2011
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

(defun bbcode/insert-line-beginning (s)
 "Insert the string s on the beginning of each line in the current region or 
just insert it if there is no region selected."
 (save-excursion 
   (if (and transient-mark-mode mark-active)
       (let ((a (region-beginning)) 
	     (b (region-end))
	     (start (point)))
	 (goto-char a)
	 (dotimes (num (- (line-number-at-pos b) 
			  (line-number-at-pos a) -1) value)
	   (insert s)
	   (forward-line)
	   (beginning-of-line))
	 (forward-line -1))
     (let ((start (max (point) 3)))
       (beginning-of-line)
       (insert s)
       (goto-char (+ (length s) start))))))


(defun bbcode-generic-input (tag)
  (interactive (list (read-from-minibuffer "Enter tag: ")) )
  (let* ((s1 (concat "[" tag "]"))
	 (s2 (concat "[/" tag "]"))
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


;;; ----------------------------------------
;;; Mode definition
(define-derived-mode bbcode-mode text-mode "bbcode"
  "Major mode for editing bbcode
\\{bbcode-mode-map}"
  (if bbcode-mode-map
      (print "GOOD!"))
  (kill-all-local-variables)
  (if bbcode-mode-map
      (print "PERFECT!"))
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
(define-key bbcode-mode-map (kbd "C-c C-c") 'bbcode-generic-input)
(define-key bbcode-mode-map (kbd "C-c a r") 'bbcode-insert-right)
(define-key bbcode-mode-map (kbd "C-c a c") 'bbcode-insert-center)
(define-key bbcode-mode-map (kbd "C-c t t") 'bbcode-table-new)
(define-key bbcode-mode-map (kbd "C-c t r") 'bbcode-table-row)
(define-key bbcode-mode-map (kbd "C-c t c") 'bbcode-table-cell)

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
