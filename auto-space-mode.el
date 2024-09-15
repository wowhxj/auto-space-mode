;;; auto-space-mode.el --- Auto adding space between Chinese and English -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun add-space-between-chinese-and-english ()
  "Automatically add a space between Chinese and English characters."
  (let ((current-char (char-before))
        (prev-char (char-before (1- (point))))
        (next-char (char-after)))
    (when (and current-char prev-char
               (or (and (is-chinese-character prev-char) (is-halfwidth-character current-char))
                   (and (is-halfwidth-character prev-char) (is-chinese-character current-char)))
               (not (eq prev-char ?\s))) ; Check if the previous character is a space
      (save-excursion
        (goto-char (1- (point)))
        (insert " ")))
    (when (and current-char next-char
               (or (and (is-chinese-character current-char) (is-halfwidth-character next-char))
                   (and (is-halfwidth-character current-char) (is-chinese-character next-char)))
               (not (eq current-char ?\s))) ; Check if the current character is a space
      (save-excursion
        (goto-char (point))
        (insert " ")))))

(defun is-chinese-character (char)
  "判断字符是否为中文字符。"
  (and char (or (and (>= char #x4e00) (<= char #x9fff))
                (and (>= char #x3400) (<= char #x4dbf))
                (and (>= char #x20000) (<= char #x2a6df))
                (and (>= char #x2a700) (<= char #x2b73f))
                (and (>= char #x2b740) (<= char #x2b81f))
                (and (>= char #x2b820) (<= char #x2ceaf)))))

(defun is-halfwidth-character (char)
  "判断字符是否为半角字符，包括英文字母、数字和标点符号。"
  (and char (or (and (>= char ?a) (<= char ?z))
                (and (>= char ?A) (<= char ?Z))
                (and (>= char ?0) (<= char ?9))
                ;; (member char '(?! ?( ?) ?- ?{ ?} ?| ?: ?\; ?\" ?< ?> ?? ? ?\; ?' ?, ?. ?\\ ?` ?[ ?])) ;; 除了标记符号外的半角
                ;; (member char '(?~ ?! ?@ ?# ?$ ?% ?^ ?& ?* ?( ?) ?_ ?+ ?- ?= ?{ ?} ?| ?: ?\" ?< ?> ?? ?/ ?\; ?' ?, ?. ?\\ ?` ?[ ?] ?\ )) ;; 绝大多数半角
                )))

(defun delayed-add-space-between-chinese-and-english ()
  "延迟执行，在中英文之间自动添加空格。"
  (run-with-idle-timer 0 nil 'add-space-between-chinese-and-english))

(defun process-pasted-text (text prev-char next-char)
  "Process pasted TEXT to add spaces between Chinese and English characters, considering PREV-CHAR and NEXT-CHAR."
  (with-temp-buffer
    (insert (if prev-char (concat (char-to-string prev-char) text) text))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((current-char (char-after))
            (next-char-internal (char-after (1+ (point)))))
        (when (and current-char next-char-internal
                   (or (and (is-chinese-character current-char) (is-halfwidth-character next-char-internal))
                       (and (is-halfwidth-character current-char) (is-chinese-character next-char-internal)))
                   (not (eq current-char ?\s))) ; Check that the current character is not a space
          (forward-char)
          (insert " ")))
      (forward-char))
    (let ((buffer-content (buffer-string)))
      (if prev-char
          (setq buffer-content (substring buffer-content 1)))
      ;; Add space between the last char of pasted text and next-char
      (if (and next-char
               (or (and (is-chinese-character (aref buffer-content (1- (length buffer-content)))) (is-halfwidth-character next-char))
                   (and (is-halfwidth-character (aref buffer-content (1- (length buffer-content)))) (is-chinese-character next-char)))
               (not (eq next-char ?\s)))
          (setq buffer-content (concat buffer-content " ")))
      buffer-content)))

(defun add-space-between-chinese-and-english-in-region (start end)
  "Add spaces between Chinese and English characters in the selected region from START to END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    ;; Adjust END to account for any spaces added during processing
    ;; Use 't' so the marker advances when text is inserted at its position
    (let ((adjusted-end (copy-marker end t)))
      (while (< (point) adjusted-end)
        (let ((current-char (char-after (point)))
              (next-char (char-after (1+ (point)))))
          (when (and current-char next-char
                     (or (and (is-chinese-character current-char) (is-halfwidth-character next-char))
                         (and (is-halfwidth-character current-char) (is-chinese-character next-char)))
                     (not (eq next-char ?\s)))
            (save-excursion
              (goto-char (1+ (point)))
              (insert " ")))
          (forward-char 1))))))

(defun remove-space-between-chinese-and-english-in-region (start end)
  "在选中区域 START 到 END 内，删除中英文字符之间的空格。"
  (interactive "r")
  (save-excursion
    (goto-char start)
    ;; 使用循环遍历区域内的空格
    (while (re-search-forward "[[:space:]]+" end t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0)))
        (let ((prev-char (char-before match-start))
              (next-char (char-after match-end)))
          (when (and prev-char next-char
                     (or (and (is-chinese-character prev-char) (is-halfwidth-character next-char))
                         (and (is-halfwidth-character prev-char) (is-chinese-character next-char))))
            ;; 删除空格
            (delete-region match-start match-end)))))))

;;;###autoload
(define-minor-mode auto-space-mode
  "在中英文之间自动添加空格的模式。"
  :lighter " Auto-Space"
  :global t
  (if auto-space-mode
      (add-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)
    (remove-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)))

(provide 'auto-space-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-space-mode.el ends here
