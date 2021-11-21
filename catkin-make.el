;;; catkin-make.el --- an emacs package which calls `catkin_make' from the current file  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(defvar catkin-make--output-buffer "*catkin_make*")
(defvar catkin-make--args "-DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")

(defun catkin-make--recursively-up-find-file (search-path target-file-name)
  "Try to find a file named TARGET-FILE-NAME recursively up in the directory tree starting from SEARCH-PATH."
  (let ((parent-dir (expand-file-name (directory-file-name (file-name-directory search-path)))))
    (if (file-exists-p (expand-file-name target-file-name parent-dir)) parent-dir
      (if (string= parent-dir "/") nil
        (catkin-make--recursively-up-find-file parent-dir target-file-name)))))

(defun catkin-make--find-current-catkin-workspace ()
  "Find the ROS root path of the current file."
  (catkin-make--recursively-up-find-file (spacemacs--file-path) ".catkin_workspace"))

(defun catkin-make--start-process (args)
  "Run `catkin_make' with ARGS as arguments."
  (start-process-shell-command
   "catkin_make"
   catkin-make--output-buffer
   (concat "catkin_make " args)))

(defun catkin-make-compile-current-workspace(args)
  "Run `catkin_make' with arguments ARGS for the workspace, where the edited file belongs."
  (let ((default-directory-tmp (catkin-make--find-current-catkin-workspace)))
    (if default-directory-tmp
        (progn
          (if args
              (setq args (or (unless (string-empty-p (string-trim args)) args) catkin-make--args))
            (setq args catkin-make--args))
          (message (concat "catkin_make " args " in " default-directory-tmp))
          (let* ((output-buffer-window (get-buffer-window catkin-make--output-buffer 'visible)))
            (if output-buffer-window
                ;; Select the output buffer window if it is already visible
                (select-window output-buffer-window)
              ;; Otherwise, create a new window below, select it and switch to the output buffer
              (split-window-below -20)
              (other-window 1)
              (switch-to-buffer (get-buffer-create catkin-make--output-buffer))))
          (let* ((default-directory default-directory-tmp)
                 (process (catkin-make--start-process args)))
            (with-current-buffer (process-buffer process)
              (require 'shell)
              (shell-mode)
              (read-only-mode)
              (set-process-filter process 'comint-output-filter))))
      (message "The current file is apparently not under a ROS workspace. If it is, try to initialize the workspace with catkin_make first.") )))

(defun catkin-make-compile-default ()
  "Run `catkin_make' of current workspace with default arguments."
  (interactive)
  (catkin-make-compile-current-workspace nil))

(defun catkin-make-compile-with-args ()
  "Run `catkin_make' of current workspace with arguments given by minibuffer."
  (interactive)
  (catkin-make-compile-current-workspace (read-from-minibuffer "catkin_make args: ")))

(defun catkin-make-keybinding-setup ()
  "Set up keybindings for package `catkin-make.'"
  (spacemacs/declare-prefix "R" "catkin-make")
  (spacemacs/declare-prefix "Rc" "compile")
  (spacemacs/declare-prefix "RC" "compile with args")
  (spacemacs/set-leader-keys "Rc" 'catkin-make-compile-default)
  (spacemacs/set-leader-keys "RC" 'catkin-make-compile-with-args))

(require 'company)
(defconst catkin-make--args-completions
  '("-DCMAKE_BUILD_TYPE="
    "-DCMAKE_INSTALL_PREFIX="
    "-DCMAKE_EXPORT_COMPILE_COMMANDS="
    "-DCATKIN_DEVEL_PREFIX="
    "-DCATKIN_ENABLE_TESTING="
    "format" "tidy"))

(defun catkin-make--company-backend (command &optional arg &rest ignored)
  "`company-mode' backend for `catkin_make' arguments."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'catkin-make--company-backend))
    (prefix (company-grab-symbol))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      catkin-make--args-completions))))

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(catkin-make--company-backend))
            (company-mode)))

(provide 'catkin-make)
;;; catkin-make.el ends here
