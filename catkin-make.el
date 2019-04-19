
(defvar catkin-make--output-buffer "*catkin_make*")
(defvar catkin-make--args "-DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")

(defun catkin-make--recursively-up-find-file (search-path target-file-name)
  (let ((parent-dir (expand-file-name (directory-file-name (file-name-directory search-path)))))
    (if (file-exists-p (expand-file-name target-file-name parent-dir)) parent-dir
      (if (string= parent-dir "/") nil
        (catkin-make--recursively-up-find-file parent-dir target-file-name)))))

(defun catkin-make--find-current-catkin-workspace ()
  (catkin-make--recursively-up-find-file (spacemacs--file-path) ".catkin_workspace"))

(defun catkin-make--start-process (args)
  (start-process-shell-command
   "catkin_make"
   catkin-make--output-buffer
   (concat "catkin_make " args)))

(defun catkin-make-compile-current-workspace(args)
  (let ((default-directory (catkin-make--find-current-catkin-workspace)))
    (if default-directory
        (progn
          (if args
              (setq args (or (unless (string-empty-p (string-trim args)) args) catkin-make--args))
            (setq args catkin-make--args))
          (message (concat "catkin_make " args))
          (let* ((output-buffer-window (get-buffer-window catkin-make--output-buffer 'visible)))
            (if output-buffer-window
                ;; Select the output buffer window if it is already visible
                (select-window output-buffer-window)
              ;; Otherwise, create a new window below, select it and switch to the output buffer
              (split-window-below -20)
              (other-window 1)
              (switch-to-buffer (get-buffer-create catkin-make--output-buffer))))
          (let ((process
                 (catkin-make--start-process args)))
            (with-current-buffer (process-buffer process)
              (require 'shell)
              (shell-mode)
              (read-only-mode)
              (set-process-filter process 'comint-output-filter))))
      (message "The current file is apparently not under a ROS workspace. If it is, try to initialize the workspace with catkin_make first.") )))

(defun catkin-make-compile-default ()
  (interactive)
  (catkin-make-compile-current-workspace nil))

(defun catkin-make-compile-with-args ()
  (interactive)
  (catkin-make-compile-current-workspace (catkin-make--read-from-minibuffer "catkin_make args: ")))

(defun catkin-make-keybinding-setup ()
  (spacemacs/declare-prefix "R" "catkin-make")
  (spacemacs/declare-prefix "Rc" "compile")
  (spacemacs/declare-prefix "RC" "compile with args")
  (spacemacs/set-leader-keys "Rc" 'catkin-make-compile-default)
  (spacemacs/set-leader-keys "RC" 'catkin-make-compile-with-args))

(require 'company)
(defconst catkin-make--args-completions
  '("-DCMAKE_BUILD_TYPE=Debug"
    "-DCMAKE_BUILD_TYPE=Release"
    "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
    "-DCMAKE_EXPORT_COMPILE_COMMANDS=OFF"
    "format" "tidy"))

(defun catkin-make--company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
    (case command
      (interactive (company-begin-backend 'catkin-make--company-backend))
      (prefix (company-grab-symbol))
      (candidates
       (remove-if-not
        (lambda (c) (string-prefix-p arg c))
        catkin-make--args-completions))))

(add-to-list 'company-backends 'catkin-make--company-backend)
(defun my-minibuffer-mode ()
  (company-mode))

(defun catkin-make--read-from-minibuffer (prompt)
  ;; TODO: better way to handle this temporary hook
  (defvar result)
  (add-hook 'minibuffer-setup-hook 'my-minibuffer-mode)
  (setq result (read-from-minibuffer prompt))
  (remove-hook 'minibuffer-setup-hook 'my-minibuffer-mode)
  result)
