(defun catkin-make--recursively-up-find-file (search-path target-file-name)
  (let ((parent-dir (expand-file-name (directory-file-name (file-name-directory search-path)))))
    (if (file-exists-p (expand-file-name target-file-name parent-dir)) parent-dir
      (if (string= parent-dir "/") nil
        (catkin-make--recursively-up-find-file parent-dir target-file-name)))))

(defun catkin-make-find-current-catkin-workspace ()
  (interactive)
  (message (catkin-make--recursively-up-find-file (spacemacs--file-path) ".catkin_workspace")))

(defun shell-command-test()
  (interactive)
  (async-shell-command "pwd"))
