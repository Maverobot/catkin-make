(defun catkin-make--recursively-up-find-file (search-path target-file-name)
  (let ((parent-dir (expand-file-name (directory-file-name (file-name-directory search-path)))))
    (if (file-exists-p (expand-file-name target-file-name parent-dir)) parent-dir
      (if (string= parent-dir "/") nil
        (catkin-make--recursively-up-find-file parent-dir target-file-name)))))

(defun catkin-make--find-current-catkin-workspace ()
  (catkin-make--recursively-up-find-file (spacemacs--file-path) ".catkin_workspace"))

(defun catkin-make-compile-current-workspace()
  "Tidies the HTML content in the buffer using `tidy'"
  (interactive)
  (let ((default-directory (catkin-make--find-current-catkin-workspace)))
    (if default-directory
        (async-shell-command
         ;; command and parameters
         "catkin_make -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=Release"
         ;; output buffer
         "*catkin_make Output*"
         ;; name of the error buffer
         "*catkin_make Error")
      (message "The current file is apparently not under a ROS workspace. If it is, try to initialize the workspace with catkin_make first.") )))
