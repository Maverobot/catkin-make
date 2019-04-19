(defun catkin-make--recursively-up-find-file (search-path target-file-name)
  (let ((parent-dir (expand-file-name (directory-file-name (file-name-directory search-path)))))
    (if (file-exists-p (expand-file-name target-file-name parent-dir)) parent-dir
      (if (string= parent-dir "/") nil
        (catkin-make--recursively-up-find-file parent-dir target-file-name)))))

(defun catkin-make--find-current-catkin-workspace ()
  (catkin-make--recursively-up-find-file (spacemacs--file-path) ".catkin_workspace"))

(defun catkin-make--start-process ()
  (start-process
   "catkin_make"
   "*catkin_make*"
   "catkin_make"
   "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=Release")
  )

(defun catkin-make-compile-current-workspace()
  "Tidies the HTML content in the buffer using `tidy'"
  (interactive)
  (let ((default-directory (catkin-make--find-current-catkin-workspace)))
    (if default-directory
        (progn
          (split-window-below -20)
          (other-window 1)
          (switch-to-buffer (get-buffer-create "*catkin_make*"))
          (erase-buffer)
          (let ((process
                 (catkin-make--start-process)))
            (with-current-buffer (process-buffer process)
              (require 'shell)
              (shell-mode)
              (read-only-mode)
              (set-process-filter process 'comint-output-filter))))
      (message "The current file is apparently not under a ROS workspace. If it is, try to initialize the workspace with catkin_make first.") )))
