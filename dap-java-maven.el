;;; dap-java-maven.el --- Debugging Java Maven tests in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Daniel Muñoz

;; Author: Daniel Muñoz <demunoz2@uc.cl>
;; Maintainer: Daniel Muñoz <demunoz2@uc.cl>
;; Created: August 23, 2025
;; Version: 0.1.2
;; Keywords: dap, java, maven, test, debug
;; Package-Requires: ((emacs "25.1") (dap-mode "0.2") (lsp-java "0.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Test and debug package for maven testing projects. For debugging  
;; calls mvn surefire plugin, so it should be able to work on your 
;; maven project.

;;; Code:

(require 'lsp-java)
(require 'dap-mode)
(require 'dap-java)

(defun dap-java-maven-run-test-at-point ()
  "Run the single Maven test at point from the project's root directory."
  (interactive)
  ;; Use the built-in project.el to find the project.
  (if (lsp-java--get-root)
      ;; If a project is found, proceed.
      (let* ((test-to-run (dap-java-test-method-at-point)))
        (if test-to-run
            (dap-start-debugging (list :program-to-start (format "mvn -Dtest=%s test" test-to-run)
                                       :name test-to-run
                                       :environment-variables `(("TERM" . "xterm-256color"))
                                       :cwd (lsp-java--get-root)
                                       :skip-debug-session t))
          (message "Could not find a test method at point.")))
    ;; If no project is found, print an error.
    (message "Not inside a known project.")))

(defun dap-java-maven-run-test-class ()
  "Run the single Maven test at point from the project's root directory."
  (interactive)
  ;; Use the built-in project.el to find the project.
  (if (lsp-java--get-root)
      ;; If a project is found, proceed.
      (let* ((test-to-run (dap-java-test-class)))
        (if test-to-run
            (dap-start-debugging (list :program-to-start (format "mvn -Dtest=%s test" test-to-run)
                                       :name test-to-run
                                       :environment-variables `(("TERM" . "xterm-256color"))
                                       :cwd (lsp-java--get-root)
                                       :skip-debug-session t))
          (message "Could not find a test method at point.")))
    ;; If no project is found, print an error.
    (message "Not inside a known project.")))

;; Run maven clean install skipping tests
(defun dap-java-maven-build-project ()
  "Build and update dependencies maven project skipping tests."
  (interactive)
  ;; Use the built-in project.el to find the project.
  (if-let (project (project-current))
      ;; If a project is found, set the directory and run the command.
      (let ((default-directory (project-root project)))
        (compile "mvn clean install -DskipTests -U"))
    ;; If no project is found, print an error.
    (message "Not inside a known project.")))

(defun dap-java-maven-debug-test-at-point ()
  "Debug the test at point using Maven launched by dap-mode."
  (interactive)
  (let* ((port (dap--find-available-port))
         (test-to-run (dap-java-test-method-at-point))
         (project-root (project-root (project-current)))
         (debug-command (format "mvn -Dmaven.surefire.debug=-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=%s -Dtest=%s test"
                                port test-to-run))
         )
    (dap-debug
     (list :name (format "Debug %s" test-to-run)
           :enviroment-variables `(("TERM" . "xterm-256color"))
           :type "java"
           :request "attach"
           :hostName "localhost"
           :wait-for-port t
           :port port
           :cwd (lsp-java--get-root)
           :program-to-start debug-command))
    ))

;;;###autoload(with-eval-after-load 'dap-java (require 'dap-java-maven))

(provide 'dap-java-maven)

;;; dap-java-maven.el ends here
