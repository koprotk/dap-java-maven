# Emacs DAP Java Maven

A companion package for `dap-mode` to simplify debugging and running Java tests in Emacs for Maven-based projects. Easily build your project, run a single test method, or execute an entire test class without leaving your editor.

---

## Features

- **Build Project:** Quickly compile your project and install the artifact, skipping the often time-consuming test phase (`mvn clean install -DskipTests`).
- **Run Test Class:** Execute all tests within the current Java file.
- **Run Test Method:** Execute the single `@Test` method where your cursor is currently located.

---

## Prerequisites

Before using this package, you must have:

1.  A working installation of [dap-mode](https://emacs-dap.github.io/dap-mode/).
2.  A Java Development Kit (**JDK**).
3.  **Apache Maven** installed and available in your system's PATH.

---

## Installation

### Doom Emacs

1.  Add the following line to your `packages.el` file:
    ```elisp
    (package! dap-java-maven :recipe (:host github :repo "koprotk/emacs-dap-java-maven"))
    ```

2.  Run `~/.emacs.d/bin/doom sync` on your command line to install the package.

### Vanilla / `use-package`

Add the following to your `init.el`. You will need to clone this repository manually and ensure it is on your `load-path`.

```elisp
(use-package dap-java-maven
  :ensure nil ;; Assumes you have cloned the repo manually
  :commands (dap-java-maven-build-skip-tests
             dap-java-maven-run-test-class
             dap-java-maven-run-test-method)
  :config
  ;; Optional: Add keybindings for convenience. This example uses `general.el`.
  (map! :leader
        :prefix "d"
        :prefix "j"
        "b" #'dap-java-maven-build-skip-tests
        "c" #'dap-java-maven-run-test-class
        "m" #'dap-java-maven-run-test-method))
