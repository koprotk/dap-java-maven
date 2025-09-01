# dap-java-maven
This is an extension of the classic Emacs DAP to work with Maven (Java) projects. The features are:
- Build a project skipping tests
- Run a test method
- Run a test class

If you wish to install it on Doom Emacs, just put this code in your `package.el`:
    (package! dap-java-maven :recipe (:host github :repo "koprotk/emacs-dap-java-maven"))
