;; Support for compiling in subdirectories from Emacs. Adapted from Coq source.
((nil
  . ((eval
      . (progn
          ;; root directory (ending with slash)
          (let ((spartan-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (spartan-project-find-file
                 (and (boundp 'spartan-project-find-file) spartan-project-find-file)))

            ;; spartan tags file
            (when spartan-root-directory
              (setq tags-file-name (concat spartan-root-directory "TAGS"))
              (add-to-list 'compilation-search-path spartan-root-directory)
              ;; Setting the compilation directory to spartan root. This is
              ;; mutually exclusive with the setting of default-directory
              ;; below.
              (if (not spartan-project-find-file)
                  (setq compile-command (concat "make -C " spartan-root-directory)))
              )
            (setq spartan-executable (concat spartan-root-directory "spartan.native")))))))
 (tuareg-mode
  (show-trailing-whitespace . t))
 (spartan-mode
  (show-trailing-whitespace . t)))
