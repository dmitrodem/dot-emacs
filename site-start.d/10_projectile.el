(require 'projectile)

(setq projectile-indexing-method 'native)
(projectile-register-project-type 'grlib '(".grlib_design")
                                  :project-file "Makefile"
                                  :compile "make vsim"
                                  :configure "make xconfig")
