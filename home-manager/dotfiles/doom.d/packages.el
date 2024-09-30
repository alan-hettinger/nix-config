(package! tldr)
(package! pandoc)
(package! nov)
(package! sicp)
(package! ob-racket :recipe
          (:host github
                 :repo "DEADB17/ob-racket"
                 :files ("*.el" "*.rkt")
                 )
          )
