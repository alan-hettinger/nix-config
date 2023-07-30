(package! tldr)
(package! pandoc)
(package! nov)
(package! evil-better-visual-line)
(package! catppuccin-theme)
(package! dired-subtree)
(package! dired-single)
(package! dired-open)
(package! mixed-pitch)
(package! olivetti)
(package! sicp)
(package! ob-racket :recipe
  (:host github
   ;; :repo "hasu/emacs-ob-racket"
   :repo "DEADB17/ob-racket"
   :files ("*.el" "*.rkt")
   )
  )
