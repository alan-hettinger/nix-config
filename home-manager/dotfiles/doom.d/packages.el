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
   :repo "DEADB17/ob-racket"
   :files ("*.el" "*.rkt")
   )
  )

(package! doom-modeline
  :pin "6125309c2caa3c98591a4c802e9b4dd2f7ea83e9")
