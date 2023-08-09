(use-package ligature
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures '(prog-mode)                            
                          '("->" "<-" "-->" "<--" "<-->"
                            "|>" "<|" "=>" "==>" "<!--"
                            "::")))
                          
