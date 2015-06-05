(require 'cl)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-personal)
(require 'init-shell)
(require 'init-paths)
(require 'init-file-management)
(require 'init-yas)
(require 'init-spelling)
(require 'init-display)
(require 'init-formatting)
(require 'init-smex)
(require 'init-git)
(require 'init-projectile)
(require 'init-org-mode)
(require 'init-speedbar)
(require 'init-scala)
(require 'init-helm)
(require 'init-cleanup)
(require 'init-javascript)
(require 'init-markdown)
(require 'init-theme)
;; needs to be last to have various functions available
(require 'init-keyboard)

(require 'post-init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b82a6fdc1f0a77f72b2b27437399051fb95c22c50b75571841d21b5ce22535f9" "51472d455204ae998ea0be03d21aa6d3f260983364aa75ae54a614604eef2dd4" "0f9ce726036ad2e49642eb6f1f2c532eae4eee74a11558bb593e99e828cae167" "41e9e014f545d246d9974978f9e542904c40d6d71a57a45dbe244035ba57ea06" "b10feb29e0ba2bf06fbf05bc9dea40a56c31f3ba99ef77da33fc8b7fe4965387" "d812d7e91ce9a896183300c8faee32513a14dde95fda8c59571fe389294ff81c" "5d46b06933737e5d02efe8d6e25576b967cfcbeea64a0b21508cd0864780d911" "d7cc15abed0a5b3aa8dd02ef69466fdacd52a08c867168cc94f0624bf52a0217" "56c7920158466a6ad7b5ac8d10376d747714d5517f29f72e7c133e03396b5b11" "4d2c5dc1f06ca209155ea92d0192a9cb621fa0059987fc8d42ade58d6e8e5f01" "c77bb19f6a30329b5e1066f622f3d38b766bc12e3d2640f1ac5bb6b342477129" "0eeea14fa20b96b0a39bc0951d3a157186da47bbaf4e6a3d7b228bab619cfb6b" "6d5ad253382d680052a6293ef17929d85a840db275bbbaa79226cb0c8ae9df7f" "0c03dbde145ca1a81b43a2219a006cf51dbf524058bc89c4c648915bd4f95dc2" "523f99feb0a686ed46eea16d376228a085084732d1c10f785c6486128546e1b2" "147a506e5d668e6b058e6826bf65de0f8df1a2d081568a870d1b145b72eefc80" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" default)))
 '(dired-listing-switches "-al")
 '(display-time-mode t)
 '(frame-background-mode (quote dark))
 '(tool-bar-mode nil))
