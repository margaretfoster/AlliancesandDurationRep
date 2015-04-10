(TeX-add-style-hook
 "Poster_Template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("baposter" "landscape" "paperheight=24in" "fontscale=.45" "paperwidth=36in" "final")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1")))
   (TeX-run-style-hooks
    "latex2e"
    "baposter"
    "baposter10"
    "times"
    "calc"
    "amsmath"
    "amssymb"
    "relsize"
    "multirow"
    "bm"
    "tikz"
    "graphicx"
    "float"
    "multicol"
    "subfigure"
    "color"
    "pgfbaselayers"
    "fontenc"
    "ae")
   (TeX-add-symbols
    "compresslist")))

