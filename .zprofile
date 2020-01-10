[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && chkboot-check && exec xinit -- -nolisten tcp vt1
