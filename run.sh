set -e
dune build _build/default/main.exe
_build/default/main.exe
dot -Tpng:cairo main.dot > main.png
open main.png
