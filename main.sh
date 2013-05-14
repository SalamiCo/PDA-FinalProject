#!/bin/bash

declare -r MAIN="main"
declare -r EXTPL=".pl"
declare -r EXTPLF=".plf"
declare -r PROLOG="Prolog"
declare -r SRCDIR="src"
declare -r BINDIR="bin"
declare -r LIBDIR="lib"
declare -r FROLOG="$LIBDIR/frolog.pl"

function Prolog () {
  if command -v swipl >/dev/null 2>&1; then
    swipl "$@"
  elif command -v swipl-win >/dev/null 2>&1; then
    swipl-win "$@"
  fi
}

cd "$(dirname "$0")"

echo -e "\033[mCompiling \033[33m$EXTPLF\033[m files to \033[33m$EXTPL\033[m..."
for FILE in $(find src -name "*.plf" | sort); do
  if [ -f "$FILE" ]; then
    OUT="${FILE%.*}$EXTPL"
    DEST="$BINDIR/${OUT#*/}"

    if [ ! -e "$DEST" ] || [ "$DEST" -ot "$FILE" ]; then
      echo -ne "\033[m  Compiling \033[34m$FILE\033[m to \033[34m$DEST\033[m... "
      echo "consult('$FROLOG'). expand('${FILE%.*}')." | "$PROLOG" >/dev/null 2>&1

      if [ -e "$OUT" ]; then
        mkdir -p "$(dirname "$DEST")"
        mv "$OUT" "$DEST"
        echo -e "\033[32mok"
      else
        echo -e "\033[31mfail"
      fi
    fi
  fi
done

echo -e "\033[mRunning the \033[33mmain.pl\033[m file..."
"$PROLOG" --quiet --nodebug -s "$BINDIR/$MAIN$EXTPL" -g "main, halt"
