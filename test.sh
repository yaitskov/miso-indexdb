#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

set -e

HOST="127.0.0.1"
PORT=${PORT:-33310}
NOBROWSER=${NOBROWSER:-N}
KEEP_BROWSER=N
CONSOLE_TO_TERM="-fghci-browser-redirect-wasi-console"

while [ $# -ne 0 ] ; do
    case "$1" in
        -nb) NOBROWSER=Y;;
        -host) HOST="$2"; shift ;;
        -no-log-to-term) CONSOLE_TO_TERM="" ;;
        -p|-port) PORT="$2" ; shift ;;
        -k|-keep) KEEP_BROWSER=Y; CONSOLE_TO_TERM="" ;;
        -h|--help|-help)
            cat<<EOF
The script runs Haskell tests in browser
Options:
  -nb        don't launch browser automatically
  -k --keep  keep browser after test completes (default $KEEP_BROWSER)
  -p -port   default ($PORT)
  -host      default ($HOST)
  -no-log-to-term
             disable redirect console.log() to termnial (default Y, but -keep flips)

EOF
            exit 1;;
        *) echo "BAD option [$1]" ; exit 1 ;;
    esac
    shift
done
function portReady() {
    timeout 1 bash -c "cat < /dev/null > /dev/tcp/$HOST/$PORT" 2>/dev/null
}
function launchBrowserWhenReady() {
    echo "Launch browser once port $PORT is ready"
    portReady || {
        echo "Port $PORT is closed. Wait & Retry"
        while : ; do
            portReady && { echo "Port $PORT is Open. Break." ; break ; }
            echo -n "."
            sleep 1
        done
        echo
    }
    chromium "http://$HOST:$PORT/main.html" 2>/dev/null
}
function browserPids() {
    ps aux | grep chromium | grep -v "grep chromium" | awk '{print $2}'
}

if [ $NOBROWSER == "N" ] ; then
    CHROME_PIDS_BEFORE="$(browserPids)"
    launchBrowserWhenReady &
    trap "kill $!" EXIT
fi

echo main | wasm32-wasi-cabal \
                --verbose repl browser \
                --repl-options="-fghci-browser $CONSOLE_TO_TERM -fghci-browser-port=$PORT -fghci-browser-host=$HOST" \
                -v0

if [ $NOBROWSER == "N" ] && [ $KEEP_BROWSER == "N" ] ; then

    CHROME_PIDS_AFTER="$(browserPids)"
    CHROME_PIDS_TO_KILL="$(comm -3 <(echo "$CHROME_PIDS_AFTER") <(echo "$CHROME_PIDS_BEFORE"))"
    for cpid in $CHROME_PIDS_TO_KILL ; do kill $cpid || true; done
    echo Browser tabs are closed
fi
