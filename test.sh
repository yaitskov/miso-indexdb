#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

HOST="127.0.0.1"
PORT=${PORT:-33310}
NOBROWSER=${NOBROWSER:-N}
KEEP_BROWSER=N

while [ $# -ne 0 ] ; do
    case "$1" in
        -nb) NOBROWSER=Y;;
        -host) HOST="$2"; shift ;;
        -p|-port) PORT="$2" ; shift ;;
        -k|-keep) KEEP_BROWSER=Y ;;
        -h|--help|-help)
            cat<<EOF
The script runs Haskell tests in browser
Options:
  -nb        don't launch browser automatically
  -k --keep  keep browser after test completes (default $KEEP_BROWSER)
  -p -port   default ($PORT)
  -host      default ($HOST)

EOF
            exit 1;;
        *) echo "BAD option [$1]" ; exit 1 ;;
    esac
    shift
done
function launchBrowserWhenReady() {
    echo "Launch browser once port $PORT is ready"
    while : ; do
        timeout 1 bash -c "cat < /dev/null > /dev/tcp/$HOST/$PORT" \
            && { echo "Port $PORT is Open. Break." ; break ; }
        echo "Port $PORT is closed. Wait & Retry"
        sleep 1
    done
    chromium "http://$HOST:$PORT/main.html" 2>/dev/null
}
if [ $NOBROWSER == "N" ] ; then
    CHROME_PIDS_BEFORE="$(ps aux |grep chromium | awk '{print $2}')"
    launchBrowserWhenReady &
fi

echo main | wasm32-wasi-cabal \
                --verbose repl browser \
                --repl-options="-fghci-browser -fghci-browser-port=$PORT -fghci-browser-host=$HOST" \
                -v0

if [ $NOBROWSER == "N" ] && [ $KEEP_BROWSER == "N" ] ; then
    CHROME_PIDS_AFTER="$(ps aux |grep chromium | awk '{print $2}')"
    CHROME_PIDS_TO_KILL="$(comm -3 <(echo "$CHROME_PIDS_AFTER") <(echo "$CHROME_PIDS_BEFORE"))"

    for cpid in $CHROME_PIDS_TO_KILL ; do kill $cpid ; done
    echo Browser tabs are closed
fi
