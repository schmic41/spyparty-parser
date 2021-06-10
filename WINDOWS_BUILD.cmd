raco make "src/spyparty_replay_parser.rkt" "src/glicko.rkt" "src/helpers.rkt" "src/parser.rkt"

raco exe -o "SpyParty_Replay_Viewer" --gui --ico "resources/favicon.ico" --embed-dlls "src/spyparty_replay_parser.rkt"

raco distribute "SpyParty_Replay_Viewer" SpyParty_Replay_Viewer.exe