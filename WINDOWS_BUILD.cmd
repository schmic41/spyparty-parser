raco make "spyparty_replay_parser.rkt" "glicko.rkt" "helpers.rkt" "parser.rkt"

raco exe -o "SpyParty_Replay_Viewer" --cs --gui --ico "favicon.ico" --embed-dlls "spyparty_replay_parser.rkt"

raco distribute "SpyParty_Replay_Viewer" SpyParty_Replay_Viewer.exe