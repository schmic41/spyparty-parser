# The SpyParty Replay Viewer

This is a GUI utility for examining and analyzing sets of SpyParty replay files. It is a combination of the [SCL Replay Tool](https://www.spypartyfans.com/gamefinder.php) (written by LtHummus and AForgottenTune), [SpyParsey](https://github.com/adamransom/spyparsey) (written by plastikqs), and pwndnoob's ELO spreadsheet.

# How to Use

To begin, open a single replay or group of replays. "Open File" lets you manually select .replay files. Holding ctrl+click selects multiple files. "Open Directory" will search the selected directory for all .replay files automatically.

Once the files have been parsed, all of the results will appear in the main window. Double click on a replay to see a full summary. Click any column header to sort the list by the header label. The format for date/time can be changed in the "Date" menu.

"Filter" opens a dialog to filter by replay features, including start date, players, and venues. Any dates used in the filter must be in the appropriate format. To undo any active filters, use the "Reset" option.

The replay viewer allows you to run the [Glicko-2](http://www.glicko.net/glicko.html) rating algorithm on all visible replays. Glicko-2 is similar to ELO, but includes several improvements (including deviation and volatility). All Glicko-2 ratings are presented as confidence intervals. To run this analysis, select the "Run Stats" option in the stats menu. A window with a list of all players will appear. Double click any player to view their complete match history and rating updates. Inside this sub-window, selecting "Plot" from the "Plot" menu will graph the confidence intervals of the player over time.

# Current State

The Replay Viewer is operational and should be bug-free.

If you encounter any bugs, please provide the error message that appears as well as the steps necessary to reproduce the error.

# Thanks

Thanks to checker, for documenting the replay header format, and to LtHummus for his Python parser, which I used as a reference when hunting down some bad bugs.

# Build

If you would like to build the binary from scratch, clone the source repository. Compiling requires the [Racket programming language](https://racket-lang.org/), version 7.7 Regular or CS. It should work with earlier versions.

Three build scripts are provided: one for Windows, one for Mac, and one for Linux. The Windows and Linux binaries have been tested, the Mac binary has not.

Once the build script has been run, the final bundled executable will be located inside the "SpyParty_Replay_Viewer" folder, along with all necessary dependencies.

The source code is released under the MIT license.
