# The SpyParty Replay Viewer

This is a GUI utility for examining and analyzing sets of SpyParty replay files. It is a combination of the [SCL Replay Tool](https://www.spypartyfans.com/gamefinder.php) (written by LtHummus and AForgottenTune), [SpyParsey](https://github.com/adamransom/spyparsey) (written by plastikqs), and pwndnoob's ELO spreadsheet.

# How to Use

To begin, open a single replay or group of replays. The "Open File" dialog allows for manual selections of .replay files; holding ctrl+click allows for multiple file selections. The "Open Directory" dialog will search the selected directory for all .replay files automatically.

Once the files have been parsed, a list of synopses will appear in the main window. Double click on a replay to see the full results. Click any column header to sort the list by the appropriate feature. The format for date/time can be changed via the "Date" menu.

The "Filter" menu opens a dialog to filter by pertinent features, including start date, players, and venues. Dates used in the filter must be in the appropriate format. To undo any active filters, use the "Reset" option.

The replay viewer includes a function that will run the [Glicko-2](http://www.glicko.net/glicko.html) rating algorithm on all active replays. The Glicko-2 algorithm is similar to the familiar ELO algorithm, but includes several improvements (such as deviation and variability). All Glicko-2 ratings are presented as confidence intervals. To run this analysis, select the "Run Stats" option in the stats menu. A window with a list of all players will appear; double click any player to view their complete match history and rating updates. Inside this sub-window, selecting "Plot" from the "Plot" menu will graph the confidence intervals of the player over time.

# Current State

Currently, the Replay Viewer is fully operational and should be relatively bug-free.

If you encounter any bugs, please provide the error message that appears as well as the steps necessary to reproduce the error.

# Thanks

Thanks to checker, for documenting the replay header format, and to LtHummus for his SpyParty Replay Parser Tool in Python, which I used as a reference when hunting down some bad bugs.

# Build

If you would like to build the binary from scratch, clone the source repository. Compiling requires the [Racket programming language](https://racket-lang.org/), version 7.7 Regular or CS. It should work with earlier versions, but has not been tested.

Three build scripts are provided: one for Windows, one for Mac, and one for Linux. The Windows binary has been tested and is included; the Mac and Linux binaries have not yet been tested.

The source code is licensed under the MIT license.
