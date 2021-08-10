# ibdsim2-shiny 1.3.0

## New features

* A set of observed data (IBD segment lengths) can now be supplied by the user and will appear in the density plots.

## Bug fixes

* Missing (or identical) labels are now caught.


# ibdsim2-shiny 1.2.2

* Use `ibdsim2` version 1.4.0.

* Added missing import statement of `lubridate` package.


# ibdsim2-shiny 1.2.1

## New features

* Intro text: Added link to QuickPed; other minor modifications.

* Added analysis date/time in output text files.


# ibdsim2-shiny 1.2.0

## New features

* Added a brief introduction and links to changelog and github repos.

* Moved "General options" down below the plots.


# ibdsim2-shiny 1.1.0

## New features

* Add `Download` button.

* Plotting is much faster thanks to an optimisation of `ibdsim2::findPattern()`.

* Memory consumption is substantially reduced, especially for big pedigrees. (Achieved by storing simulation data only for the selected individuals.

* Better error message when pedigree is too big for plotting region.

* Improved plot of 3/4-siblings (when chosen from the builtin list).

* The "Simulate" buttons are now disabled until some parameter changes.


# ibdsim2-shiny 1.0.1

* Initial version