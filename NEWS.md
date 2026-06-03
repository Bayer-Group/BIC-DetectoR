# DetectoR (development version)
* Add jarl linter
* Apply jarl fixes
* Add manifest.json
* Add `rsconnect` to Suggests
* Update README
* Add GHA for R CMD CHECK, air, jarl
* Update DESCRIPTION file
* Update NEWS
* Make renv lockfile explicit

# DetectoR 3.0.0

## Overview

This prepares DetectoR to move to public repository by allowing the user to upload MedDRA data, instead of storing it inside the app (because it's proprietary data). It also comes with a rework of the UI, adoption of golem framework, and some quality-of-life changes 😎.

Please review the functionality of the app and compare its results with those of the main branch and study TLFs, to ensure it's ready for production! 😊

## Breaking Changes
- MedDRA data now should be uploaded by the user.

## New Features
- MedDRA data now should be uploaded by the user, instead of loading internal package data.
- Data filters remember their previous state (i.e., there's no need to reset the filters every time a new filter is added) #38.
- AE groupings (SOC/PTs/MLGs/SMQs) and common AE types (treatment-emergent, serious, etc.), can now be filtered directly using a selector on top of the plot (#41 #42).
- Key variables now can be mapped from the available variables in the datasets (i.e., safety flag, treatment-emergent flag, duration of exposure variables, etc.). This makes unnecessary creating derived variables (like numeric SAFFN instead of character SAFFL) and allow flexibility for future CDISC changes.
- ADAE/ADSL/MedDRA files now can be uploaded in different formats (R, SAS, csv) (#39).

## Removed Features
- Removed "download report" feature, as it didn't work for all plots and its utility is unclear.
- Removed patient information pop-up on Double Dot Plot, as it wasn't used in DIG meetings, needed rework to function properly and can be substituted by the Dataset view tab. This can be re-implemented in the future if it's still relevant (it'll need to be adapted to plotly).

## User Interface Changes
- Light/dark theme support.
- Enhanced color contrast for accessibility.
- New, modern color palette inspired by Material Design and Bayer brand image.
- The app features Bayer and DIG logos, as well as Bayer color palette and font (Bayer Sans).
- Simplified flow of the app, with large buttons showing the next step (e.g., "Upload Data", "Go to Double Dot Plot").
- Improved validation feedback of uploaded files (ADAE, ADSL and MedDRA datasets).
- Information about total and filtered number of subjects and AEs now shows on Data Upload and Data Filter tabs, to make it easier to check that the filters work as expected.
- New tooltip below the plots shows the current active data filters.
- All plots are now interactive and powered by `{plotly}`.
- Plots now are larger and essential information is shown more prominently. Sidebar is hidden by default, and p-values for Double Dot Plot are shown on hover, instead of on separate plots (as per client's feedback #47 #43).
- Double Dot Plot axes now always start at 0%, to keep comparison consistent when filtering AE.
- Statistical significance on Double Dot Plot is now also shown with color codes (red for verum, blue for comparison) and asterisks.
- Improved appearance of help text on the app (Welcome, About and Data Manual tabs), with larger font sizes, text emphasis and better whitespace.

## Bug Fixes
- Filtering specific AE groupings using ADAE filters didn't work as intended before (#45 #46). Now all the PT/MLGs/etc. that are present in the data can be filtered.
- The app no longer crashes or throw error messages when data filtering results in zero rows. Now an empty plot or a descriptive shiny error message appears instead.

## Back-End Code Changes
- Adopted golem framework.
- Removed old, unused, and private code (e.g., yavin connection, old list of study names).
- Reduced code duplication and simplified functions.
- Homogenized coding style following tidyverse conventions and Air formatter.
- Reworked module structure. Now a single module is in charge of calculations (mod_calculate), and we have one module per plot view (mod_graph, mod_heatmap, mod_volcano, mod_table). Internal communication of objects between modules follows the "strategy of the petit r", with a reactive list `r`.
- All inline and internal CSS and JavaScript now sits on a separate external .css and .js files.
- CSS file can be used to change app colors, including plotly colors for Double Dot Plot and Volcano Plot.
- The app is less reactive and waits until a button is clicked, to reduce unintended plot updating and improve performance (#37).
- Improved automatic test coverage.
- Updated README, documentation and added code comments and structure.

# DetectoR 2.2.9

- Adoption of renvs

# DetectoR 1.0.0

- Initial version
