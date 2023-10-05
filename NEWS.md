## Todo:
* *Short Term*
  * **_Finish documenting functions._**
  * Add 2 articles: "Process Space in valley floor", and "Description of outputs"
  * Add longitudinal cross section plot to KML output.
  * Add more data to cross section plots:
    * Slope upstream, downstream?
    * Identify terraces?
    * % of Cross section in "Process Space" ?
    * Suggestions?
  * Speed up cross section plot generation and add progress bar
  * Add inset map to cross section plots. Simple line of mainLine with point and cross section?
* *Medium Term*
  * Change raster interpolation away from cross section. Grid of points instead?
  * Remove need for low use packages: purrr, cowplot, smoothr, gstat, concaveman
  * Consider replacing raster package with stars package?

## ProcessSpace 0.1.05

* Fixed introduced error from sf update to st_distance
* Many small tweaks and rewrite of article


## ProcessSpace 0.1.033

* Added some documentation
* Improved code on homepage

## ProcessSpace 0.1.03

* Numerous bug fixes
* Some warning silencing because of incompatibilities in CRS definitions
* Added to 'Built-in' article
* Added version limits to Imports in DESCRIPTIONS

## ProcessSpace 0.1.02

* Fixed leftside/rightside issues.
* Removed cutDir2 argument to simplify code transfer between different segments
* Corrected issue with Process Space polygons when streamline made in Arc

## ProcessSpace 0.1.011

* Added zip package to handle kmz generation better.
* Many CRS bugs fixed

## ProcessSpace 0.1.001

* Added NEWS tab.
* Bug Fixes: lots

## ProcessSpace 0.1.0

* This is the first release of the ProcessSpace package.
