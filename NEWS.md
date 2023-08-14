# Change log of the R package 'chronosphere'

# chronosphere 0.6.0 - 2023-08-14

### Changed
- **IMPORTANT**: Dataset identifier coordinates were changed: `dat` -> `src`, `var` -> `ser`. This reflects changes of the database that were necessary so formal variables can be introduced. 

* * *

# chronosphere 0.5.0 - 2023-05-23

### Added
- the `configure()` function to support setting of global variables: curl, timeout and remote (debugging)
- `'tinytest'` - dependencies
- `'curl'` as suggest, curl support for `datasets()` and `fetch()`
- new example data to show the use of `datadir`

### Removed
- plate tectonic reconstruction-related functionality to package `'rgplates'`
- Spatial and RasterArrays refactored into package `'via'`
- `shaper()` function, color `palettes()` to `'restools'` (to be published soon)

* * *

# chronosphere 0.4.1 - 2021-04-16 (build 80)
### Added 
- added the platemodel example to the package's files
- fixed wrong example

* * *

# chronosphere 0.4.0 - 2020-10-18 (build 79 - CRAN submission take 2)
### Added 
- vignette url fix


* * *

# chronosphere 0.4.0 - 2020-10-18 (build 78 - Initial CRAN submission)
### Added 
- minor fixes


* * *

# chronosphere 0.4.0 (build 77) - 2020-10-13
### Added
- package collate in DESCRIPTION
- The SpatialArray class is added to the package, mimicking RasterArrays for vector objects. 
- The SpatialList class is added, to be used in the SpatialStack. 
- The XArray class-union is made for the efficient generalization of methods shared by RasterArray and SpatialArray. Shared methods are impplemented.
- the spTransform() function is implemented for SpatialStacks and SpatialArrays.
- the apply() function gained the MARGIN=NULL submethod. THis submethod is useful for the iteration of functions for every XArray item.
- The mapedge() function for the quick calculation of map edges in non-equirectangular projections.
- the 'coasts' data demo object of the PaleoMAP Paleocoastlines
- types() generic and SpatialArray-method is added.


### Changed
- Multiple RasterArray methods are now generalized for the XArray and are no longer specific to RasterArrays.
- vignette was renamed from chronos.Rmd to chronosphere.Rmd, references to 'clim' were removed and uses 'coasts' now instead

### Removed
- the reporting tools added in build 75 are removed as they require further testing
- the 'clim' demo object was removed to decrease the size of the package

### Known issues/missing features
- SpatialArrays and SpatialStacks do not have a default plotting method yet.
- The apply() function has limit utility on SpatialArrays.
- rare issues with res/ver defaulting, use explicit ver and res to ensure correct download

* * *

# chronosphere 0.3.1 (build 76) - 2020-09-23
### Added
- Forced UTF-8 encoding for the registry tables
- added API call to chronosphere object attributes.

* * *

# chronosphere 0.3.1 (build 75) - 2020-09-20
### Added
- create_metadata() and report() to create data report based on the pbdb download
- generate_bib() and related functions to generate bibliography for report
- capitalize() to capitalise strings - for formatting purposes
- pkg_file() to access files within the package
- template files to inst/rmarkdown/

* * *

# chronosphere 0.3.1 (build 74) - 2020-08-20
### Added
- The SpatialLinesDataFrame-method of the reconstruct() function.


* * *

# chronosphere 0.3.1 (build 73) - 2020-08-07
### Added
- The reference() and info() functions. The new reference() function is now used within fetch().

### Fixed
- minor bugs.


* * *

# chronosphere 0.3.0 (build 72) - 2020-07-30
### Added
- The 'plateperiod' argument was accidentally deleted during update to build 69, now it is live again. 


* * *

# chronosphere 0.3.0 (build 71) - 2020-07-14
### Changed
- Fixed forced download of registry table when datadir was given and data were already present on disk/storage device


* * *

# chronosphere 0.3.0 (build 70) - 2020-06-11
### Added
- datasets() function gained the 'dat', 'master'  and 'greetings' arguments. The default setting of the function only downloads the list of datasets (dat) and variables (var). Setting the 'dat' argument to a character entry will download the list of all archives from that dataset. Setting the master argument to 'master=TRUE' will download a list of all archives, which is expected to be very long in the near future. The argument 'greetings=TRUE' will display a reminder that additional versions and resolutions are available.
- the downloaded objects now have chronosphere attributes. These record the accession information used previously, which allows the repetition of function call, if necessary - including changes.
- fetch() can return the function call, using call=TRUE. This can be either an expression or a terminal message, depending on the settings of call.expr=TRUE/FALSE.
- fetch() can be used with an already downloaded the chronosphere-object to either redownload it, or get its download function call.
- fetch() can be used to return multiple variables from the same dataset. These will be concatenated into a list, unless they are RasterArrays, in which case fetch() will try to cbind() them
- the extent() method of the RasterArray class
- the server log log.csv is only checked once per session.

### Removed
- The dataindex() deprecated placeholder function was removed. Use datasets() instead.
- previous updates to reconstruct()


* * *

# chronosphere 0.2.2 (build 69) - 2020-03-11
### Changed
- reconstruct - defense against bad ages
- rotationModels() and validCoords() added


* * *

# chronosphere 0.2.2 (build 68) - 2020-03-11
### Changed
- reconstruct - defense against bad long/lat


* * *

# chronosphere 0.2.2 (build 67) - 2020-03-06
### Changed
- bug fixes


* * *

# chronosphere 0.2.2 (build 66) - 2020-03-05
### Added
- new method extract('RasterArray', 'matrix')
- by=NULL is added as to extract('RasterArray', 'data.frame') 

### Changed
- bug fix of newbounds() when col was given.
- by=NULL is the new default of extract('RasterArray', 'data.frame'). The function was redesigned to accomodate n-dimensional RasterArray input.
- bug fix of dimnames('RasterArray')


* * *

# chronosphere 0.2.2 (build 65) - 2020-02-20 
## Added
- plateperiod argument of the reconstruct() function


* * *

# chronosphere 0.2.2 (build 64) - 2020-02-20 
## Changed
- mapplot overlap offset fixed


* * *

# chronosphere 0.2.2 (build 63) - 2020-02-18 
## Added
- rotate-method for the RasterArray class


* * *

# chronosphere 0.2.2 (build 62) - 2020-02-17 
## Added
- zzz.R with chronosphere package help file

### Changed
- the dataindex() function was renamed to datasets()


* * *

# chronosphere 0.2.1 (build 61) - 2020-02-14 
### Changed
- mapplot() coordinate reset fixed
- IPCC palettes added


* * *

# chronosphere 0.2.1 (build 60) - 2020-02-12 
### Changed
- corrected documentation problems


* * *

# chronosphere 0.2.1 (build 59) - 2020-01-27 
### Added
- support for shapefile fetching

### Changed
- fetch() defaults to the coarsest resolution (highest res entry, new default is res=NULL)


* * *

# chronosphere 0.2.1 (build 58) - 2020-01-12 
### Added
- the nums(), colnums() and rownums() functions
- the ... argument to fetch to reach variable-specific loading options

### Changed 
- Fixed issue with offline reconstruction method (one entry in an age with enumerate = FALSE)


* * *

# chronosphere 0.2.0 (build 57) - 2019-12-11 (CRAN initial submission, take 3)
### Changed 
- replaced all occurrences of T and F with TRUE and FALSE respectively
- on.exit statements for mapplot() and showPal()
- RasterArray constructor now works for stacks that do not have the same length as the dim product (with warning)
- Fixed bug with colnames and rownames assignment
- t() copies colnames and rownames attributes
- Raster variable loading is now done with R code provided by the server
- renamed NEWS file to NEWS.md


* * *

# chronosphere 0.2.0 (build 56) - 2019-12-03 (CRAN initial submission, take 2)
### Added 
- return value documentation for all functions
- on.exit() statements where options and par are changed.

### Changed
- description field in DESCRIPTION
- LazyData is set to false
- usage entries for 'dems' and 'clim'


* * *

# chronosphere 0.2.0 (build 55)  - 2019-11-29 (CRAN initial submission)
### Added 
- support for NAs in the RasterArray constructor and defenses
- 'clim' data object
- bug fix for the apply() RasterArray method

### Changed 
- [[ of RasterArrays now wrap output in a RasterArray by default.
- reconstruct() function's local submodule no longer returns coordinates for points that are situated on plates that did not exist on the at reconstruction date (matrix method returns NA coordinates, Sp methods omit)


