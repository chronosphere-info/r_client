# Change log of the R package 'chronosphere'

## [0.4.1] - 2021-04-16 (build 80)
### Added 
- added the platemodel example to the package's files
- fixed wrong example

## [0.4.0] - 2020-10-18 (build 79 - CRAN submission take 2)
### Added 
- vignette url fix


## [0.4.0] - 2020-10-18 (build 78 - Initial CRAN submission)
### Added 
- minor fixes


## [0.4.0 (build 77)] - 2020-10-13
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

## [0.3.1 (build 76)] - 2020-09-23
### Added
- Forced UTF-8 encoding for the registry tables
- added API call to chronosphere object attributes.

## [0.3.1 (build 75)] - 2020-09-20
### Added
- create_metadata() and report() to create data report based on the pbdb download
- generate_bib() and related functions to generate bibliography for report
- capitalize() to capitalise strings - for formatting purposes
- pkg_file() to access files within the package
- template files to inst/rmarkdown/

## [0.3.1 (build 74)] - 2020-08-20
### Added
- The SpatialLinesDataFrame-method of the reconstruct() function.


## [0.3.1 (build 73)] - 2020-08-07
### Added
- The reference() and info() functions. The new reference() function is now used within fetch().

### Fixed
- minor bugs.


## [0.3.0 (build 72)] - 2020-07-30
### Added
- The 'plateperiod' argument was accidentally deleted during update to build 69, now it is live again. 


## [0.3.0 (build 71)] - 2020-07-14
### Changed
- Fixed forced download of registry table when datadir was given and data were already present on disk/storage device


## [0.3.0 (build 70)] - 2020-06-11
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


## [0.2.2 (build 69)] - 2020-03-11
### Changed
- reconstruct - defense against bad ages
- rotationModels() and validCoords() added


## [0.2.2 (build 68)] - 2020-03-11
### Changed
- reconstruct - defense against bad long/lat


## [0.2.2 (build 67)] - 2020-03-06
### Changed
- bug fixes


## [0.2.2 (build 66)] - 2020-03-05
### Added
- new method extract('RasterArray', 'matrix')
- by=NULL is added as to extract('RasterArray', 'data.frame') 

### Changed
- bug fix of newbounds() when col was given.
- by=NULL is the new default of extract('RasterArray', 'data.frame'). The function was redesigned to accomodate n-dimensional RasterArray input.
- bug fix of dimnames('RasterArray')


## [0.2.2 (build 65)] - 2020-02-20 
## Added
- plateperiod argument of the reconstruct() function


## [0.2.2 (build 64)] - 2020-02-20 
## Changed
- mapplot overlap offset fixed


## [0.2.2 (build 63)] - 2020-02-18 
## Added
- rotate-method for the RasterArray class


## [0.2.2 (build 62)] - 2020-02-17 
## Added
- zzz.R with chronosphere package help file

### Changed
- the dataindex() function was renamed to datasets()


## [0.2.1 (build 61)] - 2020-02-14 
### Changed
- mapplot() coordinate reset fixed
- IPCC palettes added


## [0.2.1 (build 60)] - 2020-02-12 
### Changed
- corrected documentation problems


## [0.2.1 (build 59)] - 2020-01-27 
### Added
- support for shapefile fetching

### Changed
- fetch() defaults to the coarsest resolution (highest res entry, new default is res=NULL)


## [0.2.1 (build 58)] - 2020-01-12 
### Added
- the nums(), colnums() and rownums() functions
- the ... argument to fetch to reach variable-specific loading options

### Changed 
- Fixed issue with offline reconstruction method (one entry in an age with enumerate = FALSE)


## [0.2.0 (build 57)] - 2019-12-11 (CRAN initial submission, take 3)
### Changed 
- replaced all occurrences of T and F with TRUE and FALSE respectively
- on.exit statements for mapplot() and showPal()
- RasterArray constructor now works for stacks that do not have the same length as the dim product (with warning)
- Fixed bug with colnames and rownames assignment
- t() copies colnames and rownames attributes
- Raster variable loading is now done with R code provided by the server
- renamed NEWS file to NEWS.md


## [0.2.0 (build 56)] - 2019-12-03 (CRAN initial submission, take 2)
### Added 
- return value documentation for all functions
- on.exit() statements where options and par are changed.

### Changed
- description field in DESCRIPTION
- LazyData is set to false
- usage entries for 'dems' and 'clim'


## [0.2.0 (build 55)]  - 2019-11-29 (CRAN initial submission)
### Added 
- support for NAs in the RasterArray constructor and defenses
- 'clim' data object
- bug fix for the apply() RasterArray method

### Changed 
- [[ of RasterArrays now wrap output in a RasterArray by default.
- reconstruct() function's local submodule no longer returns coordinates for points that are situated on plates that did not exist on the at reconstruction date (matrix method returns NA coordinates, Sp methods omit)


## [0.1.12 (build 54)]  - 2019-11-29
### Changed
- fetching function changes and remote server file change

## [0.1.12 (build 53)]  - 2019-11-28
### Changed
- most of vignette is written
- lots of small changes
- major documentation upgrades, including examples
- mapplot() completed


## [0.1.12 (build 52)]  - 2019-11-27 
### Added
- reconstruction() local method support for MacOS
- reconstruction() local method support for custom paths under windows
- added support for default installation path  of 64bit gplates in windows
- vignette template
- lot's of minor fixes

### Fixed 
- missing drop=TRUE for age-iterated reconstruction


## [0.1.12 (build 51)]  - 2019-11-25 
### Added
- server side feedback on chronosphere version used
- data citations are displayed upon fetch()

### Changed
- Removed resolution directories from remote server


## [0.1.12 (build 50)]  - 2019-11-24 
### Added
- Windows support for the local reconstruction submodule (default path)
- defense against different projections for SPDF input to reconstruct()
- major documentation upgrade (package passes R CMD check) without notes


## [0.1.12 (build 49)]  - 2019-11-22 
### Added
- "plates" method for local reconstruction submodule

## [0.1.12 (build 48)]  - 2019-11-21 
### Added
- local reconstruction submodule to the reconstruct() function (only GNU/Linux)

### Changed
- the rangelocator() function was renamed to shaper()


## [0.1.11 (build 47)]  - 2019-11-20 
### Added
- plate tectonic model class as a preparation for GPlates-based coordinate reconstruction
- utility functions
- the PaleoReefs database

## [0.1.10 (build 46)]  - 2019-11-17 
### Changed
- raster::projectRaster()-transformed to generic, RasterArray projection function is now: projectRaster() instead of ProjectRaster()
- solved bug with subset method of RasterArrays RA[1] returns first layer instead of wrong first row.
- solved same bug with setReplacement [<- method

## [0.1.9 (build 45)]  - 2019-11-15 
### Added
- mapplot() for RasterArrays implemented

## [0.1.8 (build 44)]  - 2019-11-14 
### Added
- The function groups Arith, Compare, Math, Math2, Summary are added for RasterArrays, inheriting from RasterStacks.
- as.list
- minor updates to documentation
- default colour added to mapplot() of SpObjects

### Changed 
- The c() method for the RasterArrays is renamed to combine() due avoid compatibility issues
- minor correction to the names<- method

## [0.1.7 (build 43)]  - 2019-11-13 
### Added
- warning for calc() method when margin is multidimensional.
- the t() method for RasterArrays
- bug fix for the fetch() function


## [0.1.7 (build 42)]  - 2019-11-12 
### Added
- calc() method for 1-2 dim RasterArrays

### Changed
- defaults to fetch(verbose=TRUE)


## [0.1.7 (build 41)]  - 2019-11-08 
### Added
- support for data.frame -type download and new data
- The verbose argument of the dataindex() and fetch() functions


## [0.1.6 (build 40)]  - 2019-11-07 
### Added
- the rangelocator() function

### Changed
- argument order of mapplot()


## [0.1.6 (build 39)]  - 2019-11-06 
### Added
- colour palettes added
- colour palettes implemented for RasterArrays


## [0.1.5 (build 38)]  - 2019-11-06
### Added
- is.na() method for RasterArrays.
- demo RasterArray data
- c() method for signature ("RasterArray", "list"), otherwise the raster::compareRaster() and functions building on that would not work.
- mask() methods for RasterArrays


## [0.1.5 (build 37)]  - 2019-11-02 
### Added
- c() methods for RasterArrays.
- The ProjectRaster() function for RasterArrays.
- coercion methods for Raster* objects to RasterArrays

### Changed
- fixed bug with the mapplot function.
- R script file structure.


## [0.1.4 (build 36)]  - 2019-10-31 
### Added
- the cellStats() S4 method of RasterArrays
- the summary() S4 method of RasterArrays

### Removed
- rates of climate change


## [0.1.3 (build 35)]  - 2019-10-31 
### Added
- the 'gradinv' color ramp object
- default colors of the mapplot() function

### Removed
- dependency on the reticulate package

### Changed
- bug fix with the cbind() and rbind() methods of RasterArrays
- the mapplot() function is reorganized to a proper S4 generic and methods


## [0.1.3 (build 34)]  - 2019-10-25 
### Changed
- remote server location changed to the likely final
"https://www.cnidaria.nat.fau.de/tersane/public/chronosphere/"


## [0.1.3 (build 33)]  - 2019-10-25 
### Added
- fetch() now supports two dimensional RasterArrays.

### Changed
- fixed bug of cbind() and rbind() methods rof RasterArrays.


## [0.1.3 (build 32)]  - 2019-10-25 
### Changed
- extract() can now accept 2 dimensional RasterArrays


## [0.1.3 (build 31)]  - 2019-10-24 
### Changed
- the multiextract() function is now an extract() method defined for RasterArrays


## [0.1.3 (build 30)]  - 2019-10-23 
### Changed
- Fixed bug with NAs for the RasterArray[<- RasterLayer replacement method


## [0.1.3 (build 29)]  - 2019-10-22 
### Added
- replacement method for RasterArray, (logical) NA insertion

### Changed
- jpeg layer orders in fetch()

### Known issues
- replacement method for RasterArray, RasterLayer not yet work for NA filling!


## [0.1.3 (build 28)]  - 2019-10-21 
### Added
- replacement methods for the RasterArray-specific colnames(), rownames(), names and dimnames()
- replacement method for RasterArray elements with [[
- replacement method for RasterArray elements with [ and a single RasterLayer


## [0.1.2 (build 27)]  - 2019-10-20 
### Added
- as.data.frame S3 method for RasterArray making the View() function work
- the rbind() method for RasterArrays
- the aggregate() and disaggregate() methods for RasterArrays


## [0.1.1 (build 26)]  - 2019-10-19 
### Added
- The cbind() method for the RasterArrays... a nightmare.
- RasterArray support for missing values.
- The newbounds() utility function. 


## [0.1.0 (build 25)]  - 2019-10-18 
### Added
- The proxy() and nvalues() S4 generic and method for RasterArrays.

### Changed
- the show method of RasterArrays now shows the proxy object and the number of layers
- Fixed bug of the rownames method of RasterArrays
- The length() method of RasterLayers now returns the length of the proxy object. Use nvalues to get the number of values in the stack. 


## [0.1.0 (build 24)]  - 2019-10-16 
### Changed
- Minor fix to reconstruct() function.


## [0.1.0 (build 23)]  - 2019-10-09 
### Changed
- Server location, publicity of files and repository structure. 


## [0.1.0 (build 22)]  - 2019-10-08 
### Added
- The multiextract() function. 

### Changed
- The climate_velocity() was renamed to the much shorter 'vocc()'
- The remote server variable is now called 'csph', shorter than chronosphere.


## [0.1.0 (build 21)]  - 2019-09-7 
### Changed
- package name changed from 'earthhist' to 'chronosphere'


## [0.0.1 (build 20)]  - 2019-09-25 
### Changed
- remote server variable place


## [0.0.1 (build 19)]  - 2019-09-25 
### Added
- enumerate argument to the reconstruct()-matrix method, for different age vectorization


## [0.0.1 (build 18)]  - 2019-09-24 
### Added
- chunk argument to reconstruct()-matrix-method, limiting the number of points to be reconstrcuted in one go

### Changed
- Wrapped the gplates_reconstruct_points() function in IteratedPointReconstruction() to iteratively query points, thus evading error with too long URL


## [0.0.1 (build 17)]  - 2019-09-19 
### Changed
- tiny bug fix in fetch()


## [0.0.1 (build 16)]  - 2019-08-14 
### Added
- versions to the layer names for better robustness

### Changed
- remote server location to FAU/cnidaria
- datadir arg of fetch() is redone, archives are saved
- ages of 'sst' variable in 'earthhist' are rounded to 5s


## [0.0.1 (build 15)]  - 2019-08-09 
### Added
- the datadir argument for fetch() and for dataindex()
- some test examples for RasterArray-subset method


## [0.0.1 (build 14)]  - 2019-07-25 
### Changed
- fetch() bug fixed.


## [0.0.1 (build 13)]  - 2019-07-25 
### Changed
- fetch() bug fix, one line did not do its job, the other one, truncated the output, 
messing up the order of the layers, leading to misalignment between RasterArray proxy indices and layer names
- test examples changed for DEM


## [0.0.1 (build 12)]  - 2019-07-25 
### Added
- reconstruct() wrapper function for all gplates web serviced based functions
- verbose argument for the reconstruction internals


## [0.0.1 (build 11)]  - 2019-07-24 
### Added
- the gplates_reconstruct_polygon() was added for gplates reconstructions. 

### Changed
- fetch() age correction


## [0.0.1 (build 10)]  - 2019-07-23 
### Added
- the gplates_reconstruct_points(), gplates_reconstruct_coastlines(), gplates_reconstruct_static_polygons() was added for gplates reconstructions. 


## [0.0.1 (build 9)]  - 2019-07-21 
### Added
- the matchtime() function was added for matching series of maps with a predefined time scale.


## [0.0.1 (build 8)]  - 2019-07-20 
### Added
- unit testing framework established specific example for fetch()
- resample() and crop() methods added for RasterArray


## [0.0.1 (build 7)]  - 2019-07-19/2 
### Added
- first version registry
- 'paleoatlas' variable
- mapplot() function
- jpeg suggested package

### Changed
- remote server file structure
- earthhist_map dataset to "paleomap" 


## [0.0.1 (build 6)]  - 2019-07-19  
### Added
- first version of the fetch function
- layers() function to show the names of the individual RasterLayers. Does the same as names(<RasterStack>). 

### Changed
- names() method of the RasterArray outputs index names


## [0.0.1 (build 4)]  - 2019-07-15  
### Added
- Change log (/inst/NEWS)


=======
# Change log of the R package 'earthhist'

## [0.0.1 (build 11)]  - 2019-07-23 
### Added
- the dataindex() function to fetch data registry from the remote server
- resolution variable in the remote server archives and layer file names
- additional variables to the registry table
- the prec and temp variables are added to the remote server
- raster package is now a dependency rather than import

### Changed
- paleoatlas dataset resolution increased to original 0.1


## [0.0.1 (build 9)]  - 2019-07-21 
### Added
- the matchtime() function was added for matching series of maps with a predefined time scale.


## [0.0.1 (build 8)]  - 2019-07-20 
### Added
- unit testing framework established specific example for fetch()
- resample() and crop() methods added for RasterArray


## [0.0.1 (build 7)]  - 2019-07-19/2 
### Added
- first version registry
- 'paleoatlas' variable
- mapplot() function
- jpeg suggested package

### Changed
- remote server file structure
- earthhist_map dataset to "paleomap" 


## [0.0.1 (build 6)]  - 2019-07-19  
### Added
- first version of the fetch function
- layers() function to show the names of the individual RasterLayers. Does the same as names(<RasterStack>). 

### Changed
- names() method of the RasterArray outputs index names


## [0.0.1 (build 4)]  - 2019-07-15  
### Added
- Change log (/inst/NEWS)

