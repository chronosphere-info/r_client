
# chronosphere <img src="man/figures/logo.png" align="right" />

[![](https://img.shields.io/badge/devel%20version-0.5.0-green.svg)](https://github.com/chronosphere/r_client)
[![](https://www.r-pkg.org/badges/version/chronosphere?color=blue)](https://cran.r-project.org/package=chronosphere)
[![](http://cranlogs.r-pkg.org/badges/grand-total/chronosphere?color=yellow)](https://cran.r-project.org/package=chronosphere)
[![CRAN
checks](https://badges.cranchecks.info/summary/chronosphere.svg)](https://cran.r-project.org/web/checks/check_results_chronosphere.html)

### Evolving Earth System Variables for <a href="https://www.r-project.org/"><img style="position: relative; top: -3px;" src="man/figures/R_logo.png" width="40px"></a>

<br>

## Client package

-----

The implemented functions allow the query, download, and import of
remotely-stored and version-controlled data items. The inherent
meta-database maps data files and import code to programming classes and
allows access to these items via files deposited in public repositories.
The purpose of the project is to increase reproducibility and establish
version tracking of results from (paleo)environmental/ecological
research.

## Example

-----

#### Distribution data

``` r
library(chronosphere)

# see available datasets
available <- datasets()

# download some data
# NaturalEarth land polygons
ne <- chronosphere::fetch("NaturalEarth", var="land", ver="4.1.0")

# The entire Paleobiology Database
pbdb <- chronosphere::fetch("pbdb", ver="20230621")

# collections
colls <- unique(pbdb[, c("collection_no","lng", "lat")])

# plot records
plot(ne$geometry, col="gray", border=NA)
points(colls$lng, colls$lat, cex=0.3, pch=16, col="#54121212")
```

![](man/figures/chronosphere_example.png)

<br>

## Notes

-----

#### History

The original chronosphere was a true **monolith** that included lots of
additional functionality to manipulate paleoenvironmental data. For
better compliance with [UNIX
philosophy](https://en.wikipedia.org/wiki/Unix_philosophy) and more
efficient distribution/development, the original package has been broken
up to three R packages:

  - `chronosphere`: version-controlled data distribution.
  - [`rgplates`](https://adamkocsis.github.io/rgplates/): functions
    related to tectonic reconstructions.
  - [`via`](https://adamkocsis.github.io/via/): Virtual Arrays for
    efficient organisation of high-dimensional data.

This is a beta version, and like R, comes with absolutely no warranty.
