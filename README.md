# chronosphere [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3530703.svg)](https://doi.org/10.5281/zenodo.3530703)
Earth System History Variables

(Ádám T. Kocsis and Nussaïbah B. Raja)

The purpose of the 'chronosphere' package is to facilitate spatially explicit analyses of deep time paleoenvironmental/paleoecological research. 

The project is developed under the umbrella of the DFG Research Unit TERSANE2 (For 2332) in collaboration with Christopher Scotese and Paul Valdes. The package will serve as a gateway to deep time global climate model results and plate tectonic/paleonvironmental reconstructions. It also implements query functions to the GPlates Web Service allowing users to reconstruct coordinates without leaving the R environment. 

This is a beta version. Much of the functionality is not yet available/perfect and data access is restricted to publicly available datasets only. 

See the blog entry below for an example application:
https://www.evolv-ed.net/post/chronosphere-paleomap/chronosphere-paleomap/

# Installing

## Install method A.

Open R and paste in: 
```r
install.packages("https://github.com/adamkocsis/chronosphere/raw/master/_archive/source/chronosphere_0.1.12-50.tar.gz", repos=NULL, type="source")
```

## Install method B. 

Alternatively, you can install the package with the 'devtools' package directly from GitHub:
```r
devtools::install_github("adamkocsis/chronosphere")
```

## After install
You should be able to attach it with the regular library function:
```r
library(chronosphere)
```

# Usage

Run dataindex() to see the publicly available variables. 

# Cite as
Kocsis, Ádám T. & Raja, Nussaïbah B. (2019). chronosphere: Earth system history variables (pre-release) (Version 0.1.5). Zenodo. http://doi.org/10.5281/zenodo.3525482

