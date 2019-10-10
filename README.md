# chronosphere
Earth System History Variables

(Ádám T. Kocsis and Nussaibah Raja-Schoob)

The purpose of the 'chronospere' package is to facilitate spatially explicit analyses of deep time paleoenvironmental/paleoecological research. 

The project is developed under the umbrella of the DFG Research Unit TERSANE2 (For 2332) in collaboration with Christopher Scotese and Paul Valdes. The package will serve as a gateway to deep-time global climate model results and plate tectonic/paleonvironmental reconstructions. It also implements query functions to the GPlates Web Service allowing users to reconstruct coordinates without leaving the R environment. 

This is a beta version. Much of the functionality is not yet available and data acces is restricted to publicly available datasets. 

# Installing

## Install method A.

Open R and type: 
```r
install.packages("devtools")
```

Then you can install the package with:
```r
devtools::install_github("adamkocsis/chronosphere")
```
## Install method B. 

Download the latest package archive with:

https://github.com/adamkocsis/chronosphere/raw/master/_archive/source/chronosphere_0.1.0-23.tar.gz

Save it somewhere, and then run 
```r
install.packages("<yourpath>chronosphere_0.1.0-23.tar.gz", repos=NULL)
```

## After install
You should be able to attach it with the regular library function:
```r
library(chronosphere)
```

# Usage

Run dataindex() to see the publicly available variables. 

