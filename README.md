# chronosphere
Earth System History Variables

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

