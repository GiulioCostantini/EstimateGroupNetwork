## Test environments
* local Windows 10 install, R 4.0.3
* Windows Server x86_64-w64-mingw32 (64-bit)
* Linux server (R 4.0.3) x86_64-redhat-linux-gnu (64-bit)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTES.
  
## Downstream dependencies
There are no downstream dependencies for this package

## CRAN submission
We get an error on  r-release-windows-ix86+x86_64 regarding the absence of package Hmisc. Since EstimateGroupNetwork does not depend or suggest Hmisc and since the tests work on all other systems, we ignored the error and submitted to CRAN.