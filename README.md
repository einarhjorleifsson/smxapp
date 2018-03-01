---
title: "smxapp"
output: 
  html_document: 
    keep_md: yes
---



Install stuff:


```r
devtools::install_github("einarhjorleifsson/smxapp",
                         dependencies = FALSE)
devtools::install_github("fishvice/xe", 
                         dependencies = FALSE,
                         args='--no-multiarch')
```

Then:

```r
library(tidyverse)
library(lubridate)
library(sp)
library(ROracle)
library(xe)
library(smxapp)
con <- connect_xe()
Cruise <- c("A4-2018", "TL1-2018") # or whatever smb cruise you have in Hafvog
import_smx(con, cruise = Cruise)
```

Once done go:

* File -> New file -> Rmarkdown... -> From Template --> smx dashboard
* Press Run Document

