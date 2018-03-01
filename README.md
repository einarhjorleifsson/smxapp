Install stuff:

    devtools::install_github("einarhjorleifsson/smxapp",
                             dependencies = FALSE)
    devtools::install_github("fishvice/xe", 
                             dependencies = FALSE, 
                             build_vignettes = TRUE)

Then:

    library(tidyverse)
    library(lubridate)
    library(sp)
    library(ROracle)
    library(xe)
    library(smxapp)
    con <- connect_xe()
    Cruise <- c("A4-2018", "TL1-2018") # or whatever smb cruise you have in Hafvog
    import_smx(con, cruise = Cruise)

Once done go:

-   File -&gt; New file -&gt; Rmarkdown... -&gt; From Template --&gt;
    smx dashboard
-   Press Run Document
