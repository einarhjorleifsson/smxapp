Install stuff:

    devtools::install_github("einarhjorleifsson/smxapp",
                             dependencies = FALSE,
                             build_vignettes = TRUE)
    devtools::install_github("fishvice/xe", 
                             dependencies = FALSE, 
                             build_vignettes = TRUE)

Some minimum information on the xe-package you can get via:

    browseVignettes(package = "xe")

And for the smxapp-package via (actually contains the same stuff as this
README):

    browseVignettes(package = "smxapp")

You can obtain some minimum files to load into Hafvog at
<ftp://ftp.hafro.is/pub/reiknid/einar/hafvog>

For the smx app you could do:

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
