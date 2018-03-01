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
    res <- import_smx(con)
    export_for_smxapp(con, res, cruise = c("A4-2018", "TL1-2018", "TH1-2018", "B3-2018"))

Once done go:

-   File -&gt; New file -&gt; Rmarkdown... -&gt; From Template --&gt;
    smx dashboard
-   Press Run Document, save it in the root directory of the project

You may get some error messages along the way - most likely associated
with not having some packages installed.
