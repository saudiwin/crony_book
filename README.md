# README

This Github repository contains R code and survey data to reproduce
results in the book [Making Democracy Safe for Business: Corporate
Politics During the Arab
Uprisings](https://www.cambridge.org/gb/universitypress/subjects/politics-international-relations/political-economy/making-democracy-safe-business-corporate-politics-during-arab-uprisings?format=HB)
by [Robert Kubinec](https://www.robertkubinec.com) (Cambridge, 2023). If
you are not familiar with how to download (or “clone”) the files in this
repository, I encourage you to consider using the free [Github Desktop
software](https://desktop.github.com/) to enable you to do so.

The repository contains one R script for each empirical chapter in the
book in the `rscripts` folder. These scripts were generated from the
underlying Rmarkdown files used to create the book. However, because the
text of the book cannot be released due to copyright issues, the code is
included without the text. The `data` folder contains necessary survey
and ancillary data for the code to run. All survey responses have been
anonymized by removing identifiers and any variables that could indicate
the location of respondents. All reproduced figures from the book are
saved in the `figures` folder.

This data and code is released under the MIT license (see included file
`LICENSE`).

## Requirements to run the code

This repository uses the R package
[`packrat`](https://rstudio.github.io/packrat/walkthrough.html) to
manage package dependencies. All of the packages used to run the code
are included in the repo as source files with the versions used when the
code was last run by the author. When R is started in the root directory
(or via a project in Rstudio), `packrat` will set up a package library
in the root directory and install any necessary packages from included
source package files, which includes the ones listed below and all of
their dependencies. *If you are having trouble installing source
packages on Mac OS X, try running R from the terminal in the project
folder (see Github issue here:
<https://github.com/rstudio/rstudio/issues/10883>)*.

Specifically, the following package libraries need to be installed to
run the code:

    tidyverse
    ggplot2
    Hmisc
    ggthemes
    qualtRics
    lubridate
    stringr
    googlesheets4
    haven
    forcats
    cjoint
    binom
    readr
    brms
    kableExtra
    boot
    mirt
    WDI
    remotes
    vdemdata
    knitr
    rmarkdown

If `packrat` is not available or you do not wish to you use it, simply
install the packages above from CRAN. Note that package `vdemdata` is
not available on CRAN but can be installed from the Github repo site
with the following code:

    remotes::install_github("vdeminstitute/vdemdata")

The session info of the machine last used to run the code is as follows:

    sessionInfo()

    ## R version 4.2.2 (2022-10-31)
    ## Platform: aarch64-apple-darwin20 (64-bit)
    ## Running under: macOS Monterey 12.6.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats4    grid      stats     graphics  grDevices utils     datasets 
    ## [8] methods   base     
    ## 
    ## other attached packages:
    ##  [1] knitr_1.43          vdemdata_13.0       WDI_2.7.8          
    ##  [4] mirt_1.39           lattice_0.20-45     boot_1.3-28.1      
    ##  [7] kableExtra_1.3.4    brms_2.19.0         Rcpp_1.0.10        
    ## [10] binom_1.1-1.1       cjoint_2.1.0        survey_4.1-1       
    ## [13] survival_3.4-0      Matrix_1.5-1        lmtest_0.9-40      
    ## [16] zoo_1.8-12          sandwich_3.0-2      haven_2.5.3        
    ## [19] googlesheets4_1.1.1 qualtRics_3.1.7     ggthemes_4.2.4     
    ## [22] lubridate_1.9.2     forcats_1.0.0       stringr_1.5.0      
    ## [25] dplyr_1.1.0         purrr_1.0.1         readr_2.1.4        
    ## [28] tidyr_1.3.0         tibble_3.2.0        ggplot2_3.4.2      
    ## [31] tidyverse_2.0.0    
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] backports_1.4.1      systemfonts_1.0.4    plyr_1.8.8          
    ##   [4] igraph_1.4.1         GPArotation_2023.3-1 splines_4.2.2       
    ##   [7] crosstalk_1.2.0      rstantools_2.3.0     inline_0.3.19       
    ##  [10] digest_0.6.31        htmltools_0.5.4      fansi_1.0.4         
    ##  [13] magrittr_2.0.3       checkmate_2.1.0      cluster_2.1.4       
    ##  [16] tzdb_0.3.0           RcppParallel_5.1.7   matrixStats_0.63.0  
    ##  [19] xts_0.13.0           svglite_2.1.1        timechange_0.2.0    
    ##  [22] prettyunits_1.1.1    colorspace_2.1-0     rvest_1.0.3         
    ##  [25] mitools_2.4          xfun_0.39            callr_3.7.3         
    ##  [28] crayon_1.5.2         jsonlite_1.8.4       glue_1.6.2          
    ##  [31] gtable_0.3.1         gargle_1.5.1         webshot_0.5.4       
    ##  [34] distributional_0.3.1 pkgbuild_1.4.0       rstan_2.21.8        
    ##  [37] dcurver_0.9.2        abind_1.4-5          scales_1.2.1        
    ##  [40] mvtnorm_1.1-3        DBI_1.1.3            miniUI_0.1.1.1      
    ##  [43] viridisLite_0.4.1    xtable_1.8-4         StanHeaders_2.21.0-7
    ##  [46] DT_0.27              htmlwidgets_1.6.1    httr_1.4.5          
    ##  [49] threejs_0.3.3        posterior_1.4.0      ellipsis_0.3.2      
    ##  [52] pkgconfig_2.0.3      loo_2.5.1            farver_2.1.1        
    ##  [55] utf8_1.2.3           tidyselect_1.2.0     rlang_1.1.1         
    ##  [58] reshape2_1.4.4       later_1.3.0          munsell_0.5.0       
    ##  [61] cellranger_1.1.0     tools_4.2.2          cli_3.6.0           
    ##  [64] generics_0.1.3       sjlabelled_1.2.0     evaluate_0.20       
    ##  [67] fastmap_1.1.1        yaml_2.3.7           processx_3.8.0      
    ##  [70] fs_1.6.1             packrat_0.9.1        pbapply_1.7-0       
    ##  [73] nlme_3.1-160         mime_0.12            xml2_1.3.3          
    ##  [76] compiler_4.2.2       bayesplot_1.10.0     shinythemes_1.2.0   
    ##  [79] rstudioapi_0.14      stringi_1.7.12       ps_1.7.2            
    ##  [82] Brobdingnag_1.2-9    markdown_1.5         permute_0.9-7       
    ##  [85] vegan_2.6-4          shinyjs_2.1.0        tensorA_0.36.2      
    ##  [88] vctrs_0.5.2          pillar_1.9.0         lifecycle_1.0.3     
    ##  [91] bridgesampling_1.1-2 insight_0.19.1       httpuv_1.6.9        
    ##  [94] R6_2.5.1             promises_1.2.0.1     gridExtra_2.3       
    ##  [97] codetools_0.2-18     MASS_7.3-58.1        colourpicker_1.2.0  
    ## [100] gtools_3.9.4         withr_2.5.0          Deriv_4.1.3         
    ## [103] shinystan_2.6.0      mgcv_1.8-41          parallel_4.2.2      
    ## [106] hms_1.1.2            coda_0.19-4          rmarkdown_2.23      
    ## [109] googledrive_2.1.1    shiny_1.7.4          base64enc_0.1-3     
    ## [112] dygraphs_1.1.1.6

I would recommend using R version 4.2 or greater to run the code.

## Running the code

The R script `master_script.R` in the root directory will run each
script in the `rscripts` folder and recompile the `README` file with the
latest session info. Note that the code reproduces some figures from
image files rather than from raw data. These are usually descriptive
graphics and if there is any question about these files, please email
the author at <rmk7@nyu.edu>.

The two scripts in `rscripts` with statistical models (`06-` and `07-`)
have an option `run_code` that is set by default to `TRUE` because the
first time the code is run it will save fitted model objects for
Bayesian regression models in `data`. Setting this option to `FALSE` in
the script after running it will save significant time at reproducing
the figures. All reproduced figures are saved by the code in the
`figures` folder.

The individual R scripts are as follows:

1.  `04a-Case-Study-Egypt.R` : code for Chapter 2: The Egyptian Military
    as the Gatekeeper
2.  `05a-Case-Study-Tunisia.R` : code for Chapter 3: Broad Rent-seeking
    and the Collapse of Tunisia’s Anti-Democratic Coalition
3.  `06-Quantitative-Surveys.R` : code for Chapter 4: Experiments in
    Business and Political Connections
4.  `07-Quantitative-Surveys-Other.R` : code for Chapter 5: Crony
    Capitalism in International Comparison

Important data files are as follows:

1.  `qual_data_new.rds`: original survey data for 2017 surveys of
    business employees in Egypt, Algeria and Tunisia
2.  `egypt_mil_survey.csv`: 2018 survey of Egyptian military officers
    and enlisted personnel and Tunisian businesspeople
3.  `all_imp_jn_m.rds` : imputed datasets for the 2018 Jordan and
    Morocco survey
4.  `all_imp_eg_vn.rds` : imputed datasets for the 2020 Egypt, Ukraine
    and Venezuela surveys
5.  `all_imp_eg_tn.rds` : imputed datasets for the 2017 Egypt and
    Tunisia survey

A brief list of questions in the survey and their meaning:

1.  `rank_eg` The rank of a military officer
2.  `Q14` / `registration` type of firm
3.  `Q74` / `ceo` whether the respondent is a CEO of the firm
4.  `Q13` / `sector_1` sector of the firm
5.  `Q38` / `bribe_increase` whether bribes paid by the firm have
    increased since the Arab Spring
6.  `Q30_2` / `supply_2` rank of military-owned companies as a supplier
    to the firm (1 = highest)
7.  `Q28_2` / `cust_2` rank of military-owned companies as a customer of
    the firm (1 = highest)
8.  `Q8` / `firm_size` Number of firm employees
9.  `Q8_1` / `conglomerate` Whether or not the company is a part of a
    conglomerate
10. `Q37` / `bribe_income` How much does the company pay in bribes as a
    percentage of its income?
11. `Q9` / `position` What is the status of the respondent (i.e. are
    they an employee or manager in the company?)
12. `ResponseId` ID of the respondent in the survey (anonymized)
13. `Q52_1` / `firm_pol` Did the firm contribute funds to a candidate in
    the elections?
14. `Q52_2` / `firm_pol` Did the firm distribute campaign literature to
    employees?
15. `Q52_3` / `firm_pol` Did the firm instruct employees to vote for a
    specific candidate?
16. `Q52_4` / `firm_pol` Did the firm host party rallies?
17. `Q33_1` / `inspect_1_1` How many times was the company inspected by
    government regulators in the past year?

Note that there is substantial additional data available in the surveys,
but this has not been released to protect the anonymity of respondents.
It is possible to share this additional data for research purposes. If
you are interested in additional data, please contact the author of the
survey at <rmk7@nyu.edu>.
