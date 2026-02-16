#!/bin/R 
# Title: Results formating
# Author: F.Jean
# Creation Date: 2025-07-20
# Revision Dates: 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# FOREWORD
#~~~~~~~~~


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# BUGS REPORT
#~~~~~~~~~~~~


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# CONTENTS ----------------------------------------------------------------------------------
#~~~~~~~~~

# 1. Settings
  # 1.1 Define language locales 
  # 1.2 Define working directory 
  # 1.3 Preparation of exportation
  # 1.4 Reproducibility for packages
  # 1.5 Install packages
  # 1.6 Load packages
  # 1.7 Load functions
  # 1.8 Prepare parallel computations
# 2. Data importation
# 3. Unique Values
# 4. Tables
  # 4.1 Table 1: Description full  
  # 4.1.1 Table 1 DF
  # 4.1.2 Table 1 FLEXTABLE  
  # 4.1.3 Table 1 SAVE 

# Z. Exportations
  # Z.1 Save objects

# Cleaning and Reset Settings

#~~~~~~~~~~~~~~~~
# END OF CONTENTS 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# SECTION 1

# TABLE OF CONTENTS FOR SECTION

# 1. Settings   -----------------------------
  # 1.1 Define langage locales 
  # 1.2 Define working directory 
  # 1.3 Preparation of exportation
  # 1.4 Reproducibility for  packages
  # 1.5 Install packages
  # 1.6 Load packages
  # 1.7 Load functions
  # 1.8 Prepare parallel computations

# 1. Settings
#~~~~~~~~~~~~

# 1.1 Define langage locales 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# put locales in english
Sys.setlocale("LC_ALL", "en_US.utf8") ;

# 1.2 Define working directory 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getwd()

# 1.3 Preparation of exportation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create list to stock objects for exportation
NULL -> Objects.ToExport.list1 ;

# Creation of exportation Repository
paste( getwd(), 
       "/Files_For_Article", 
       sep = "" ) ->
  ExportFile1 ;

dir.create( ExportFile1, mode="0777" )

# 1.4 Reproducibility for  packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try( find.package( "renv" ), silent = TRUE ) -> test ;
if ( class( test ) == "try-error" )
  { 
    install.packages(  "renv" ) ;
  } 

# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = getOption("download.file.method")) ; options(repos="http://cran.rstudio.com/") ; Sys.setenv("RENV_CONFIG_REPOS_OVERRIDE" = "http://cran.rstudio.com")

#renv::init()

renv::status()
# renv::deactivate(clean = TRUE)

# 1.5 Install packages
#~~~~~~~~~~~~~~~~~~~~~

c( "openxlsx", "furrr", "ggplot2",
"xtable", "officer", "flextable"
) -> 
  PackagesNames1 ;

for (a in 1:length( PackagesNames1 ) )
{
  try( find.package( PackagesNames1[a]), silent = TRUE ) -> test ;
  
  if ( class( test ) == "try-error" )
  { print( PackagesNames1[a] ) ; 
    install.packages(  PackagesNames1[a] ) ;
  } 
}

rm( PackagesNames1 ) ;


# 1.6 Load packages
#~~~~~~~~~~~~~~~~~~

c( "ggplot2", "officer"
) -> 
  PackagesNames1 ;

for (a in 1:length( PackagesNames1 ) )
{
  require( PackagesNames1[a], character.only = TRUE  ) 
}

rm( PackagesNames1 ) ;


# 1.7 Load functions
#~~~~~~~~~~~~~~~~~~~

#source( "ScriptR0_StockOfFunctions_NotParalle.R" )

# 1.8 Prepare parallel computations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if ( future::availableCores() <= 3 )
{
  1 -> NbOfCores1 ;
} else
{
  future::availableCores() - 2 -> NbOfCores1 ;
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# SECTION 2

# TABLE OF CONTENTS FOR SECTION

# 2. Data importation   -----------------------------

# 2. Data importation 
#~~~~~~~~~~~~~~~~~~~~

c( "RESTCA2_ObjectsForLinkScript.RData" ) -> FilesToImport1 ;

load( FilesToImport1 )

# Cleaning
#~~~~~~~~~

rm( FilesToImport1 ) ;

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# SECTION 3

# TABLE OF CONTENTS FOR SECTION

# 3. Unique Values   -----------------------------

# 3. Unique Values
#~~~~~~~~~~~~~~~~~

# number of subjects analyzed
#val1.nb.subjects <- 

# Stock results
#~~~~~~~~~~~~~~

#Objects.ToExport.list1 <- c( Objects.ToExport.list1, 
#                       list( valX. =  valX.  ) 
#                       ) ;

# Cleaning
#~~~~~~~~~

rm( ) ;

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# 4. Tables   -----------------------------
# 4.1 Table 1: Description full  
# 4.1.1 Table 1 DF
# 4.1.2 Table 1 FLEXTABLE  
# 4.1.3 Table 1 SAVE 

# 4. Tables
#~~~~~~~~~~

# Liste complète des fonctions de flextable
# https://davidgohel.github.io/flextable/reference/index.html 

# 4.1 Table 1:      --------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.1.1 Table 1 DF
#~~~~~~~~~~~~~~~~~

# prepare data
#~~~~~~~~~~~~~

results.univariate.ccTM ->
  df.tmp1 ;

colnames(df.tmp1) <- NULL ; 

rbind( rep( "", ncol(df.tmp1)),
       df.tmp1 ) ->
  df.tmp1 ; 

NULL -> rownames(df.tmp1) ;

paste0( "X", 1:ncol(df.tmp1) ) -> colnames(df.tmp1) ;

# prepare the matrix
#~~~~~~~~~~~~~~~~~~~

rep( "",  12 * 2 ) |>
  matrix(  ncol = 2 ) ->
  tab.tmp1 ;

# prepare fisrt line which will be colnames for programmation reason
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c( " ", 
   "% (n) or mean (sd) "
) ->
  colnames( tab.tmp1 ) ;

# prepare first colomns which will be rownames for programmation reason
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c( 
  "General:",
  "Sex: female",
  "Age (years)",
  "Adolescents: yes",
  "Weight (kg)",
  "Body Mass Index (BMI) (kg/m²)",
  "Eating disorder:",
  "Anorexia nervosa",
  "Boulimia nervosa",
  "Eating disorder not otherwise specified",
  "EAT-26 total score",
  "History of hospitalization for eating disorder"
) -> 
  tab.tmp1[,1] ;

# Fill the tab
#~~~~~~~~~~~~~


# TOTAL
c( 
  which( df.tmp1[,3] == "Genre") + 1,
  which( df.tmp1[,3] == "Age"),
  which( df.tmp1[,3] == "En_couple") +2,
  which( df.tmp1[,3] == "Enfants") +2,
  which( df.tmp1[,3] == "Vit_seul") +2,
  which( df.tmp1[,3] == "Enfants_au_domicile") +2,
  which( df.tmp1[,3] == "Conflit_conjugal") +2,
  which( df.tmp1[,3] == "Rupture_amoureuse") +2,
  which( df.tmp1[,3] == "Violences_conjugales_physiques") +2,
  which( df.tmp1[,3] == "Violences_conjugales_verbales") +2,
  which( df.tmp1[,3] == "Difficultes_avec_enfants") +2,
  which( df.tmp1[,3] == "Difficultes_avec_parents") +2,
  which( df.tmp1[,3] == "Difficultes_avec_autres_famille") +2,
  which( df.tmp1[,3] == "Deuil") +2,
  which( df.tmp1[,3] == "Difficulte_professionnelle") +2,
  which( df.tmp1[,3] == "Difficulte_financiere") +2
) ->
  line1 ;

df.tmp1 |>
  subset(  
    select = c( "X6", "X10" )
  ) |> 
  (function(x) x[line1,])() |> 
  transform( 
    v1= X6, v2 = " (", v3 = X10, v4 = ")" 
  ) |>
  tidyr::unite( V1, v1, v2, v3, v4, sep ="" ) |>
  subset( select = V1 ) |>
  apply( 
    2, 
    FUN = function(a)
    {  a[a == " ()"]  <- "" ; a } 
  ) ->
  tab.tmp1[,2] ; 

# stock a matrix version for Rmd extraction
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# restore first line
tab.tmp1 -> 
  tab.tmp.mat1 ;

c(
  " ",
  "Total sample",
  "No cannabis use",
  "Cannabis use ≤ 1 times/month",
  "Cannabis use ≤ 1 times/week",
  "Cannabis use > 1 times/week ",
  "   "
) -> 
  colnames( tab.tmp.mat1 ) ;

# 4.1.2 Table 1 FLEXTABLE         ------------------
#~~~~~~~~~~~~~~~~~~~~~~~~

# import matrix
tab.tmp1 |>
  xtable::xtable() |>
  flextable::as_flextable( include.rownames = FALSE 
  ) -> 
  tab1 ;

# HEADER 
#~~~~~~~

# add header
tab1$col_keys

tab1 |>
  flextable::add_header( 
    "X." = " ",
    "X...n..or.mean..sd.."  = paste0( "Total sample (n = ", 
                                      tmp.df[3,5], ")" ),
    "X...n..or.mean..sd..." = paste0( "No cannabis use (n = ", 
                                      tmp.df[3,19], ")" ),
    "X....n..or.mean..sd.." = paste0( "Cannabis use ≤ 1 times/month (n = ", 
                                      tmp.df[3,27], ")" ),
    "X.....n..or.mean..sd..." =  paste0( "Cannabis use ≤ 1 times/week (n = ", 
                                         tmp.df[3,35], ")" ),
    "X....n..or.mean..sd..."  = paste0( "Cannabis use > 1 times/week (n = ", 
                                        tmp.df[3,43], ")" ),
    "p" = ""
  ) -> 
  tab1 ;

# fix title

tab1 |>
  flextable::merge_h( 
    part = "header"
  ) -> 
  tab1 ;

# fix border of header
tab1 |>
  flextable::border(  
    i = 1, 
    part = "header", 
    border.bottom = officer::fp_border( color = "white" ) 
  ) -> 
  tab1 ;

tab1 |>
  flextable::border( 
    i = 1, 
    j = 3:6,
    part = "header", 
    border.bottom = officer::fp_border( width = 1 ) 
  ) -> 
  tab1 ;

# FOOTER
#~~~~~~~

# add footer
tab1$col_keys

"COVID19: Corona virus disease 2019; N: group size; %: percentage; n: count; m: mean; sd: standard deviation; Common N*: Group size of available cases for the test, p: p-value; \n p-values from Analysis of Deviance with cluster robust and heteroscedasticity robust standard error" ->
  footnote1

tab1 |>
  flextable::add_footer( 
    "X." = footnote1,
    "X...n..or.mean..sd.."  = footnote1,
    "X...n..or.mean..sd..." = footnote1,
    "X....n..or.mean..sd.." = footnote1,
    "X.....n..or.mean..sd..." =  footnote1,
    "X....n..or.mean..sd..."  = footnote1,
    "p" = footnote1
  ) ->
  tab1 ;

rm( footnote1 )

# fix footer
tab1 |>
  flextable::merge_h( 
    part = "footer"
  ) -> 
  tab1 ;

# BODY 
#~~~~~

# bold and italic body
tab1 |>
  flextable::italic( 
    i = c(1,8,13),
    italic = T, 
    part = "body"
  ) ->
  tab1 ;

tab1 |>
  flextable::bold( 
    i = c(1,8,13),
    bold = T, 
    part = "body"
  ) ->
  tab1 ;

# FORMATING 
#~~~~~~~~~~

# font size  
tab1 |>
  flextable::fontsize( 
    part = "header", 
    size = 8
  ) ->
  tab1 ;

tab1 |>
  flextable::fontsize( 
    part = "body", 
    size = 8
  ) ->
  tab1 ;

tab1 |>
  flextable::fontsize( 
    part = "footer", 
    size = 6
  ) ->
  tab1 ;


# justification 
tab1 |>
  flextable::align( 
    align = "center", 
    part = "all" 
  ) ->
  tab1 ;

tab1 |>
  flextable::align( 
    j = 1,
    align = "left", 
    part = "all" 
  ) ->
  tab1 ;

# width, heights 
tab1 |>
  flextable::width(  
    j = 1, 
    width = 1.3 
  ) ->
  tab1 ;

tab1 |>
  flextable::width( 
    j = 2:7, width = 0.8 ) ->
  tab1 ;

# Stock with proper name
tab.tmp.mat1 -> tab1.mat ;

# 4.1.3 Table 1 SAVE        ------------------
#~~~~~~~~~~~~~~~~~~~
officer::read_docx() -> docx ;

"Table 1: Characteristics of the full sample and according to stimulants use at inclusion" ->
  title1 ;

docx |>
  officer::body_add_par( 
    title1, 
    style = "table title" 
  ) |>
  officer::body_add_par( 
    " ", 
    style = "centered"
  ) -> 
  docx ;

docx |>
  flextable::body_add_flextable( 
    tab1, 
    align = "left" ) ->
  docx ;

#docx |> body_end_section_landscape() -> docx ;

docx |>
  print( 
    target = paste0( ExportFile1, 
                     "/",
                     "Table1.docx" 
    ) 
  ) ;

tab1 |> 
  flextable::save_as_image( 
    path =  paste0( ExportFile1, 
                    "/",
                    "Table1.png" 
    ),
    zoom = 2 )

rm( docx, title1 ) ;

# clean
rm( docx, title1, df.tmp1, df.tmp2, df.tmp3, line1, tab.tmp1, tab.tmp.mat1 ) ;

# Stock results ----------------------------------
#~~~~~~~~~~~~~~

Objects.ToExport.list1 <- c( Objects.ToExport.list1, 
                             list(  tab1 = tab1  ) 
) ;

# Cleaning
#~~~~~~~~~

rm( ) ;


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# Z. Exportations   -----------------------------
  # Z.1 Save objects

# Z. Exportations
#~~~~~~~~~~~~~~~~

# Z.1 Save objects
#~~~~~~~~~~~~~~~~~

save( list = names(Objects.ToExport.list1) , 
      file = "ObjectsForQmd.RData" )  ;

##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# CLEANNING AND RESET SETTINGS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#graphics.off() ;

#rm( list = ls() ) ;

#.rs.restartR() ;

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# END