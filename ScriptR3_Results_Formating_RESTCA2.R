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

c( "ObjectsForFormating.RData" ) -> FilesToImport1 ;

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
val1.nb.subjects <- 

# Stock results
#~~~~~~~~~~~~~~

Objects.ToExport.list1 <- c( Objects.ToExport.list1, 
                       list( valX. =  valX.  ) 
                       ) ;

# Cleaning
#~~~~~~~~~

rm( ) ;


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