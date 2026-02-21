#!/bin/R 
# Title: 
# Author: 
# Version:

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# CONTENTS ----------------------------------------------------------------------------------
#~~~~~~~~~
# a/ Foreword
# b/ Bugs Report
#>>>>>>>>
# 1. Settings
  # 1.1 Record start time
  # 1.2 Define language locales 
  # 1.3 Define working directory 
  # 1.4 Preparation of exportation
  # 1.5 Reproducibility for packages
  # 1.6 Install packages
  # 1.7 Load packages
  # 1.8 Load functions
  # 1.9 Prepare parallel computations
# 2. Data importation
  # 2.1 Define files to import 
  # 2.2 Files loading
  # 2.3 Quality ckecking 
  # 2.4 Subseting 
# 3. LLM
 
# Z. Exportations
  # Z.1 Exporting references
  # Z.2 Exporting results
  # Z.3 Exporting objects 
# Print runing time
# Cleaning and Reset Configuration

#~~~~~~~~~~~~~~~~
# END OF CONTENTS 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# FOREWORD
#~~~~~~~~~


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# BUGS REPORT
#~~~~~~~~~~~~

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# TABLE OF CONTENTS FOR SECTION

# 1. Settings   -----------------------------
  # 1.1 Record start time
  # 1.2 Define langage locales 
  # 1.3 Define working directory 
  # 1.4 Preparation of exportation
  # 1.5 Reproducibility for packages
  # 1.6 Install packages
  # 1.7 Load packages
  # 1.8 Load functions
  # 1.9 Prepare parallel computations

# 1. Settings
#~~~~~~~~~~~~
# 1.1 Record start time
#~~~~~~~~~~~~~~~~~~~~~~

Sys.time() -> StartTime ;

# 1.2 Define langage locales 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# put locales in english
Sys.setlocale("LC_ALL", "en_US.utf8") ;

# 1.3 Define working directory 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getwd( ) ;

# 1.4 Preparation of exportation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create list to stock objects for exportation
NULL -> Objects.ToExport.list1 ;

# Create list to stock objects for XLSX exportation
NULL -> Objects.ToExport.XLSX.list1 ;

# Creation of exportation Repository
paste( "~/Bureau/Results/Export_RESTCA1d_", 
       format( Sys.time(), "%Y%m%d%Hh%M" ), 
       sep = "" ) -> 
  ExportFile1 ;

dir.create( ExportFile1, mode="0777" ) ;

# 1.5 Reproducibility for  packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.6 Install packages
#~~~~~~~~~~~~~~~~~~~~~

c( "openxlsx", "ggplot2", "furrr", "tictoc"
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


# 1.7 Load packages
#~~~~~~~~~~~~~~~~~~

c( "ggplot2"
) -> 
  PackagesNames1 ;

for (a in 1:length( PackagesNames1 ) )
{
  require( PackagesNames1[a], character.only = TRUE  ) 
}

rm( PackagesNames1 ) ;


# 1.8 Load functions
#~~~~~~~~~~~~~~~~~~~


# 1.9 Prepare parallel computations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if ( future::availableCores() <= 3 )
{
  1 -> NbOfCores1 ;
} else
{
  future::availableCores() - 4 -> NbOfCores1 ;
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# TABLE OF CONTENTS FOR SECTION

# 2. Data importation   -----------------------------
  # 2.1 Define files to import 
  # 2.2 Files loading
  # 2.3 Quality ckecking 
  # 2.4 Subseting 

# 2. Data importation
#~~~~~~~~~~~~~~~~~~~~
# 2.1 Define files for importation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c( "~/Bureau/Results/RESTCA_clean_data.RData" ) -> FilesToImport1 ;

# 2.2 Files loading
#~~~~~~~~~~~~~~~~~~

base::load( FilesToImport1 )

# Cleaning
#~~~~~~~~~
rm( FilesToImport1 ) ;

# 2.3 Quality ckecking 
#~~~~~~~~~~~~~~~~~~~~~


#df1.cc |>
#  DataQuality.Analysis() |>
#  View() ;

#df1.im |>
#  DataQuality.Analysis() |>
#  View() ;

# 2.4 Subseting
#~~~~~~~~~~~~~~

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# TABLE OF CONTENTS FOR SECTION

# 3. LMM  -----------------------------

# 3. LLM
#~~~~~~~


# test du bash pour llama

PROMPT="Explain Ubuntu in one sentence."
~/Programs/llama.cpp/build/bin/llama-cli -m ~/Programs/LLM/DeepSeek-R1-Distill-Qwen-1.5B-f32.gguf \
-p "$PROMPT" \
--threads 1 \
--temp 0.8 --seed 42 \
--no-conversation   --no-display-prompt

~/Programs/llama.cpp/build/bin/llama-cli -m ~/Programs/LLM/DeepSeek-R1-Distill-Qwen-1.5B-f32.gguf --interactive=false \
-p "Explain Ubuntu in one sentence." \
--threads 1 \
--temp 0.7 --seed 42 \
--no-conversation  --no-display-prompt --simple-io


writeLines( PROMPT, "~/Bureau/Results/prompt.txt" )

date
~/Programs/llama.cpp/build/bin/llama-cli --model ~/Programs/LLM/DeepSeek-R1-Distill-Qwen-1.5B-f32.gguf \
--file ~/Bureau/Results/prompt.txt \
--threads 1 \
--temp 0.5 --seed 42 \
--no-conversation  --no-display-prompt --simple-io > ~/Bureau/Results/output_llama.txt
date

# Prepare corpus
df1.av$obstacle_PEC_TCA |>
  na.omit() -> vect1 ;

paste( 1:length(vect1), vect1, sep = ": ") |>
  paste( collapse = "; " ) ->
  vect1 ;

paste0( "Analyse et synthètise les réponses à la question 'Quelque chose vous a-t-il empêché d’obtenir l’aide dont vous aviez besoin au début de votre prise en soin ?  (regard des autres, argent, peur de grossir, stigma, religion, autres… ). Détaillez.'. Les réponses à la question sont : ", vect1 ) ->
  vect1 ; vect1 ;

paste0( vect1, ". Prends en compte que chaque individu a sa réponse après un chiffre et un slash et deux points (exemple : '1/: réponse'). Prends en compte que cette question a été posé à des patients souffrant d'un trouble du comportement alimentaire lors d'une étude clinique. Donne une réponse en moins de 200 mots et plus de 150 mots en anglais. Ne demande pas d'informations complémentaires dans ta réponse. Ne fait pas de commentaires autres dans ta réponse." ) ->
  vect1 ; vect1

writeLines( vect1, "~/Bureau/Results/prompt.txt" )


# Fonction pour automatique

llama_prompt <- function( prompt,
                         model_path = "~/Programs/llama.cpp/build/bin/llama-cli",
                         model_file = "~/Programs/LLM/DeepSeek-R1-Distill-Qwen-1.5B-f32.gguf",
                         directory = getwd(),
                         temp = 0.5,
                         seed = 42,
                         number.of.threads = 1
                         ) {
  
  # version llama.cpp
  version1_file <- paste0( directory, "/version_llama.txt" )
  
  cmd.v <- sprintf(
    '%s --version > %s 2>&1',
    model_path,
    version1_file
  )

  system( cmd.v )
  
  # version GPT
  writeLines( model_file, paste0( directory, "/version_GPT.txt" ))
  
  # version R
  sessionInfo() |> capture.output() |>
    writeLines( paste0( directory, "/version_R.txt" ))
  
  # Créer fichier temporaire pour le prompt
  writeLines(prompt, paste0( directory, "/prompt.txt" ))
  
  prompt_file <- paste0( directory, "/prompt.txt" ) 
  
  # Fichier temporaire pour la sortie
  output_file <- paste0( directory, "/output_llama.txt" )
  
  # Construire et exécuter la commande 
  
  cmd <- sprintf(
    '%s --model %s --file %s --temp %s --seed %s --threads %s --no-conversation  --no-display-prompt --simple-io > %s',
    model_path,
    model_file,
    prompt_file,
    temp,
    seed,
    number.of.threads,
    output_file
  )
  
  print( cmd )
  
  system(cmd)
  
  # Lire la sortie
  result <- readLines(output_file, warn = FALSE)
  
  # Retourner la réponse comme chaîne
  return(paste(result, collapse = "\n"))
}



# Exemple d’utilisation

PROMPT="Explain Ubuntu in one sentence."
tictoc::tic()
llama_prompt( PROMPT,
                          model_path = "~/Programs/llama.cpp/build/bin/llama-cli",
                          model_file = "~/Programs/LLM/DeepSeek-R1-Distill-Qwen-1.5B-f32.gguf",
              directory = ExportFile1,
                          temp = 1,
                          seed = 42) -> output ; output ;
tictoc::toc()

# Analyse
tictoc::tic()
llama_prompt( vect1,
              model_path = "~/Programs/llama.cpp/build/bin/llama-cli",
              model_file = "~/Programs/LLM/nb-notram-llama-3.2-3b-instruct-q8_0.gguf",
              directory = ExportFile1,
              temp = 0.5,
              seed = 42,
              number.of.threads = 1 ) -> output ; output ;
tictoc::toc()

# Stock results --------------
#~~~~~~~~~~~~~~
Objects.ToExport.list1 <- c( Objects.ToExport.list1, 
                            list(
                              output = output
                             ) 
) ;

Objects.ToExport.XLSX.list1 <- c( Objects.ToExport.XLSX.list1, 
                             list(
                               output = output
                                ) 
) ;

# Cleaning
#~~~~~~~~~
rm( GraphFile1, ) ;

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# TABLE OF CONTENTS FOR SECTION

# Z. Exportations   -----------------------------
  # Z.1 Exporting references
  # Z.2 Exporting results
  # Z.3 Exporting objects 


# Z. Exportations
#~~~~~~~~~~~~~~~~
# Z.1 Exporting references
#~~~~~~~~~~~~~~~~~~~~~~~~~

# bib
knitr::write_bib( c( .packages() ), 
                  paste( ExportFile1, 
                         "References_Softwares.bib", 
                         sep = "/" 
                  ) 
) ;


sessionInfo()$otherPkgs -> OtherPkgs.list ;

References_articles <- NULL ;

correctBib <- function(x, name) { gsub( ",", paste0( name, "," ), x[1] ) -> x[1] ; x }

for (a in 1:length( OtherPkgs.list ) )
{
  names(OtherPkgs.list)[a] |> 
    citation() |>
    toBibtex() |>
    correctBib(  
      names(OtherPkgs.list)[a]
    ) |>
    c( References_articles
    ) ->
    References_articles ;
} 

sessionInfo()  -> a ;
names( a$loadedOnly ) -> loadedOnlyPkgs.list ;

for (a in 1:length( loadedOnlyPkgs.list ) )
{
  loadedOnlyPkgs.list[a] |> 
    citation() |>
    toBibtex() |>
    correctBib(  
      loadedOnlyPkgs.list[a]
    ) |>
    c( References_articles
    ) ->
    References_articles ;
} ;

References_articles |>
  write( 
    paste( ExportFile1, 
           "References_Articles.bib", 
           sep = "/" 
    ) 
  ) ;

# xlsx
"Versions, Packages and Repository"  |>
  c(  capture.output( sessionInfo() ) ) |>
  c(  "" ) |>
  c(  "" ) |>
  c(  "Repository" ) |>
  c(  getOption( "repos" ) ) |>
  c(  "" ) |>
  c(  "" ) |> 
  c(  "References" ) |>
  c(  "R" ) |> 
  c(  capture.output( citation() ) ) |>
  c(  "Packages" ) ->
  references ;

sessionInfo()$otherPkgs -> OtherPkgs.list ;

for (a in 1:length( OtherPkgs.list ) )
{
  names(OtherPkgs.list)[a] |> 
    citation() |> 
    capture.output() |> 
    c( references
    ) -> 
    references ;
} ;

sessionInfo() -> a ;
names( a$loadedOnly ) -> loadedOnlyPkgs.list ;

for (a in 1:length( loadedOnlyPkgs.list ) )
{
  loadedOnlyPkgs.list[a] |> 
    citation() |> 
    capture.output() |> 
    c( references
    ) -> 
    references ;
} ;

Objects.ToExport.list1 <- c( Objects.ToExport.list1, 
                             list( references = references 
                             ) 
) ;

Objects.ToExport.XLSX.list1 <- c( Objects.ToExport.XLSX.list1, 
                                  list( references = references 
                                  ) 
) ;

rm( OtherPkgs.list, a, References_articles ) ;

# Z.2 Exporting results
#~~~~~~~~~~~~~~~~~~~~~~

Objects.ToExport.XLSX.list1[names(Objects.ToExport.XLSX.list1) != ""] ->
  Objects.ToExport.XLSX.list1 ;

Objects.ToExport.XLSX.list1$references |>
  as.data.frame() ->
  Objects.ToExport.XLSX.list1$references ;


paste( ExportFile1, 
       "Results_InCSV", 
       sep = "/" 
) -> 
  tmp.file1 ;

dir.create( tmp.file1, mode = "0777" ) ;


for ( a in 1:length( Objects.ToExport.XLSX.list1 ) )
{
  
  paste( names( Objects.ToExport.XLSX.list1 )[a],
         ".csv", 
         sep = "" 
  ) -> 
    name ;
  
  paste( tmp.file1, 
         name , 
         sep = "/"
  ) -> 
    name ; 
  
  write.csv( x = Objects.ToExport.XLSX.list1[[a]], 
                        file = name ) ;
} 

rm( name, a, tmp.file1 ) ;

# XLSX
paste( ExportFile1,
       "Results_InXLSX",
       sep = "/" ) ->
  tmp.file2 ;

dir.create( tmp.file2, mode = "0777" ) ;


for ( a in 1:length( Objects.ToExport.XLSX.list1 ) )
{
  paste( names( Objects.ToExport.XLSX.list1 )[a],
         ".xlsx",
         sep = "" ) ->
    name ;
  
  paste( tmp.file2,
         name ,
         sep = "/") ->
    name ;
  
  writexl::write_xlsx( x = Objects.ToExport.XLSX.list1[[a]],
                       path = name ) ;
} ;

rm( name, a, tmp.file2 ) ;

# Z.3 Exporting objects
#~~~~~~~~~~~~~~~~~~~~~~

# Save objects
save( Objects.ToExport.list1,  
      file = "~/Desktop/Results/RESTCA1_ObjectsForLinkScript.RData" )


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Save environment 
#renv::snapshot()

# Print runing time
#~~~~~~~~~~~~~~~~~~
Sys.time() -> EndTime ; EndTime - StartTime -> Time ;  print(Time) ; rm( StartTime, EndTime, Time ) ;

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Cleaning and Reset Configuration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#graphics.off() ;

#rm( list = ls() ) ;

#.rs.restartR() ;

#RestartR <- function() { assign( '.Last',  function() { system('R') }, envir = globalenv()) ; quit( save = 'no') } ; RestartR() ;

