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
# 3. Text mining 
  # 3.1 obstacle_PEC_TCA
# 4. Other analysis
  # 4.1 Descriptive

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
paste( "~/Desktop/Results/Export_RESTCA1c_", 
       format( Sys.time(), "%Y%m%d%Hh%M" ), 
       sep = "" ) -> 
  ExportFile1 ;

dir.create( ExportFile1, mode="0777" ) ;

# 1.5 Reproducibility for  packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if ( !require( "renv", character.only = TRUE  ) )
{ 
  install.packages( "renv" ) ; 
  #require( "renv", character.only = TRUE ) ;
} ;

# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = getOption("download.file.method")) ; options(repos="http://cran.rstudio.com/") ; Sys.setenv("RENV_CONFIG_REPOS_OVERRIDE" = "http://cran.rstudio.com")

#renv::init()
#renv::restore()
#renv::snapshot()
renv::status()
# renv::deactivate(clean = TRUE)

# 1.6 Install packages
#~~~~~~~~~~~~~~~~~~~~~

c( "openxlsx", "furrr", "ggplot2", "wordcloud","corrplot", "svglite",
   "forcats", "NbClust", "ggwordcloud",
   "BTM", "topicmodels", "igraph", "tnet",  "scatterplot3d"
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

c( "ggplot2", "ggwordcloud"
) -> 
  PackagesNames1 ;

for (a in 1:length( PackagesNames1 ) )
{
  require( PackagesNames1[a], character.only = TRUE  ) 
}

rm( PackagesNames1 ) ;


# 1.8 Load functions
#~~~~~~~~~~~~~~~~~~~

source( "ScriptR0_StockOfFunctions_Parallel.R" )

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

c( "~/Desktop/Results/RESTCA_clean_data.RData" ) -> FilesToImport1 ;

# 2.2 Files loading
#~~~~~~~~~~~~~~~~~~

base::load( FilesToImport1 )

# Cleaning
#~~~~~~~~~
rm( FilesToImport1 ) ;

# 2.3 Quality ckecking 
#~~~~~~~~~~~~~~~~~~~~~

df1.av |>
  DataQuality.Analysis() #|>
 # View() ;

#df1.cc |>
#  DataQuality.Analysis() |>
#  View() ;

#df1.im |>
#  DataQuality.Analysis() |>
#  View() ;

# 2.4 Subseting
#~~~~~~~~~~~~~~
df1.av |>
  subset( !is.na( df1.av$obstacle_PEC_TCA ) ) ->
  df1.av.ccTM ;
  
dim( df1.av.ccTM ) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# TABLE OF CONTENTS FOR SECTION

# 3. Text mining  -----------------------------
  # 3.1 obstacle_PEC_TCA

# 3. Text mining 
#~~~~~~~~~~~~~~~

# 3.1 obstacle_PEC_TCA  ---------------------
#~~~~~~~~~~~~~~~~~~~~

# Prepare graphing
paste( ExportFile1, 
       "Graph_TextMining_Q5_obstacle_PEC_TCA",  
       sep = "/" 
) -> 
  GraphFile1 ;

dir.create( GraphFile1, mode="0777" ) 

# Prepare corpus
df1.av$obstacle_PEC_TCA |>
  as.character() |>
  CleanEmpty() |>
  na.omit() |>
  as.list() |>
  tm::VectorSource() |>
  tm::VCorpus() |>
  TextCleaning( 
    WordsToRM = c( "l", "c" ),
    language = "french",
    RMstopwords = TRUE,
    RMpunctuation =  FALSE
  )|>
  lemmatization.corpus( 
    lang = "fr",
    unknown.words = TRUE
  ) ->
  clean.lemma.Punctuation ;

clean.lemma.Punctuation |>
  TextCleaning( 
    language = "fr",
    RMstopwords = FALSE,
    RMpunctuation =  TRUE
  ) ->
  clean.lemma.NoPunctuation

clean.lemma.NoPunctuation |>
  tm::TermDocumentMatrix() ->
  tdm.clean.lemma.NoPunctuation ;

tdm.clean.lemma.NoPunctuation |>
  tf.idf.rank() ->
  tdm.clean.lemma.NoPunctuation.tfidf  ;

tdm.clean.lemma.NoPunctuation.tfidf ->  
  Q5.obstacle_PEC_TCA.lemma.NoPunctuation.tfidf ;

Corpus.Description( clean.lemma.Punctuation, round = 2 ) ->
  Q5.obstacle_PEC_TCA.Description.doc ;

# wordnetwork

clean.lemma.Punctuation |>
  lapply( as.character ) |>
  unlist() |>
  # (function(x) substring( x, first = 1, last = nchar(x) - 1 ) )() |>
  #paste0( ".") |>
  TextToWordNetwork.preparation( 
    Sentence.fusion.proximity = 0,
    WordsToFusion = list( c("", "") ),
    remove.stopwords = TRUE,
    language = "french",
    words.to.remove = NULL 
  ) |>
  WordNetwork( 
    save.plot = FALSE, 
    exportfile = GraphFile1, 
    wordfrequence.filter = 2, 
    weight.filter = 0.1, 
    labels.filter = 0.1, 
    seed = 123 ) ->
  Q5.wordNetwork ;

Q5.wordNetwork$Network

ggsave( Q5.wordNetwork$Network, height = 6, width = 6, dpi = 600,
        filename = paste0( GraphFile1, "/Wordnet_Q5.png") )

ggsave( Q5.wordNetwork$Network, height = 6, width = 6, dpi = 600,
        filename = paste0( GraphFile1, "/Wordnet_Q5.pdf") )

Q5.wordNetwork$Barplot

ggsave( Q5.wordNetwork$Barplot, dpi = 600,
        filename = paste0( GraphFile1, "/Barplot_Q5.png") )

ggsave( Q5.wordNetwork$Barplot, dpi = 600,
        filename = paste0( GraphFile1, "/Barplot_Q5.pdf") )

# wordcloud

png( filename = paste0( GraphFile1, "/Wordcloud_Q5.png"), 
     # width = 400, height = 400 
)
set.seed(14243);
wordcloud::wordcloud( rownames(tdm.clean.lemma.NoPunctuation.tfidf),
                      tdm.clean.lemma.NoPunctuation.tfidf$total,
                      max.words = 20,
                      rot.per = 0.2,
                      min.freq = 1,
                      colors = Scale.Blue(30) ) ;
dev.off()

pdf( file = paste0( GraphFile1, "/Wordcloud_Q5.pdf"), 
     # width = 400, height = 400 
)
set.seed(14243);
wordcloud::wordcloud( rownames(tdm.clean.lemma.NoPunctuation.tfidf),
                      tdm.clean.lemma.NoPunctuation.tfidf$total,
                      max.words = 20,
                      rot.per = 0.2,
                      min.freq = 1,
                      colors = Scale.Blue(30) ) ;
dev.off()

# tdm

tdm.clean.lemma.NoPunctuation |>
  tm::removeSparseTerms( 0.98) |>
  as.matrix() |>
  (function(x) data.frame( x, 
                           counttot = rowSums(x), 
                           words = rownames(x) ) )() |>
  dplyr::filter( counttot > 0 ) |>
  dplyr::arrange( counttot  )  |>
  tidyr::gather(  key = "documents", 
                  value = count,
                  -words, 
                  -counttot )  |>
  dplyr::arrange( counttot )  |>
  ggplot2::ggplot( aes( y = reorder( words, counttot),  
                        x = documents,
                        fill = count #log10(count)
  ) ) +
  geom_tile(colour = "white") +
  scale_fill_gradient( high = "blue", low="lightskyblue1", na.value = "grey90") +    ylab("") +
  theme(panel.background = element_blank() ) +
  labs( #fill = "log10(compte)",
    x = "Documents", y = "Terms"  ) -> 
  F1 ;

F1 ;

ggsave( F1, filename = paste0( GraphFile1, "/TDM_Q5.png"),
        width = 12, height = 11, dpi = 600 )

ggsave( F1, filename = paste0( GraphFile1, "/TDM_Q5.pdf"),
        width = 12, height = 11, dpi = 600)

# hclust
set.seed(14243);
tdm.clean.lemma.NoPunctuation |>
  tm::removeSparseTerms( 0.96) |>
  as.matrix() |>
  (function(x) data.frame( x, 
                           counttot = rowSums(x), 
                           words = rownames(x) ) )() |>
  dplyr::filter( counttot > 0 ) |>
  dplyr::arrange( counttot  )  |>
  dplyr::select( -counttot ) |>
  (function(x) magrittr::set_rownames( x, x$words ) )() |>
  dplyr::select( -words ) -> dmatrix ;
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "silhouette")
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "kl")
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "dunn")
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "gap")
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "sdbw")
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "sdindex")
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "hubert")
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "tau")
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "ch")
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "cindex")
NbClust::NbClust( data = dmatrix,
                  distance = "euclidean",
                  min.nc = 2, max.nc = 15, 
                  method = "ward.D", index = "db")


png( filename = paste0( GraphFile1, "/Hclust_Q5.png")
)
set.seed(14243);
tdm.clean.lemma.NoPunctuation |>
  tm::removeSparseTerms( 0.96) |>
  as.matrix() |>
  (function(x) data.frame( x, 
                           counttot = rowSums(x), 
                           words = rownames(x) ) )() |>
  dplyr::filter( counttot > 0 ) |>
  dplyr::arrange( counttot  )  |>
  dplyr::select( -counttot ) |>
  (function(x) magrittr::set_rownames( x, x$words ) )() |>
  dplyr::select( -words ) |>
  dist( method = "euclidian") |>
  hclust( method = "ward.D" ) -> hc1 ; 
  plot(  as.dendrogram(hc1),horiz=F,
         xlab = "", main = "",  
         ylab = "High", sub = "Ward Method") ;

dev.off()

pdf( file = paste0( GraphFile1, "/Hclust_Q5.pdf")
)
set.seed(14243);
tdm.clean.lemma.NoPunctuation |>
  tm::removeSparseTerms( 0.96) |>
  as.matrix() |>
  (function(x) data.frame( x, 
                           counttot = rowSums(x), 
                           words = rownames(x) ) )() |>
  dplyr::filter( counttot > 0 ) |>
  dplyr::arrange( counttot  )  |>
  dplyr::select( -counttot ) |>
  (function(x) magrittr::set_rownames( x, x$words ) )() |>
  dplyr::select( -words ) |>
  dist( method = "euclidian") |>
  hclust( method = "ward.D" ) -> hc1 ; 
plot(  as.dendrogram(hc1),horiz=F,
       xlab = "", main = "",
       ylab = "High", sub = "Ward Method") ;
#rect.hclust(hc1, k = 3, border = 2:4)

dev.off()

# corrplot spaerman

png( filename = paste0( GraphFile1, "/Corrplot_Q5.png")
     # width = 400, height = 400
)
set.seed(14243);
tdm.clean.lemma.NoPunctuation |>
  tm::removeSparseTerms( 0.97) |>
  as.matrix() |>
  t() |>
  cor( method = "spearman" ) |>
  corrplot::corrplot( type = "lower",
                      order = "hclust",
                      hclust.method = "ward.D",
                      tl.col = "black",
                      diag = FALSE )
dev.off()


pdf( file = paste0( GraphFile1, "/Corrplot_Q5.pdf")
     # width = 400, height = 400
)
set.seed(14243);
tdm.clean.lemma.NoPunctuation |>
  tm::removeSparseTerms( 0.97) |>
  as.matrix() |>
  t() |>
  cor( method = "spearman" ) |>
  corrplot::corrplot( type = "lower",
                      order = "hclust",
                      hclust.method = "ward.D",
                      tl.col = "black",
                      diag = FALSE )
dev.off()

# bitopic

set.seed(14243);
tdm.clean.lemma.NoPunctuation |>
  tm::removeSparseTerms( 0.97) |>
  as.matrix() |>
  (function(x) data.frame( Terms = rownames(x), x ) )() |>
  (function(x) reshape( x, varying = 2:ncol(x), direction = "long", sep = "" ) )() |> 
  base::subset( select = c( Terms, time), subset = X != 0 ) |>
  (function(x) data.frame( id = x$time, lemma = x$Terms ) )() ->
  df.tmp1

a = 15
lapply( 2:a, function(x) {
  BTM::BTM( df.tmp1,
            k = a, 
            iter = 10000, 
            background = F, trace = T ) } ) ->
  Models ;

Models |>
  lapply( function(x) logLik(x)$ll )  |>
  as.matrix()  |>
  as.data.frame()  |>
  dplyr::mutate( k = 2:a, logLikelihood = as.numeric( V1 ) )  |>
  dplyr::select( k, logLikelihood ) ->
  models.df ;

models.df  |>
  ggplot( ) +
  aes( x = k, y = logLikelihood ) +
  geom_point() +
  geom_line() +
  scale_x_continuous( breaks = seq(0,100,1)) ->
  F2 ; 

F2 ;

ggsave( F2, filename = paste0( GraphFile1, "/Biterms_topic_loglik_Q5.png") )

BTM::BTM( df.tmp1,
          k = 3, 
          iter = 1000, 
          background = FALSE, 
          trace = 500 ) ->
  BTmodel

terms( BTmodel , top_n = 10
)  ->
  BTterms ;

lapply( 1:length(BTterms), 
        function(a) data.frame( topic = a, BTterms[[a]] ) ) |>
  dplyr::bind_rows() ->
  BTterms ;

png( filename = paste0( GraphFile1, "/Biterms_topic_Q5.png"), 
     # width = 400, height = 400
)
set.seed(122)
BTterms |>
  reshape( direction = "wide", idvar = "token", timevar = "topic" ) |> 
  (function(x) { x[,1] -> rownames(x) ; x[,-1] -> x ; 1:ncol(x) -> colnames(x) ; x } )()-> df1.tmp ; 
df1.tmp[ is.na(df1.tmp) ] <- 0 ;
df1.tmp |>
  wordcloud::comparison.cloud(  scale = c( 2, 0.5), 
                                max.words =  60, 
                                colors = rainbow(10),
                                title.size = 2, 
                                title.colors = "black",
                                title.bg.colors = "grey95" ) ;
dev.off()

set.seed(123)
BTterms |>
ggplot() +
  aes(label = token, 
      size = probability * 10,
 #     x = topic,
      angle_group = topic,
      color = topic ) +
  geom_text_wordcloud( ) +
  theme_minimal() +
  scale_color_gradient(low = "black", high = "grey80") -> F3 ; F3

ggsave( F3, 
        filename = paste0( GraphFile1, "/Biterms_topic_3WC_Q5.png") )

ggsave( F3,  
        filename = paste0( GraphFile1, "/Biterms_topic_3WC_Q5.pdf") )



# LDA
lapply( 2:20,
        function(x) {
          topicmodels::LDA( tm::as.TermDocumentMatrix( t( as.matrix( 
            tdm.clean.lemma.NoPunctuation ) ), weighting = tm::weightTf) ,
            k = x,
            method = "Gibbs",
            control = list( seed =  list(254672,109,122887,145629037,2),
                            nstart = 5,
                            best = TRUE,
                            iter = 5000,
                            burnin = 1000,
                            thin = 100 ) ) }
)  |>
  lapply(  topicmodels::logLik ) |>
  as.matrix() |>
  as.data.frame() |>
  dplyr::mutate( k = 2:20, logLikelyhood = as.numeric( V1 ) ) |>
  dplyr::select(  k, logLikelyhood )  |>
  ggplot(  ) +
  aes( x = k, y = logLikelyhood ) +
  geom_point() +
  geom_line() +
  geom_vline( xintercept = 6, linetype = 2) +
  scale_x_continuous( breaks = seq(0,100,1) ) ->
  graph.bestmodelselection ;

ggsave( graph.bestmodelselection, filename = paste0( GraphFile1, "/LDA_topic_loglik_Q5.png") )
ggsave( graph.bestmodelselection, filename = paste0( GraphFile1, "/LDA_topic_loglik_Q5.pdf") )


graph.bestmodelselection

tm::as.TermDocumentMatrix( t( as.matrix( tdm.clean.lemma.NoPunctuation ) ), weighting = tm::weightTf ) |>
  topicmodels::LDA( 
    k = 6,
    method = "Gibbs",
    control = list( seed =  list(254672,109,122887,145629037,2),
                    nstart = 5,
                    best = TRUE,
                    iter = 5000,
                    burnin = 1000,
                    thin = 100 ) ) ->
  topic1 ;

topicmodels::posterior( topic1 ) -> posterior.topic ;

rm( topic1 )

png( filename = paste0( GraphFile1, "/LDA_topic_Q5.png"), 
     # width = 400, height = 400 
)
posterior.topic$terms |>
  as.matrix() |>
  t() |>
  wordcloud::comparison.cloud(  
    scale = c( 2, 0.5), max.words =  60, colors = rainbow(11) ) ;
dev.off()

pdf( file = paste0( GraphFile1, "/LDA_topic_Q5.pdf"), 
     # width = 400, height = 400 
)
posterior.topic$terms |>
  as.matrix() |>
  t() |>
  wordcloud::comparison.cloud(  
    scale = c( 2, 0.5), max.words =  60, colors = rainbow(11) ) ;
dev.off()



# Stock results --------------
#~~~~~~~~~~~~~~
Objects.ToExport.list1 <- c( Objects.ToExport.list1, 
                            list(
                              
                              Q5.obstacle_PEC_TCA.Description.doc = Q5.obstacle_PEC_TCA.Description.doc,
                              Q5.obstacle_PEC_TCA.lemma.NoPunctuation.tfidf = Q5.obstacle_PEC_TCA.lemma.NoPunctuation.tfidf,
                              Q5.wordNetwork = Q5.wordNetwork$WordsFreq,
                              Q5.wordNetwork.Links = Q5.wordNetwork$Links
                              
                            ) 
) ;

Objects.ToExport.XLSX.list1 <- c( Objects.ToExport.XLSX.list1, 
                             list(
                               
                               Q5.obstacle_PEC_TCA.Description.doc = Q5.obstacle_PEC_TCA.Description.doc,
                               Q5.obstacle_PEC_TCA.lemma.NoPunctuation.tfidf = Q5.obstacle_PEC_TCA.lemma.NoPunctuation.tfidf,
                               Q5.wordNetwork = Q5.wordNetwork$WordsFreq,
                               Q5.wordNetwork.Links = Q5.wordNetwork$Links
                             ) 
) ;

# Cleaning
#~~~~~~~~~
rm( GraphFile1, F1, F2 ) ;


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# TABLE OF CONTENTS FOR SECTION

# 4. Other analysis  -----------------------------
# 4.1 Descriptive

# 4. Other analysis
#~~~~~~~~~~~~~~~~~~

# 4.1 Descriptive
#~~~~~~~~~~~~~~~~

df1.av.ccTM|>
  Univariate.Analysis(  
    conf.level = 0.95, binom.ci.method = "exact", 
    boot.ci.method = "bca", nsim = 10000, round = 2, seed = 123, 
    ncores = NbOfCores1 
  ) -> 
  results.univariate.ccTM ;

# Stock results --------------
#~~~~~~~~~~~~~~
Objects.ToExport.list1 <- c( Objects.ToExport.list1, 
                             list(
                               results.univariate.ccTM = results.univariate.ccTM
                               
                             ) 
) ;

Objects.ToExport.XLSX.list1 <- c( Objects.ToExport.XLSX.list1, 
                                  list(
                                    
                                    results.univariate.ccTM = results.univariate.ccTM
                                  ) 
) ;

# Cleaning
#~~~~~~~~~
rm(  ) ;

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
      file = "~/Desktop/Results/RESTCA2_ObjectsForLinkScript.RData" )


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

