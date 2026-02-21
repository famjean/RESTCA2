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
   "forcats", "NbClust", "ggwordcloud", "textplot", "ggraph",
   "concaveman",
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
    weight.filter = 2, 
    labels.filter = 0.2, 
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
  tm::removeSparseTerms( 0.97) |>
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
  tm::removeSparseTerms( 0.97) |>
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
  tm::removeSparseTerms( 0.97) |>
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
  tm::removeSparseTerms( 0.97) |>
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
  (function(x) reshape( x, varying = 2:ncol(x), 
                        direction = "long", sep = "" ) )() |> 
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
          iter = 10000, 
          background = FALSE, 
          trace = T ) ->
  BTmodel

terms( BTmodel , top_n = 10
)  ->
  BTterms ;
lapply( 1:length(BTterms), 
        function(a) data.frame( topic = a, BTterms[[a]] ) ) |>
  dplyr::bind_rows() ->
  BTterms ;

terms( BTmodel, top_n = 400
)  ->
  BTterms2 ;

lapply( 1:length(BTterms2), 
        function(a) data.frame( topic = a, BTterms2[[a]] ) ) |>
  dplyr::bind_rows() ->
  BTterms2 ;

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
BTterms2 |>
  dplyr::arrange( desc(probability) ) |>
  head(24) |>
ggplot() +
  aes(label = token, 
      size = probability * 10,
 #     x = topic,
      angle_group = topic,
      color = topic ) +
  geom_text_wordcloud( shape ="triangle-upright" ) +
  theme_minimal() +
  scale_color_gradient(low = "black", high = "grey80") -> F3 ; F3

ggsave( F3, width = 7, height = 7,
        filename = paste0( GraphFile1, "/Biterms_topic_3WC_Q5.png") )

ggsave( F3,  
        filename = paste0( GraphFile1, "/Biterms_topic_3WC_Q5.pdf") )

# prepare data BTM
BTterms2 |>
  dplyr::arrange( desc(probability) ) |>
  head(24) |>
  subset( select = c( token, topic, probability ) 
          )-> tmp4 ;
table(tmp4$token)[ table(tmp4$token) > 1] |>
  names() ->
  names.a ;
for (a in names.a)
{
  tmp4[ tmp4$token == a, "token" ][1] -> token1 ;
  tmp4[ tmp4$token == a, "topic" ] |>
    paste( collapse = "-" ) -> topic1 ;
  tmp4[ tmp4$token == a, "probability" ] |>
    mean() -> prob1 ;
  tmp4$topic <- as.character( tmp4$topic ) ;
  rbind( tmp4[ tmp4$token != a, ],
         c( token1, topic1, prob1 ) ) |>
    data.frame() ->
    tmp4 ;
  tmp4$topic <- factor( tmp4$topic ) ;
  tmp4$probability <- as.numeric( tmp4$probability ) ;
}
rm(a,names.a)
# prepare network 
tmp6 <- NULL ;
for (a in 1:max( unique( df.tmp1$id ) ) )
{
  subset(df.tmp1, df.tmp1$id == a) ->
    tmp.a ;
  
  if ( nrow(tmp.a) > 1 )
  {
    for (b in 1:( nrow(tmp.a) - 1 ) )
    {
      rbind( tmp6,
             c( tmp.a[b,2], 
                tmp.a[b+1,2] )
      ) ->
        tmp6 ; 
    }
  }
}
rm( tmp.a )
for (a in 1:nrow(tmp6) )
{
  if ( tmp6[a,1] > tmp6[a,2] )
  {
    tmp6[a,1] -> t1 ;
    tmp6[a,1] -> t2 ;
    t1 -> tmp6[a,2] ;
    t2 -> tmp6[a,1] ;
  }
}
rm( t1, t2)
apply( tmp6, 1, paste, collapse = "_") -> tmp7 ;
data.frame( two_tokens = tmp7 ) -> tmp7 ;
data.frame( two_tokens = names( table( tmp7$two_tokens ) ), 
       weight = as.numeric( table( tmp7$two_tokens ) ) ) -> tmp8 ;
strsplit(tmp8$two_tokens, "_" ) |> 
  rbind.force() |>
  (function(x) data.frame( from = x[,1], 
                           to = x[,2], 
                           link.weight = tmp8$weight ) )() ->
  tmp9 ; 
tmp9 |>
  subset( tmp9$from %in% c( tmp4$token ) & tmp9$to %in% c( tmp4$token ) ) |>
  na.omit() |>
  data.frame() ->
  tmp10 ; 

all(  c(tmp10$from, tmp10$to ) %in% tmp4$token )

links <- data.frame( tmp10 ) ;
nodes <- data.frame( tmp4 ) |> dplyr::arrange( topic ) ;
# Create graph
igraph::graph_from_data_frame(
  d = links, # from to type weight
  vertices = nodes, # id label type type.label size 
  directed = F 
) ->
  net1 ;
# Custom layout
igraph::vcount(net1) -> n1 ;
matrix(NA, nrow = n1, ncol = 2) -> layout1 ;  # creat matrix
set.seed(123)  
layout1[1:2, 1] <- rnorm( length( 1:2 ), -0.5, 1)  # Assign x-coordinates 
layout1[4:11, 1] <- rnorm( length( 4:11 ), 2.5, 1)  # Assign x-coordinates 
layout1[13:19, 1] <- rnorm( length( 13:19 ), -2.5, 1)  # Assign x-coordinates 
layout1[12, 1] <- rnorm( length( 12 ), -0.5, 0)  # Assign x-coordinates 
layout1[ c(3,20), 1] <- rnorm( length( c(3,20) ), 0.5, 0.8)  # Assign x-coordinates 
layout1[1:2, 2] <- rnorm( length( 1:2 ), 3, 0.3)  # Assign y-coordinates 
layout1[4:11, 2] <- rnorm( length( 4:11 ), 0, 1)  # Assign y-coordinates 
layout1[13:19, 2] <- rnorm( length( 13:19 ), 0, 1)  # Assign y-coordinates 
layout1[12, 2] <- rnorm( length( 12 ), 1.3, 0)  # Assign y-coordinates 
layout1[ c(3,20), 2] <- rnorm( length( c(3,20) ), 1.7, 0.8)  # Assign y-coordinates 
# Plot
plot(
  net1,
  layout = layout1,
  edge.color = "grey80",
  edge.width = igraph::E(net1)$link.weight * 1,
  vertex.color = "grey80",
  vertex.frame.color = "grey60", 
  vertex.size = igraph::V(net1)$probability * 100,
  #vertex.label = igraph::V(net1)$media,
  vertex.label.color = "black",
  mark.groups = list( c(1:2, 3, 20, 12), c(4:11, 12), c(13:19, 3, 20, 12) ),
  mark.col = c(adjustcolor("green", alpha.f = 0.4),
               adjustcolor("red", alpha.f = 0.4),
               adjustcolor("blue", alpha.f = 0.4)
               ),
  mark.border = c( "green", "red","blue")
)
# legend
legend( x = "bottomleft", 
        c("topic 1","topic 2", "topic 3"), 
        pch=21,
        col = c("green", "red","blue"), 
        pt.bg = c("green", "red","blue"), 
        pt.cex = 2, 
        cex = 0.8, 
        bty = "n", 
        ncol = 1 )

png( filename = paste0( GraphFile1, "/BTMv2_Q5.png")
     # width = 400, height = 400
)
plot(
  net1,
  layout = layout1,
  edge.color = "grey80",
  edge.width = igraph::E(net1)$link.weight * 1,
  vertex.color = "grey80",
  vertex.frame.color = "grey60", 
  vertex.size = igraph::V(net1)$probability * 100,
  #vertex.label = igraph::V(net1)$media,
  vertex.label.color = "black",
  mark.groups = list( c(1:2, 3, 20, 12), c(4:11, 12), c(13:19, 3, 20, 12) ),
  mark.col = c(adjustcolor("green", alpha.f = 0.4),
               adjustcolor("red", alpha.f = 0.4),
               adjustcolor("blue", alpha.f = 0.4)
  ),
  mark.border = c( "green", "red","blue")
)
# legend
legend( x = "bottomleft", 
        c("topic 1","topic 2", "topic 3"), 
        pch=21,
        col = c("green", "red","blue"), 
        pt.bg = c("green", "red","blue"), 
        pt.cex = 1.5, 
        cex = 0.8, 
        bty = "n", 
        ncol = 1 )
dev.off()


pdf( file = paste0( GraphFile1, "/BTMv2_Q5.pdf")
     # width = 400, height = 400
)
plot(
  net1,
  layout = layout1,
  edge.color = "grey80",
  edge.width = igraph::E(net1)$link.weight * 1,
  vertex.color = "grey80",
  vertex.frame.color = "grey60", 
  vertex.size = igraph::V(net1)$probability * 100,
  #vertex.label = igraph::V(net1)$media,
  vertex.label.color = "black",
  mark.groups = list( c(1:2, 3, 20, 12), c(4:11, 12), c(13:19, 3, 20, 12) ),
  mark.col = c(adjustcolor("green", alpha.f = 0.4),
               adjustcolor("red", alpha.f = 0.4),
               adjustcolor("blue", alpha.f = 0.4)
  ),
  mark.border = c( "green", "red","blue")
)
# legend
legend( x = "bottomleft", 
        c("topic 1","topic 2", "topic 3"), 
        pch=21,
        col = c("green", "red","blue"), 
        pt.bg = c("green", "red","blue"), 
        pt.cex = 1.5, 
        cex = 0.8, 
        bty = "n", 
        ncol = 1 )
dev.off()


png( filename = paste0( GraphFile1, "/BTMv2BW_Q5.png")
     # width = 400, height = 400
)
plot(
  net1,
  layout = layout1,
  edge.color = "grey60",
  edge.width = igraph::E(net1)$link.weight * 1,
  vertex.color = "grey60",
  vertex.frame.color = "grey40", 
  vertex.size = igraph::V(net1)$probability * 100,
  #vertex.label = igraph::V(net1)$media,
  vertex.label.color = "black",
  mark.groups = list( c(1:2, 3, 20, 12), c(4:11, 12), c(13:19, 3, 20, 12) ),
  mark.col = c(adjustcolor("grey60", alpha.f = 0.8),
               adjustcolor("grey30", alpha.f = 0.8),
               adjustcolor("grey80", alpha.f = 0.8)
  ),
  mark.border = c( "grey60", "grey30","grey80")
)
# legend
legend( x = "bottomleft", 
        c("topic 1","topic 2", "topic 3"), 
        pch=21,
        col = c("grey60", "grey30","grey80"), 
        pt.bg = c("grey60", "grey30","grey80"), 
        pt.cex = 1.5, 
        cex = 0.8, 
        bty = "n", 
        ncol = 1 )
dev.off()


pdf( file = paste0( GraphFile1, "/BTMv2BW_Q5.pdf")
     # width = 400, height = 400
)
plot(
  net1,
  layout = layout1,
  edge.color = "grey60",
  edge.width = igraph::E(net1)$link.weight * 1,
  vertex.color = "grey60",
  vertex.frame.color = "grey40", 
  vertex.size = igraph::V(net1)$probability * 100,
  #vertex.label = igraph::V(net1)$media,
  vertex.label.color = "black",
  mark.groups = list( c(1:2, 3, 20, 12), c(4:11, 12), c(13:19, 3, 20, 12) ),
  mark.col = c(adjustcolor("grey60", alpha.f = 0.8),
               adjustcolor("grey30", alpha.f = 0.8),
               adjustcolor("grey80", alpha.f = 0.8)
  ),
  mark.border = c( "grey60", "grey30","grey80")
)
# legend
legend( x = "bottomleft", 
        c("topic 1","topic 2", "topic 3"), 
        pch=21,
        col = c("grey60", "grey30","grey80"), 
        pt.bg = c("grey60", "grey30","grey80"), 
        pt.cex = 1.5, 
        cex = 0.8, 
        bty = "n", 
        ncol = 1 )
dev.off()

# LDA
lapply( 2:20,
        function(x) {
          topicmodels::LDA( tm::as.TermDocumentMatrix( 
            t( as.matrix( 
            tdm.clean.lemma.NoPunctuation ) ), 
            weighting = tm::weightTf) ,
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

set.seed(123)

posterior.topic$terms |>
  as.matrix() |>
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "token") |>
  tidyr::pivot_longer(
    cols = -token,
    names_to = "topic",
    values_to = "probability"
  ) |>
  transform( topic = factor(topic) ) |> 
  dplyr::arrange( desc(probability) ) |>
  head(60) |>
  ggplot() +
  aes(label = token, 
      size = probability * 10,
      angle_group = topic,
      color = topic ) +
  geom_text_wordcloud( ) +
  theme_minimal()  -> F4 ; F4

ggsave( F4, 
        filename = paste0( GraphFile1, "/LDA_topic_6WC_Q5.png") )

ggsave( F4,  
        filename = paste0( GraphFile1, "/LDA_topic_6WC_Q5.pdf") )

set.seed(123)
posterior.topic$terms |> 
  t() |> 
  ( function(x) cbind( words = rownames(x), x ) )() |>
  as.data.frame() |>
  reshape(
    direction = "long",
    varying = colnames( t( posterior.topic$terms ) ),
    v.names = "probability",
    timevar = "topic",
    times = colnames( t( posterior.topic$terms ) )
  ) |>
  transform( probability = as.numeric(probability),
             topic = as.numeric( topic) ) |>
  dplyr::arrange( desc(probability) ) |>
  head(40) |>
  ggplot() +
  aes(label = words, 
      size = probability * 10,
      #     x = topic,
      angle_group = topic,
      color = topic ) +
  geom_text_wordcloud( ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2", name = "Topic" ) -> F4B ; F4B

ggsave( F4B, width = 7, height = 7,
        filename = paste0( GraphFile1, "/LDAbis_topic_3WC_Q5.png") )

ggsave( F4B,  
        filename = paste0( GraphFile1, "/LDAbis_topic_3WC_Q5.pdf") )





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

Q5.wordNetwork.Links = Q5.wordNetwork$Links

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
save( list = names(Objects.ToExport.list1),  
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

