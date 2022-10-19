GLOBAL_ <- env (api.key = "IxLBfWwkta6uI7wyQxcfri72B",
                api.key.secret = "2ZuBlVCYxR3RgO3x0WTu2ilzx1y",
                bearer.token = "HJJv1EEi",
                access.token =  "142098391-",
                access.token.secret = "5ECFWIY2iFOYSlgAZGI1KjNQF7xrj",
                app = "ProjectSentimentAnalysisACC",
                INPUT_DATA_FILE_PATH  =  "data",
                PHOENIX = 1,
                METROQC = 2,
                OCTRANS = 3,
                REM = 4,
                BLEUPANIER = 5
)

install.packages <- function ( pkgs ) {
  for(p in pkgs) 
  {
    #if(!require(p,character.only = TRUE)) install.packages(p)
    if (!p %in% installed.packages()) install.packages(p)
    library(p,character.only = TRUE)
  }  
}

create.token  <- function (){
  
  token <- create_token(  app = GLOBAL_$app,
                consumer_key = GLOBAL_$api.key,
                consumer_secret = GLOBAL_$api.key.secret,
                access_token = GLOBAL_$access.token,
                access_secret = GLOBAL_$access.token.secret  )  
  return(token)
}


seach.tweets <- function (keyword, token_){
   tweets <- search_tweets (keyword, n = 250000, include_rts = TRUE,token = token_)
   return(tweets)
}

get.timeline <- function(keyword, n_, token_){

  timeline_ <- get_timeline( keyword, n = n_, token = token_)  
}

clean.data <- function (data_){
  
  stripped  <- gsub("http\\S+", "", data_)
  stripped  <- gsub("[s|S]*[0-9]+", "", stripped)
  stripped  <- gsub("http[^[:space:]]*", "", stripped)
  stripped  <- gsub("#[a-z|A-Z]*", "", stripped)
  
  #data_$stripped  <- gsub("[s|S]*[0-9]+", "", data_$stripped)
  #data_$stripped  <- gsub("[c'est]+", "", data_$stripped)
  
  return(stripped)
}

read.project.file <- function(file_ ){
  
  if(!file.exists(file_)) return (NULL)
  
  return ( readRDS(file_) )
  
}

get.project.files <- function ( file_path_, pattern_   ){
  
  require(data.table)
  
  list.files = list.files(path = file_path_ , pattern = pattern_ )
  
  list.files <- paste(file_path_,list.files,sep='')
  
  list.data = lapply(list.files, function (x) data.table(readRDS(x)))
  
  files.data = rbindlist(list.data, fill = TRUE)
  
  return ( files.data )
}

save.project.file <- function (data_, file.path_, project.file.name_){
  
  file_name_ = (paste(project.file.name_,'rds',sep ='.'))
  file_ <- file.path ( file.path_ , file_name_ )
  saveRDS(data_, file_)
}

plot.fq.words <- function (data_, n_, x_label, y_label, title_){
  
    data_ %>% 
      count (word, sort = TRUE) %>% 
      top_n ( n_ ) %>% 
      mutate(word = reorder(word,n)) %>% 
      ggplot(aes(x = word, y = n)) + 
      geom_col() +
      xlab(NULL) + 
      coord_flip() + 
      theme_classic() + 
      labs(x = x_label , y = y_label, title = title_)
  
}

query.tweeter <- function (keyword_) {
  tweets_      <- seach.tweets(keyword_, token_)
  tweets.data  <- get.timeline(keyword_, 500000, token_)
  
  save.project.file(tweets_,GLOBAL_$INPUT_DATA_FILE_PATH, paste(keyword_,'_t',sep='')) 
  save.project.file(tweets.data,GLOBAL_$INPUT_DATA_FILE_PATH, paste(keyword_,'_d',sep=''))
  
}

remove.stop.words <- function (data_) {
  stop.words.fr_  <- tibble (word = stopwords_fr)
  stop.words.en_  <- tibble (word = stopwords_en)
  data.clean      <- data_  %>% 
                      anti_join(stop.words.en_) %>%
                      anti_join(stop.words.fr_) %>% 
                      anti_join(stop_words)
  return(data.clean)
}


compute_sentiments_bing <- function (data_ , project.name_, false_positive_ ) {

  tweets.projects.stem <- tweets.projects    %>%
                          select( stripped ) %>% 
                          unnest_tokens( word, stripped )
  
  tweets.projects.cleaned <- remove.stop.words (tweets.projects.stem)
  
  #plot.fq.words (tweets.projects.cleaned , 50, "Count", "Unique Words", project.name_)
  
  sentiment.bing.positive  <- get_sentiments("bing") %>% filter(sentiment == "positive")
  sentiment.bing.negative  <- get_sentiments("bing") %>% filter(sentiment == "negative")
  
  
  tweets.projects.bing <- tweets.projects.cleaned %>%
                          inner_join(get_sentiments("bing"))   %>%
                          count (word, sentiment, sort = TRUE) %>%
                          ungroup()
   
  
   tweets.projects.bing <-  filter(tweets.projects.bing , !grepl(false_positive_,word))
   #tweets.projects.bing <-  filter(tweets.projects.bing , n > 3 )
  
   tweets.projects.bing %>% 
                        group_by(sentiment) %>%
                        top_n(3) %>%
                        ungroup() %>%
                        mutate(word = reorder(word, n )) %>%
                        ggplot(aes(word, n, fill = sentiment)) +
                        geom_col (show.legend = FALSE) +
                        facet_wrap(~ sentiment, scales = "free_y") +
                        labs( title = paste("Tweets containing",project.name_,'using Bing'),
                              y  =" Contribution to sentiment",
                              x = NULL) +
                        coord_flip() + theme_bw()
}


compute_sentiments_nrc <- function (data_ , project.name_, false_positive_ ) {
  
  tweets.projects.stem <- tweets.projects    %>%
    select( stripped ) %>% 
    unnest_tokens( word, stripped )
  
  tweets.projects.cleaned <- remove.stop.words (tweets.projects.stem)
  
  #plot.fq.words (tweets.projects.cleaned , 50, "Count", "Unique Words", project.name_)
  
  sentiment.bing.positive  <- get_sentiments("nrc") %>% filter(sentiment == "positive")
  sentiment.bing.negative  <- get_sentiments("nrc") %>% filter(sentiment == "negative")
  
  
  tweets.projects.bing <- tweets.projects.cleaned %>%
    inner_join(get_sentiments("nrc"))   %>%
    count (word, sentiment, sort = TRUE) %>%
    ungroup()
  
  
  tweets.projects.bing <-  filter(tweets.projects.bing , !grepl(false_positive_,word))
  tweets.projects.bing <-  filter(tweets.projects.bing , n >= 3 )
  
  tweets.projects.bing %>% 
    group_by(sentiment) %>%
    top_n(3) %>%
    ungroup() %>%
    mutate(word = reorder(word, n )) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col (show.legend = FALSE) +
    facet_wrap(~ sentiment, scales = "free_y") +
    labs( title = paste("Tweets containing",project.name_,'using NRC'),
          y  =" Contribution to sentiment",
          x = NULL) +
    coord_flip() + theme_bw()
}

compute_sentiments_affin <- function (data_ , project.name_, false_positive_ ) {
  
  tweets.projects.stem <- tweets.projects    %>%
    select( stripped ) %>% 
    unnest_tokens( word, stripped )
  
  tweets.projects.cleaned <- remove.stop.words (tweets.projects.stem)
  
  #plot.fq.words (tweets.projects.cleaned , 50, "Count", "Unique Words", project.name_)
  
  sentiment.afinn.positive <- get_sentiments("afinn") %>% filter(value == "3")
  
  sentiment.afinn.negative <- get_sentiments("afinn") %>% filter(value == "-3")
  
  
  tweets.projects.afinn <- tweets.projects.cleaned %>%
    inner_join(get_sentiments("afinn"))   %>%
    count (word, value, sort = TRUE) %>%
    ungroup()
  
  
  
  tweets.projects.afinn <-  filter(tweets.projects.afinn , !grepl(false_positive_,word))
  #tweets.projects.afinn <-  filter(tweets.projects.afinn , n >= 3 )
  
  tweets.projects.afinn %>% 
    group_by(value) %>%
    top_n(3) %>%
    ungroup() %>%
    mutate(word = reorder(word, n )) %>%
    ggplot(aes(word, n, fill = value)) +
    geom_col (show.legend = FALSE) +
    facet_wrap(~ value, scales = "free_y") +
    labs( title = paste("Tweets containing",project.name_,'using Affin'),
          y  =" Contribution to sentiment",
          x = NULL) +
    coord_flip() + theme_bw()
}


compute_sentiments <- function ( data_ , project.name_){
  
  tweets.data <- data_
  tweets.projects <- tweets.data %>% select(screen_name, text)
  tweets.projects$stripped <- clean.data (tweets.projects$text)

  tweets.projects.stem <- tweets.projects    %>%
                          select( stripped ) %>% 
                          unnest_tokens( word, stripped )

  tweets.projects.cleaned <- remove.stop.words (tweets.projects.stem)

  plot.fq.words (tweets.projects.cleaned , 50, "Count", "Unique Words", project.name_)
  
  sentiment.bing.positive  <- get_sentiments("bing") %>% filter(sentiment == "positive")
  sentiment.afinn.positive <- get_sentiments("afinn") %>% filter(value == "3")
  
  sentiment.bing.negative  <- get_sentiments("bing") %>% filter(sentiment == "negative")
  sentiment.afinn.negative <- get_sentiments("afinn") %>% filter(value == "-3")

  
  tweets.projects.bing <- tweets.projects.cleaned %>%
                          inner_join(get_sentiments("bing"))   %>%
                          count (word, sentiment, sort = TRUE) %>%
                          ungroup()
  
  tweets.projects.afinn <- tweets.projects.cleaned %>%
                          inner_join(get_sentiments("afinn"))   %>%
                          count (word, value, sort = TRUE) %>%
                          ungroup()

  tweets.projects.bing %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n )) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col (show.legend = FALSE) +
        facet_wrap(~ sentiment, scales = "free_y") +
        labs( title = paste("Tweets containing",project.name_),
              y  =" Contribution to sentiment",
              x = NULL) +
        coord_flip() + theme_bw()

  # tweets.projects.afinn %>%
  #   group_by(value) %>%
  #   top_n(5) %>%
  #   ungroup() %>%
  #   mutate(word = reorder(word, n )) %>%
  #   ggplot(aes(word, n, fill = value)) +
  #   geom_col (show.legend = FALSE) +
  #   facet_wrap(~ value, scales =  "free_y") +
  #   labs( title = paste("Tweets containing",project.name_),
  #         y  =" Contribution to sentiment",
  #         x = NULL) +
  #   coord_flip() + theme_bw()
  
}




#===============================================================================
#           INITIALIZE PROGRAM
#===============================================================================
#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

required.pckgs = c('plyr','ggplot2','pdftools','readr','tm','tidytext','markdown',
                   'RColorBrewer','wordcloud','SnowballC','cluster','tidyverse',
                   'ggdendro','dendextend', 'rtweet','tidytext','textdata','tidyr',
                   'udpipe','lsa','rlang', 'dplyr')

install.packages(required.pckgs)
#gc()

token_          <- create.token ()
#===============================================================================


#===============================================================================
#           SEARCH FOR TWEETS
#===============================================================================

#keyword_ <- "O-Train Ottawa" #"@OC_Transpo" #"@OCTranspoLive" #@OCTranspoDirect #@OC_Transpo @Ottawa_Circule

#keyword_ <-  "@MetroQuebec" #"@MétroVAL"

#keyword_ <- "@PJCCI /@pontChampBridge @SDeChamplainMTL @PontChamplain @ChamplainPont @PJCCI"

#keyword_ <- "@REMgrandmtl"

#keyword_ <-   "@Phoenix Pay system"
#"#@PhoenixPaysystem" #,"Phoenix Pay System")
#"@Phoenix Pay system" "@fixphoenix"#"@PIPSC_IPFPC"#@Phoenixpaysyst" "@PhoenixPaySystem" 
#"Phoenix pay system"

keyword_ <- "" #"lepanierbleu" #"le panierbleu quebec" #"@lepanierbleu_qc"
query.tweeter (keyword_)

#===============================================================================


#===============================================================================
#           READ TWEETS PER FILE
#===============================================================================

dest_         <- "~/data/"
folder_       <- c('Phoenix/','MetroQC/','OCTransport/','REM/','BleuPanier/')
project.name  <- c('Project Phoenix Pay System','Metro Quebec city ',
                   'Ottawa Transport','Reseau Express of Montreal ','Panier bleu') 


# path_PHOENIX  <-  paste( dest_, folder_[GLOBAL_$PHOENIX],  sep='' )
# path_METROQC  <-  paste( dest_, folder_[GLOBAL_$METROQC],  sep='' )
# path_OCTRANS  <-  paste( dest_, folder_[GLOBAL_$OCTRANS],  sep='' )
# path_REM      <-  paste( dest_, folder_[GLOBAL_$REM],  sep='' )

path_ <- c( paste( dest_, folder_[GLOBAL_$PHOENIX],  sep='' ),
            paste( dest_, folder_[GLOBAL_$METROQC],  sep='' ),
            paste( dest_, folder_[GLOBAL_$OCTRANS],  sep='' ), 
            paste( dest_, folder_[GLOBAL_$REM],  sep='' ),
            paste( dest_, folder_[GLOBAL_$BLEUPANIER],  sep='' )
           )
pattern_ <-  NULL 
tweets.data <- NULL

#=== change based on the prOject ===
#index <- GLOBAL_$PHOENIX 
#index <- GLOBAL_$OCTRANS
#index <- GLOBAL_$METROQC
#index <- GLOBAL_$REM
index <- GLOBAL_$BLEUPANIER

tweets.data <- get.project.files  ( path_[index] , pattern_ )

tweets.projects <-  tweets.data %>% select(screen_name, text) 

#tweets.projects <-   filter(tweets.projects , grepl('O-Train',text)) #o-train only
#tweets.projects <-   filter(tweets.projects , grepl('MetroQuebec',screen_name)) #Metro QUebec only

tweets.projects <-   filter(tweets.projects , !grepl('epanierbleu_qc',screen_name)) #Panier bleu
tweets.projects <- tweets.projects[!duplicated(tweets.projects$text)]

tweets.projects$stripped <- clean.data (tweets.projects$text)

false_positive <- 'XXXXXXX'
compute_sentiments_bing ( tweets.projects$stripped , project.name[index], false_positive)
#false_positive <- '(???|???)' #panier bleu

#false_positive <- '(properly|rich|correctly|top|progress|fair)' #Phoenix
#false_positive <- '(patience|enjoy|safe|correct|accessible)' #o-train
#false_positive <- '(rapport|optimal|humour|wow|nice|excellent|champ)' #Metro train
#false_positive <- '(saint|regard|support|groundbreaking|jealous|eyesore|boring|chilly|toll|nice)' #REM

compute_sentiments_bing ( tweets.projects$stripped , project.name[index], false_positive)
compute_sentiments_affin ( tweets.projects$stripped , project.name[index], false_positive)
compute_sentiments_nrc ( tweets.projects$stripped , project.name[index], false_positive)


# joy <-  get_sentiments("nrc") %>% filter(sentiment == "joy")
# 
# tweets.projects$word <- tweets.projects$stripped 
# tweets.projects %>%
#          inner_join(joy)  %>%
#          count(word, sort = TRUE)
 
# get_sentiments("nrc") %>%
#         filter(!is.na(sentiment)) %>%
#         inner_join(joy)  %>%
#         count(sentiment, sort = TRUE)

#compute_sentiments ( tweets.projects , project.name[index])


t<- filter(tweets.projects , grepl('passe',stripped))
View(t)
