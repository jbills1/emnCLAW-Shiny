options(shiny.maxRequestSize=100*1024^2) #max input file size is now 100 MB

function(input, output, session) {

  #this stores the uploaded data
  uploadedData <- eventReactive(input$go, {
    req(input$file1)
    dget(input$file1$datapath)
  })

  #this converts the words into a format readable by text2vec
  wordsForProcessing <- eventReactive(input$go, {
      arg1 <- uploadedData()
      words <- arg1$processedWords[complete.cases(arg1$processedWords$Word),]
      new_df <- data.frame(dplyr::select(words,Word,Statement))
      new_df <- aggregate(Word~Statement,data=new_df,c)
      new_df$Statement <- NULL #data frane with a list of character vectors
      new_df$Word
  })

  #text2vec portion, creating word vectors
  text2vecReac <- eventReactive(c(input$min_term_input, input$skipgram), {

      STRING<-wordsForProcessing()
      vocab <- create_vocabulary(itoken(STRING))
      vocab <- prune_vocabulary(vocab, term_count_min = input$min_term_input) #min_term_count of 5 was suggested, but 10 seems to work better
      it <- itoken(STRING)
      vectorizer <- vocab_vectorizer(vocab,
                                     # don't vectorize input
                                     grow_dtm = FALSE,
                                     # use window of 5 for context words
                                     skip_grams_window = input$skipgram) #
      tcm <- create_tcm(it, vectorizer)
      set.seed(10) #this doesn't really do anything, there results still differ every time
      fit <- glove(tcm = tcm,
                   word_vectors_size = 50,
                   x_max = 10, learning_rate = 0.2,
                   num_iters = 15)
      word_vectors <- fit$word_vectors[[1]] + fit$word_vectors[[2]]
      rownames(word_vectors) <- rownames(tcm) #creates word vector matrix
      word_vectors
    })

  #tsne portion, entropy reduction step, this creates the X and Y coordinates
  tsneReac<-eventReactive(input$perplexity, {
      set.seed(10) #this is necessary for reproducible results at this step
      rtsne_out <- Rtsne(unique(as.matrix(text2vecReac())),perplexity=input$perplexity) #removing duplicates is required
      coords <- rtsne_out$Y
      coords #the coordinates
  })

  #DBSCAN portion, creates the list of clusters
  clusterListPrePlyr <- eventReactive(input$go, {
    ds <- dbscan(tsneReac(), eps = input$eps, minPts = input$minpts) #i've set the default eps at 1.2 and default minpts at 3

    #this for loop lists all of the clusters except the first two. the first seemed too big to be meaningful
    cluster_list <- list()
    for (i in 2:max(ds$cluster)){
      cluster_list[[i]] <- rownames(text2vecReac())[ds$cluster==i]
    }

    cluster_list

  })

  #this step makes all of the output rows the same length
  dbscanReac <- eventReactive(input$go, {

    plyr::ldply(clusterListPrePlyr(), rbind)

  })

  #this step outputs the clusters
  output$concepts <- DT::renderDataTable({
    #dfx <<- dbscanReac() #double headed arrow will put df in global environment
    dbscanReac()
  },server=FALSE, selection = 'single')

  #this step selects sentences containing the words in a cluster that a user clicks
  output$search <- DT::renderDataTable({

   s <- input$concepts_rows_selected
   rowsSelected <- dbscanReac()[s,]

   #this step selects the words from the row that the user clicks
   convertToChar <- paste(do.call(c,lapply(rowsSelected,as.character)),collapse=",")
   convertToChar <- unlist(strsplit(convertToChar, split=","))

   #below here, i've made it so that if an ID and filter are selected in sprout, it should appear in the output
   table_a <- uploadedData()$rawSentences
   table_b <- uploadedData()$processedWords
   sid = table_b %>% filter(Word %in% convertToChar) %>% select(Sentence) %>% distinct() %>% .[[1]] #select indeces of all sentences

  #this is probably a lot longer than it should be. sorry to anyone who has to go through this.
   if(!is.na(table_b$id[1])==TRUE & !is.na(table_b$filter[1])==TRUE){

     table_c<-select(table_b,Statement,id,filter) #this could be an issue because it only checks the first element and not all of them
     tojoin<-table_c[!duplicated(table_c), ]
     finale<-inner_join(table_a,tojoin,'Statement') %>% dplyr::rename(Filter=filter,ID=id)
     sid = table_b %>% filter(Word %in% convertToChar) %>% select(Sentence) %>% distinct() %>% .[[1]] #select indeces of all sentences
     df_1 <- finale %>% filter(SentenceID %in% sid) %>% select(Sentence,ID,Filter)
     words2highlight = uploadedData()$wordMap %>% filter(Word %in% convertToChar) %>% .[[2]] %>% paste(.,collapse="|") %>% paste0("(",.,")")
     mutate(df_1,Matches=str_count(df_1$Sentence, words2highlight)) %>% arrange(desc(Matches)) %>% mutate(Sentence = gsub(words2highlight,replacement=HTML("<span style='background-color: Yellow'>\\1</span>"),Sentence))

   } else if(!is.na(table_b$id[1])==TRUE & !is.na(table_b$filter[1])==FALSE) { #this could be an issue because it only checks the first element and not all of them

     table_c <- select(table_b,Statement,id)
     tojoin <- table_c[!duplicated(table_c), ]
     finale <- inner_join(table_a,tojoin,'Statement') %>% dplyr::rename(ID=id)
     sid <- table_b %>% filter(Word %in% convertToChar) %>% select(Sentence) %>% distinct() %>% .[[1]] #select indeces of all sentences
     df_1 <- finale %>% filter(SentenceID %in% sid) %>% select(Sentence,ID)
     words2highlight <- uploadedData()$wordMap %>% filter(Word %in% convertToChar) %>% .[[2]] %>% paste(.,collapse="|") %>% paste0("(",.,")")
     mutate(df_1,Matches=str_count(df_1$Sentence, words2highlight)) %>% arrange(desc(Matches)) %>% mutate(Sentence = gsub(words2highlight,replacement=HTML("<span style='background-color: Yellow'>\\1</span>"),Sentence))

   } else

     df_1 <- uploadedData()$rawSentences %>% filter(SentenceID %in% sid) %>% select(Sentence)
     words2highlight = uploadedData()$wordMap %>% filter(Word %in% convertToChar) %>% .[[2]] %>% paste(.,collapse="|") %>% paste0("(",.,")")
     #uploadedData()$rawSentences %>% filter(SentenceID %in% sid) %>% select(Sentence) %>% mutate(Sentence = gsub(words2highlight,replacement=HTML("<span style='background-color: Yellow'>\\1</span>"),Sentence))
     #mutate(df_1,Matches=str_count(df_1$Sentence, words2highlight)) %>% arrange(desc(Matches)) %>% mutate(Sentence = gsub(words2highlight,replacement=HTML("<span style='background-color: Yellow'>\\1</span>"),Sentence))
     #the one above her is only matches, not proportion
     mutate(df_1,Matches=str_count(df_1$Sentence, words2highlight),TotalWords = sapply(gregexpr("\\W+", df_1$Sentence), length), Proportion = str_count(df_1$Sentence, words2highlight)/as.numeric(sapply(gregexpr("\\W+", df_1$Sentence), length))) %>% arrange(desc(Matches)) %>% mutate(Sentence = gsub(words2highlight,replacement=HTML("<span style='background-color: Yellow'>\\1</span>"),Sentence))

   #####################below here the entire document is displayed when a cluster is clicked (instead of just a sentence)
   # words <- uploadedData()$processedWords
   # sid = words %>% filter(Word %in% convertToChar) %>% select(Statement) %>% distinct() %>% .[[1]]
   # indeces1<-uploadedData()$rawSentences %>% filter(Statement %in% sid) %>% select(Statement) %>% distinct()
   # unique_indeces<-unique(indeces1$Statement) #gives statements where team and management are
   #
   #
   # list_for_storage<-list()
   # for(i in unique_indeces){
   #   list_for_storage[[i]]<-paste0(filter(uploadedData()$rawSentences,uploadedData()$rawSentences[['Statement']]==i)$Sentence,collapse = " ")
   # }
   #
   # rslt<-Filter(Negate(is.null), list_for_storage) %>% unlist() %>% data_frame()
   # # i havent gotten the highlighting figured out.
   # # colnames(rslt)[1] <- "Sentences"
   # # words2highlight = uploadedData()$wordMap %>% filter(Word %in% c('team','management')) %>% .[[2]] %>% paste(.,collapse="|") %>% paste0("(",.,")")
   # # mutate(rslt, Sentences=gsub(words2highlight,"<strong>\\1</strong>",rslt))


   # #below here is all the old stuff that works well! just in case something goes wrong with the above stuff.
   # words <- uploadedData()$processedWords ################dont delete this, delete everything below here
   # sid = words %>% filter(Word %in% convertToChar) %>% select(Sentence) %>% distinct() %>% .[[1]] #select indeces of all sentences
   # df_1 <- uploadedData()$rawSentences %>% filter(SentenceID %in% sid) %>% select(Sentence)
   # words2highlight = uploadedData()$wordMap %>% filter(Word %in% convertToChar) %>% .[[2]] %>% paste(.,collapse="|") %>% paste0("(",.,")")
   # #uploadedData()$rawSentences %>% filter(SentenceID %in% sid) %>% select(Sentence) %>% mutate(Sentence = gsub(words2highlight,replacement=HTML("<span style='background-color: Yellow'>\\1</span>"),Sentence))
   # mutate(df_1,Matches=str_count(df_1$Sentence, words2highlight)) %>% arrange(desc(Matches)) %>% mutate(Sentence = gsub(words2highlight,replacement=HTML("<span style='background-color: Yellow'>\\1</span>"),Sentence))
   # # #mutate(df_1,Proportion=str_count(df_1$Sentence, words2highlight)/sapply(gregexpr("\\W+", df_1$Sentence), length)) %>% arrange(desc(Proportion)) %>% mutate(Sentence = gsub(words2highlight,"<strong>\\1</strong>",Sentence)) #this one looks at proportion of matches per sentence length

    },escape=FALSE)

  #this step creates the TSNE plot and colors words that belong to the same cluster
  imageClick <- eventReactive(input$go, {

    df <- data.frame(text2vecReac())
    df["Cluster"] <- NA

    for (i in 1:length(clusterListPrePlyr())){
      arf <- c()
      arf <- row.names(df) %in% clusterListPrePlyr()[[i]] %>% which(TRUE)
      df[arf,ncol(df)] <- i
    }

    outfile <- tempfile(fileext='.jpeg')
    jpeg(outfile, width=1999, height=1400)
    plot(tsneReac(), t='n', main="BarnesHutSNE", xlab="",ylab="")
    text(tsneReac(), labels=rownames(text2vecReac()),col=df$Cluster)

    dev.off()


    list(
      src=outfile,
      contentType='image/jpeg')

  })

  #this step is for displaying the plot
  output$image <- renderImage({

    imageClick()

  },deleteFile=TRUE)

}
