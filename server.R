#Server

shinyServer(function(input, output, session){
  
  #file upload and prep ####
  file_upload<-reactive({ 
    req(input$file)
    file<-read.csv(input$file$datapath, header = TRUE)
    screenshot(scale = 3)
  })
  
  #Merge all free text fields in its own reactive object
  
  texts_merged<- reactive({
    file_df<-as.data.frame(file_upload())
    file_df$data<- removeNumbers(paste(file_df$History_Details, file_df$Findings_Gross, file_df$Histopathology_Results,
                         file_df$Microbiology_Results, file_df$Parasitology_Results, file_df$Clinical_Pathology_Results,
                         file_df$Diagnosis, file_df$Diagnosis_Provisional, file_df$Diagnosis_Morphological, 
                         file_df$Diagnosis_Comments)) 
   
     drop<- c('History_Details','Findings_Gross','Histopathology_Results','Microbiology_Results','Parasitology_Results',
             'Clinical_Pathology_Results','Clinical_Pathology_Notes','Diagnosis','Diagnosis_Provisional',
             'Diagnosis_Morphological','Diagnosis_Comments','Histopathology_Notes','Microbiology_Notes','Parasitology_Notes')
    
     file_df<-file_df[,!(names(file_df) %in% drop)]
  })
  
  #convert file to corpus object ####
  the_corpus<-reactive({
        file_c <- corpus(texts_merged(), 
                     text_field = "data",
                     docid_field = 'Accession_Number') 
 
        incorrect_format<-docvars(file_c) %>% filter (!is.na(as.numeric(Date_Sent)))
    incorrect_format$Date_Sent<-as.Date(as.numeric(incorrect_format$Date_Sent), origin ="1899-12-30")

    
    correct_format<-docvars(file_c) %>% filter (is.na(as.numeric(Date_Sent)))
      correct_format$Date_Sent<-as.Date(correct_format$Date_Sent, format = '%d/%m/%Y')
     
      merged<-rbind(incorrect_format,correct_format)
    docvars(file_c)<- merged
    
    docvars(file_c)<- docvars(file_c) %>% mutate(Month_Submitted =as.factor(month(Date_Sent)),
                                                 Year_Submitted = as.factor(year(Date_Sent)))
 
    file_c<-file_c %>% corpus_subset(!is.na(Date_Sent))   
    
    file_c
      })
  
  
  #DESCRIBE functions ####
  
  #Filters (Age, species, gender, date) ####
  
  #Filter by Time in date range ('daterange')
  
  output$daterange<-renderUI({ dateRangeInput('daterange', format = 'dd/mm/yyyy',
                                              label = 'Date range input: dd-mm-yyyy',
                                              start = min(docvars(the_corpus())$Date_Sent),
                                              end = max(docvars(the_corpus())$Date_Sent)
  )})
  
  filtered_by_date<-reactive({
    date_filtered<-corpus_subset(the_corpus(), Date_Sent>= input$daterange[1] & Date_Sent <= input$daterange[2])
  })
  
  
  #filter by Animal_Age_Classification 
  output$ageselect<-renderUI({ pickerInput("ageselect", "Select Age Class", 
                                           choices = levels(as.factor(docvars(filtered_by_date())$Animal_Age_Classification)), 
                                           options = list('actions-box'=TRUE), multiple= TRUE,
                                           selected = levels(as.factor(docvars(filtered_by_date())$Animal_Age_Classification)))
  })
  
  #filter by Animal_Sex_Classification
  output$genderselect<-renderUI({ pickerInput("genderselect", "Select Sex", 
                                              choices = levels(as.factor(docvars(filtered_by_date())$Animal_Sex_Classification)), 
                                              options = list('actions-box'=TRUE), multiple= TRUE,
                                              selected = levels(as.factor(docvars(filtered_by_date())$Animal_Sex_Classification)))
  })
  
  #filter by species ('Animal_Breed' in the .csv output)
  output$speciesselect<-renderUI({ pickerInput("speciesselect", "Select Species", 
                                               choices = levels(as.factor(docvars(filtered_by_date())$Animal_Breed)), 
                                               options = list('actions-box'=TRUE), multiple= TRUE,
                                               selected = levels(as.factor(docvars(filtered_by_date())$Animal_Breed)))
  })
  
  filtered_corpus<- reactive({corpus_subset(filtered_by_date(),
                                            Animal_Age_Classification %in% input$ageselect & 
                                              Animal_Sex_Classification %in% input$genderselect & 
                                              Animal_Breed %in% input$speciesselect) 
    })
  
  #Visuaisations for Describe tab ####
  
  output$speciesplot<-renderPlot({
 ggplot(docvars(filtered_corpus()),aes(Animal_Breed)) +
      geom_bar(fill='steelblue')+
      xlab('Species')+  
      ylab('Number')+
      theme_economist()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      })

  
  output$speciestable<-renderTable({speciestable<-table(docvars(filtered_corpus())$Animal_Breed, 
                                          dnn = 'Species')
    speciestable<-as.data.frame(speciestable) %>% 
      mutate(Percent=round((Freq/sum(Freq))*100, digits = 2))
    
    return (speciestable)
        })
  
  
  output$ageplot<-renderPlot({
    ggplot(docvars(filtered_corpus()),aes(Animal_Age_Classification)) +
      geom_bar(fill='steelblue')+
      theme(axis.text.x = element_text(angle = 50, vjust = 0.6, size=15))+
      xlab('Age Class')+
      ylab('Number')+
      theme_economist()})
  
  output$agetable<-renderTable({agetable<-table(docvars(filtered_corpus())$Animal_Age_Classification,
                                      dnn = 'Age_class') 
  agetable<-as.data.frame(agetable) %>% 
    mutate(Percent=round((Freq/sum(Freq))*100, digits = 2))

  return (agetable)
        })
  
  output$sexplot<-renderPlot({
    ggplot(docvars(filtered_corpus()),aes(Animal_Sex_Classification)) +
      geom_bar(fill='steelblue')+
      theme(axis.text.x = element_text(angle = 50, vjust = 0.6, size=15))+
      xlab('Gender')+
      ylab('Number')+
      theme_economist()})
  
  output$sextable<-renderTable({sextable<- table(docvars(filtered_corpus())$Animal_Sex_Classification,
                                      dnn = 'Gender')
    sextable<-as.data.frame(sextable) %>% 
      mutate(Percent=round((Freq/sum(Freq))*100, digits = 2))
    
    return (sextable)
        })
  
  #Submissions per month/year visualisation and table
  
  output$timeplot<-renderPlot({
    ggplot(docvars(filtered_corpus()),aes_string(input$time)) +
      geom_bar(fill='steelblue')+
      xlab(input$time)+
      ylab('Number')+
      theme_economist()+
      theme(axis.text.x = element_text(angle = 50, vjust = 0.6, size=15))
    })
  
  output$timetable<-renderTable({timetable<- table(docvars(filtered_corpus())[input$time],
                                       dnn = input$time)
    timetable<-as.data.frame(timetable) %>% 
      mutate(Percent=round((Freq/sum(Freq))*100, digits = 2))
    
    return (timetable)
        })
  
  #Generate custom table ####
 
  categories<-c('Type','Status','Reviewed','Has_Attachments','Gross_Images_Stored','Fixed_Specimens_Stored','Submitter',
                'Organisation','City','Animal_Number_Submitted','Animal_Number_Risk','Animal_Number_Affected',
                'Animal_Number_Dead','Animal_Location','Animal_Location_Type','Animal_Location_Details','Animal_Breed',
                'Animal_Species','Animal_Species_Common_Name','Animal_Sex_Classification','Animal_Age_Classification',
                'Animal_Age_Details','Pathologists','Month_Submitted','Year_Submitted')
  
  output$frequency<-renderUI({ pickerInput("frequency", "Select frequency variable", 
                                           choices = levels(as.factor(categories)))})
  
  output$proportional<-renderUI({ pickerInput("proportional", "Select proportion variable", 
                                              choices = levels(as.factor(categories)))})
  
  #Generate custom visualisation - freq of X, proportional by Y
  output$customplot <- renderPlot({
    ggplot(docvars(filtered_corpus()),aes_string(input$frequency))+
      geom_bar(aes_string(fill=input$proportional),position = 'stack') +
      theme_economist()+
      theme(axis.text.x = element_text(angle = 50, vjust = 0.6, size=15))
  }) # Need to make x-axis labels more readable when multiple factors Plotly????
  
  custable<-reactive({ 
    req(!is.null(input$frequency))
    req(!is.null(input$proportional))                  
      if(input$frequency != input$proportional){
    table<-data.frame(table(c(docvars(filtered_corpus())[input$frequency],
                       docvars(filtered_corpus())[input$proportional]))) %>%
      reshape(idvar = input$frequency, timevar = input$proportional, direction ='wide') %>%
      mutate(total=rowSums(select_if(.,is.numeric))) %>%
      adorn_totals('row')}
    else {table(docvars(filtered_corpus())[input$frequency])}
    })
  
  output$customtable<-renderTable({custable()})
  
  #Make table downloadable as csv if people want to do further analysis
  output$customdata<-downloadHandler( filename = function(){"DEEcustomdata.csv"}, 
                                      content = function(fname){
                                        write.csv(custable(), fname)
                                      } )
  
  #EXPLORE functions ####
  #Word-corrs graph ####
  #render ggraph to allow overview (by word-correlations) of uploaded corpus
  word_correlations<- reactive ({ 
    data(stop_words)
   filtered_df <- data.frame(text=unlist(sapply(filtered_corpus(), '[')), 
                             Accession_Number = docnames(filtered_corpus()),
                                        stringsAsFactors=F, row.names = NULL) 
   tidy_words<-unnest_tokens(filtered_df,word,text,'words')
   tidy_words<-tidy_words %>% anti_join(stop_words) 
   word_cors <- tidy_words %>%
      group_by(word) %>%   
      filter(n()>(length(unique(tidy_words$Accession_Number))*0.05))%>% 
    #Words that appear fewer times than 5% of the total number of records uploaded are filtered out  
      pairwise_cor(word, Accession_Number, sort = TRUE)
    return(word_cors)
     })
#convert to igraph to then pass to plotly
  output$ggraph <-renderPlot({ 
    word_correlations() %>%
      filter(correlation >=input$corr) %>%  #use intermediate variable to split 
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()
  })
  
  
  #LDA for topic modelling wordcloud ####
  # Fit the LDA
  topic_model<-reactive({
    req(filtered_corpus())
    k_dfm <-  tokens(filtered_corpus(),remove_punct = TRUE,verbose = FALSE,
                     remove_symbols = TRUE, remove_numbers = TRUE) %>%
      quanteda::dfm() %>% dfm_remove(stopwords())
    rowTotals <- apply(k_dfm , 1, sum) #Find the sum of words in each Document
    k_dfm   <- k_dfm[rowTotals> 0, ]           #remove all docs without words
    k_lda <- LDA(k_dfm, k=input$k, control = list(seed=1234))
    #calculate beta per word
    k_topics <- tidy(k_lda, matrix = "beta")
  })
  #Extract top_n terms per topic
  k_top_terms <- reactive({topic_model() %>%
      group_by(topic) %>%
      top_n(input$top_n, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
  })
  #create wordlcloud of top_n words per topic
  output$wordcloud<-renderPlot({ 
    par(mar = rep(0, 4))
    k_top_terms() %>%
      mutate(topic = paste("topic", topic)) %>%
      acast(term ~ topic, value.var = "beta", fill = 0) %>%
      comparison.cloud(max.words = 500, rot.per = 0.35)
  })
  
  
  #EXAMINE functions ####
  
   #Create frequency ranked top TF:IDF datatable responsive to sliderinput ####
  tf_idf<-reactive({
    req(filtered_corpus())
      k_dfm <-tokens(filtered_corpus(),remove_punct = TRUE,verbose = FALSE, 
                     remove_symbols = TRUE, remove_numbers = TRUE) %>%
      quanteda::dfm() %>% dfm_remove(stopwords())
    tidy_k<-tidy(k_dfm)
    tidy_k<-tidy_k %>% rename(Accession_Number = document, word = term)
    tidy_k_tf_idf <- tidy_k %>%
      bind_tf_idf(word, Accession_Number, count) %>%
      arrange(desc(tf_idf))
    k_reports_summaries<-tidy_k_tf_idf %>%     
      group_by(Accession_Number) %>%  
      slice_max(tf_idf, n=input$slice_max) 
    k_important_words <-  k_reports_summaries %>%  
      ungroup() %>%
      count(word, sort= TRUE )
  })
  
  #output selectable table widget of frequency ranked top TF:IDF words 
  output$table <- DT::renderDataTable({tf_idf()})
  
  observeEvent(input$resetSelection,{
  output$table <- DT::renderDataTable({tf_idf()})
  })
  
  #Locate words selected from ^ in corpus and pass to kwic() for phrase-limited-rule-based-negation-detection
  
  #Create reactive object to store selected rows
  selected_rows<- reactive ({tf_idf()[input$table_rows_selected,]})
  
  #Create reactive object to hold results dataframe of negation detection
  negpattern<- c("none","never","neither","not", "without", "no", "can't", "don't","won't", 'negative') # add 'nor', 'isn't', 'didn't', 'negative'
  puncts<-c('.*\\.' ,  '.*/' , '.*:' , '.*;' , '.*!') # consider including only fullstops.... 
  find_negs<-function(y,z){grep(paste(negpattern, collapse = "|"), x=paste(y,z), ignore.case = TRUE)}

    negation_detection<-reactive({
        # Word identification
        k_kwic_pattern<- tokens(filtered_corpus()) %>%
      kwic(pattern = selected_rows()$word , window = 10, valuetype = 'fixed')
        # Punctuation identification in pre  
     k_kwic_pattern$pre<- k_kwic_pattern$pre %>% 
       gsub(pattern = paste(puncts, collapse = "|"), replacement = "")
        # Punctuation identification in post 
      k_kwic_pattern$post<-k_kwic_pattern$post %>%
       gsub(pattern = paste(puncts, collapse = "|"), replacement = "")
      #Can these lines be condensed?
        #negation detection
          k_kwic_pattern<-k_kwic_pattern[as.integer(
      if((length(find_negs(k_kwic_pattern$pre,k_kwic_pattern$post)))<1){1:nrow(k_kwic_pattern)}      #Try and separate these out to intermediate variable
      else{-as.integer((find_negs(k_kwic_pattern$pre,k_kwic_pattern$post)))}),]
         return(k_kwic_pattern)
        
     })
  unique_docs<-reactive({length(unique(negation_detection()$docname))})     
  
  
  #filter original upload by accession numbers of selected diagnoses for download  
  download_csv<-reactive({as.data.frame(file_upload()) %>% 
      filter(Accession_Number %in% negation_detection()$docname)})
  
  #print total number of 'cases' across all selected words  
  output$num<-renderPrint({ 
    req(diagnosis_df())
    print(unique_docs())
  })
  
  
  #Download button - download all records for selected diagnoses
  output$dldata<-downloadHandler(  filename = function(){"DEEDiscovery.csv"},  
                                   content = function(fname){
                                     write.csv(download_csv(), fname)
                                   } )
  
  #Apply phrase-limited-rule-based-negation-detection on each word selected back through the corpus,
  # and collate unique document ID's to identify number of 'cases'
  diagnosis_df<-reactive({ 
    validate(need(nrow(selected_rows())  > 0, "Please select words from the Table of Important Words"))
    if(nrow((selected_rows())) > 0) {
    out<-transform((selected_rows()), 
              n = sapply(selected_rows()$word, function(x, negation){
                kwic<-filter(negation, pattern == x) 
                length(unique(kwic$docname))
              },negation_detection() ))
    return(out)
    }})
  
  #create new table of words and their number of 'cases' 
  output$seltable<- DT::renderDataTable( diagnosis_df(), selection = 'multiple') 
  
  #plot seltable ^
  output$plot <-renderPlot({
    req(diagnosis_df())
    diagnosis_df() %>% 
      ggplot(aes(word,n))+
      geom_bar(stat='identity')+
      theme_economist() +
      ggtitle('Diagnoses Comparison')
  }) 

})