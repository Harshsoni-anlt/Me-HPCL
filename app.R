library(shiny)
library(shinyLP)
library(shinythemes)
library(DT)

require(tidyverse)
require(readxl)
library(dplyr)
library(tm)
library(lsa)


# Define UI for application
ui=shinyUI(
    
    fluidPage(
        
        list(tags$head(HTML('<link rel="icon", href="logo.png",
                        type="image/png" />'))),
        div(style="padding: 1px 0px; width: '100%'",
            titlePanel(
                title="", windowTitle="Me@HPCL Search Engine"
            )
        ),
        
        navbarPage(title=div( "Me@HPCL Search Engine"),
                   inverse = F, # for diff color view
                   theme = shinytheme("united"),
                   
                   tabPanel("About App", icon = icon("home"),
                            fluidRow(
                                column(6, panel_div(class_type = "primary", panel_title = "About",
                                                    content = HTML("Me@HPCL is a visioning tool to gain deeper insights into the needs of the people regarding their careers and developmental interventions for employee’s overall development. 
                                                             <br/>
                                                             This search engine will search for any keywords and match the Me@HPCL database and return a list of employees who have mentioned similar keywords in their Me@HPCL document..
                                                             "))),
                                column(6, panel_div("primary", " Data Template",
                                                    HTML("Download Data Upload Template <a href='https://drive.google.com/uc?export=download&id=17B01IfJ0n0MU_WtBvZnFDe0doWYPEpc5' target='_top'>(click me)</a>")))
                            )  
                            ,
                            
                            
                            
                            
                            fluidRow(
                                column(6, panel_div(class_type = "primary", panel_title = "Directions",
                                                    content = HTML("
                                                             
                                                             
Steps to use Me@HPCL:- 
<br/>
1.) Download data table Template from the Data Template box.
<br/>
2.)Fill data as per column available. 
<br/>
3.)Save data file as (.xlsx). 
<br/>
4.)Upload data file into (Upload Data File) tab. 
<br/>
5.)Combine Multiple columns Using the checkbox and select button. 
<br/>
6.)Use the search box to search the term. 
<br/>
7.)Use slider to adjust the score. 
<br/>
8.)Download CSV file using Download button."))),
                                
                                
                            )  ) # end of fluidRow
                   ,
                   tabPanel("Upload Data File", icon = icon("table"),
                            
                            fileInput("file1", "Choose excel File", accept = ".xlsx"),
                            
                            
                            fluidPage(
                                titlePanel("Original Data Set")
                                # tabItem("data",
                                #         tableOutput("contents")) 
                            ),
                            
                            DT::dataTableOutput("contents")
                            
                            
                   ),
                   
                   tabPanel("Word Search", icon = icon("search"),
                            fluidRow(
                                
                                sidebarPanel(
                                    
                                    
                                    
                                    uiOutput("checkbox"),
                                    actionButton("do", "Select",icon = icon("search")),
                                    textInput("search_terms", label="Search Terms",value="Time"),
                                    
                                    
                                    
                                    
                                    # Copy the line below to make a slider range 
                                    sliderInput("slider2", label = h3("Slider Range"), min = 0, 
                                                max = 1, value = c(.1, .9))
                                    ,
                                    downloadButton('downloadData', 'Download'),
                                    tags$br(),
                                    tags$br(),
                                    tags$br(),
                                    
                                    p("Made For",a("HPCL",
                                                   href = "https://www.hindustanpetroleum.com/"
                                    ), ".")
                                    
                                    
                                )
                            ),
                            
                            DT::dataTableOutput("fun")
                   )
                   
        ) # end of fluid page
    )) # end of shiny





# Define server logic
server<-function(input,output){
    
    
    
    ####################################Tab 1 data############################################################
    
    raw <- reactive({
        file <- input$file1
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "xlsx", "Please upload a excel file"))
        
        df <- read_excel(file$datapath)
        df
        
        
    })
    
    output$contents<- renderDT({
        DT::datatable(
            raw(),
            filter = 'top', extensions = c('Buttons', 'Scroller'),
            options = list(scrollY = 650,
                           scrollX = 500,
                           deferRender = TRUE,
                           scroller = TRUE,
                           # paging = TRUE,
                           #pageLength = 25,
                           buttons = list('excel',
                                          list(extend = 'colvis', targets = 0, visible = FALSE)),
                           dom = 'lBfrtip',
                           fixedColumns = TRUE), 
            rownames = FALSE)
        
    })
    ####################################Tab 2 data############################################################
    
    
    df0=reactive({
        dff <- subset(raw(), select = -c(1,2,3,4))
        d=as.data.frame(dff)
        return(d)
    })
    
    
    output$checkbox <- renderUI({
        checkboxGroupInput(inputId = "select_var", 
                           label = "Select variables", 
                           choices = names(df0())
        )
    })
    
    data <- eventReactive(input$do, {
        
        # Select columns to print ----
        df_sel <- {
            req(input$select_var)
            df_s <- df0() %>% select(input$select_var)
            df_sel=as.data.frame(df_s)
            dataf <-  df_sel %>% 
                unite(expectation, input$select_var, sep = " ", remove = TRUE)
            return(dataf)
            
        }
        
        data2=reactive({
            data <- data.frame(Name=raw()$Name,df_sel)
            data2=as.data.frame(data)
            return(data2)
        })
        return(data2())
        
    })
    
    
    finaldftouse=reactive({
        data <- data.frame(data())
    })
    
    #2nd par
    query <- reactive({
        return(input$search_terms)
    })
    
    
    
    
    df=reactive(
        {
            df=as.data.frame(finaldftouse())
            
            return(df)
            
        })
    
    docList=reactive(
        {
            docList <- as.list(df()$expectation)
            return(docList)})
    
    N.docs=reactive(
        {
            N.docs <- length(df()$expectation)
            return(N.docs)})
    
    my.corpus=reactive({
        
        query <- query()
        
        {
            
            
            my.docs <- VectorSource(c(docList(), query))
            my.docs$Names <- c(names(docList()), "query")
            
            my.corpus <- Corpus(my.docs)
            my.corpus <- tm_map(my.corpus, removeWords, stopwords("en"))
            my.corpus <- tm_map(my.corpus, removePunctuation)
            my.corpus <- tm_map(my.corpus, stripWhitespace)
            my.corpus <- tm_map(my.corpus, removeNumbers)
            my.corpus <- tm_map(my.corpus, removeWords, c("please","need","mail",
                                                          "email","unable","NA","h","prb","_NA_"))
            my.corpus<- tm_map(my.corpus, stemDocument)
            
            
            
            
            
            
            ## Create Term Docuemnt Matrix
            dataframe_DF<-data.frame(text=unlist(sapply(my.corpus,`[`, "content")), stringsAsFactors=F)
            
            # Transform term document matrix into a dataframe
            term.doc.matrix.desc <- TermDocumentMatrix(my.corpus)
            
            term.doc.matrix <- as.matrix(term.doc.matrix.desc)
            
            
            #--------------------------------# Feature Engineering Ends#-----------------------------------------------------
            
            
            #--------------------------------# Implement TF-IDF Weighting Starts#-----------------------------------------------------
            
            get.tf.idf.weights <- function(tf.vec, df) {
                # Computes tfidf weights from a term frequency vector and a document
                # frequency scalar
                weight = rep(0, length(tf.vec))
                weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.docs()/df)
                weight}
            
            
            get.weights.per.term.vec <- function(tfidf.row) {
                term.df <- sum(tfidf.row[1:N.docs()] > 0)
                tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
                return(tf.idf.vec)}
            
            
            tfidf.matrix <- t(apply(term.doc.matrix.desc, c(1), FUN = get.weights.per.term.vec))
            colnames(tfidf.matrix) <- colnames(term.doc.matrix.desc)
            
            tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
            
            query.vector <- tfidf.matrix[, (N.docs() + 1)]
            tfidf.matrix <- tfidf.matrix[, 1:N.docs()]
            
            doc.scores <- t(query.vector) %*% tfidf.matrix
            doc.scores <- round(doc.scores,digits = 2)
            
            results.df <- data.frame(Name=raw()$Name,Emp_No.=raw()$Emp_No.,text = unlist(docList()), score = t(doc.scores))
            
            results.df <- results.df[order(results.df$score, decreasing = TRUE), ]
            results.df=results.df[!rowSums(results.df[-2] == 0),]
            
            results.df=results.df[complete.cases(results.df), ]
            options(width = 6000)
            
            
            results.df=as.data.frame(results.df)
            
            
            
            
            
            return(results.df)
            
        }
    })
    
    result=reactive(
        {results.df2=as.data.frame(my.corpus())
        return(results.df2)
        })
    
    
    result2=reactive({
        result <- result()[result()$score >= input$slider2[1]&input$slider2[2]>=result()$score,]
        result
    })
    
    output$fun =  renderDT({
        
        result2()
    })
    
    
    
    
    
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(result2(), con)
        }
    )
    
}
shinyApp(ui,server)   
