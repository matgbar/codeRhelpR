#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# The most recent version of this program is available on GitHub
# If used in academic and professional products please cite: 

#------------------------------------------------------------------------------
#Will need to have these packages installed  
#Packages required will download if you do not have them
#Need to be conected to the Internet
#------------------------------------------------------------------------------

if(!require(xlsx)) install.packages("xlsx")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(shiny)) install.packages("shiny")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(shinyFiles)) install.packages("shinyFiles")
if(!require(caret)) install.packages("caret")
if(!require(psych)) install.packages("psych")
if(!require(e1071)) install.packages("e1071")
if(!require(cowplot)) install.packages("cowplot")
if(!require(RColorBrewer)) install.packages("RColorBrewer")

library(xlsx)
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyFiles)
library(caret)
library(psych)
library(cowplot)
library(RColorBrewer)

#------------------------------------------------------------------------------
#This information is specific to Turtle Project
#IDs may be useful 
#------------------------------------------------------------------------------

cohort_list <- list()
cohort_list[[1]] <- c("001","003","004","007","009")
cohort_list[[2]] <- c("012","015","016","017","018","020")
cohort_list[[3]] <- c("028","029","031","033","037","041")
cohort_list[[4]] <- c("027","042","046","049","054","055","056")
cohort_list[[5]] <- c("057","060","062","063","065","068")
cohort_list[[6]] <- c("073","075","076","077","078","086")
cohort_list[[7]] <- c("087","088","090","092","093","095","096")
cohort_list[[8]] <- c("102","103","106","107","109","111","112")
cohort_list[[9]] <- c("115","117","118","119","120","123")
cohort_list[[10]] <- c("130","131","132","134","135","140")
cohort_list[[11]] <- c("141","146","147","148","150","152","154")
cohort_list[[12]] <- c("157","163","164","166","169","170")
names(cohort_list) <- paste("Cohort", 1:12)

#------------------------------------------------------------------------------
#Definiting the UI (this is the graphic rendering )
# Define UI for application that draws a histogram
#------------------------------------------------------------------------------

ui <- shinyUI(
    fluidPage(theme = shinytheme("spacelab"),
              tags$head(
                  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
              ),
              titlePanel("codeR helpR: Open Source Reliability and Analysis for Behavioral Coding"),
              tabsetPanel(
                  tabPanel("Setup", 
                           wellPanel(
                               fluidRow( 
                                   column(4, 
                                          tags$b("File and Directory Setup:"),
                                          hr(),
                                          shinyDirButton(id='dir',
                                                         label='Select Directory',
                                                         title = 'Select Working Directory'),
                                          shinyFilesButton(id='fileIn', 
                                                           title = 'Select .xlsx file', 
                                                           label = 'Coding Data File', 
                                                           multiple = F), 
                                          shinyFilesButton(id='codeIn', 
                                                           title = 'Select .xlsx file', 
                                                           label = 'Coding Scheme File', 
                                                           multiple = F),
                                          tags$br(),
                                          tags$br(),
                                          actionButton("load", 
                                                       label = "Load File/Directory", 
                                                       style = "background: #0270b7; border-color: #0270b7; color: #ffffff"),
                                          tags$br(), 
                                          tags$br(),
                                          tags$b("Variable Setup:"),
                                          hr(),
                                          textInput("target_id", 
                                                    label = "Identify Target ID:", 
                                                    placeholder = "e.g., 001"),
                                          selectInput("behav_var", 
                                                      label = "Choose variable name with behavioral codes:", 
                                                      choices = "Select a data set first"), 
                                          selectInput("timing_var", #Need to make reactive...  
                                                      label = "Choose variable that contains time stamps:", 
                                                      choices = "Select a data set first"), 
                                          selectInput("coder_var", 
                                                      label = "Choose variable that differentiates coders:", 
                                                      choices = "Select a data set first"),
                                          textInput("coder_1", 
                                                    label = "Input coder 1 ID"),
                                          textInput("coder_2", 
                                                    label = "Input coder 2 ID"),
                                          selectInput("cohort", 
                                                      label = "Select the correct cohort for the target ID (defined above):", 
                                                      choices = paste("Cohort", 1:12))
                                          ), 
                                   column(2,
                                          tags$b("Coding Scheme Overview:"),
                                          hr(),
                                          tableOutput("code_dat")
                                          ),
                                   column(2, 
                                          tags$b("Coder 1 Data:"),
                                          hr(),
                                          tableOutput("tab_coder1"), 
                                          tags$b("Codes used by coder 1:"), 
                                          hr(), 
                                          tableOutput("used_coder1")
                                          ),
                                   column(2, 
                                          tags$b("Coder 2 Data:"),
                                          hr(),
                                          tableOutput("tab_coder2"), 
                                          tags$b("Codes used by coder 2:"), 
                                          hr(), 
                                          tableOutput("used_coder2")
                                          ),
                                   column(2, 
                                          numericInput("point_interval", 
                                                       label = "Point event tolerance (in seconds):", 
                                                       min = .01, 
                                                       value = .5),
                                          tags$br(),
                                          actionButton("process", 
                                                       label = "Compute\nReliabilities",
                                                       style = "background: #0270b7; border-color: #0270b7; color: #ffffff"),
                                          tags$br(), 
                                          tags$br(),
                                          actionButton("save", 
                                                        label = 'Save .RData',
                                                        style = "background: #0270b7; border-color: #0270b7; color: #ffffff")
                                          )
                                   )
                               )
                           ), 
                  tabPanel("Interactive Plotting", 
                           sidebarLayout(
                               sidebarPanel(width = 3,
                                   wellPanel(
                                       selectInput("behav_displ", 
                                                   label = "Choose behavior codes to display",
                                                   choices = c("")), 
                                       actionButton("plot_displ", 
                                                    label = "Display Summaries", 
                                                    style = "background: #0270b7; border-color: #0270b7; color: #ffffff"), 
                                       verbatimTextOutput("conf_matrix_txt")
                                       )
                                   ), 
                               mainPanel(width = 9,
                                         plotOutput('plot', 
                                                    height = '900px')
                                         )
                               )
                           )
                  )
              )
    )

###############################################################################
###############################################################################
###############################################################################
###############################################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    options(shiny.maxRequestSize=150*1024^2)
    
    #------------------------------------------------------------------------------
    #Setting up a user folder for the shiny buttons below 
    #------------------------------------------------------------------------------
    
    user.folder<-'C:/'
    
    if(Sys.getenv('USERPROFILE')=="")
        user.folder<-"~"
    
    if(Sys.getenv("HOMEPATH")!="")
        user.folder<-Sys.getenv('USERPROFILE')
    
    shinyDirChoose(input, 'dir', roots = c(User = user.folder), 
                   session = session)

    observeEvent(input$dir,{
        #browser()
        if(!is.null(input$dir)){
            rv$wd<-parseDirPath(roots=c(User=user.folder), input$dir)
            shinyFileChoose(input, 'fileIn', roots=c(wd=rv$wd, User=user.folder))
            shinyFileChoose(input, 'codeIn', roots=c(wd=rv$wd, User=user.folder))
        }
        else{
            shinyFileChoose(input, 'fileIn', roots=c(User=user.folder))
            shinyFileChoose(input, 'codeIn', roots=c(User=user.folder))
        }
    })
    
    rv<-reactiveValues(
        dat1 = NULL,
        code_dat = NULL,
        duration_codes = NULL, 
        event_codes = NULL,
        family_codes = NULL, 
        coder_1 = NULL, 
        coder_2 = NULL, 
        tab_coder1 = NULL, 
        tab_coder2 = NULL
    )
    
    observeEvent(input$load, {
        #browser()
        file_name_tmp <- parseFilePaths(roots = c(wd=rv$wd, User=user.folder), input$fileIn)
        
        code_name_tmp <- parseFilePaths(roots = c(wd=rv$wd, User=user.folder), input$codeIn)
        
        if(grepl(".xls", file_name_tmp$datapath)){
            rv$dat1 <- read.xlsx(file_name_tmp$datapath, 
                                 sheetIndex = 1,
                                 header = TRUE, 
                                 as.data.frame = TRUE,
                                 colClasses = NA)
        }
        
        if(grepl(".csv", file_name_tmp$datapath)){
            rv$dat1 <- read.csv(file_name_tmp$datapath, 
                                header = TRUE)
        }
        
        if(grepl(".txt", file_name_tmp$datapath)){
            rv$dat1 <- read.table(file_name_tmp$datapath, 
                                  header = TRUE, 
                                  sep = "\t")
        }
        
        if(grepl(".xls", code_name_tmp$datapath)){
            rv$code_dat <- read.xlsx(code_name_tmp$datapath, 
                                     sheetIndex = 1, 
                                     header = TRUE, 
                                     as.data.frame = TRUE, 
                                     colClasses = NA)
        }
        
        if(grepl(".csv", code_name_tmp$datapath)){
            rv$code_dat <- read.csv(code_name_tmp$datapath, 
                                    header = TRUE)
        }
        
        if(grepl(".txt", code_name_tmp$datapath)){
            rv$code_dat <- read.table(code_name_tmp$datapath, 
                                      header = TRUE, 
                                      sep = "\t")
        }
        
        if(!is.null(rv$dat1)){
            #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            #May want to come back and create logic that eliminates variables
            #Will require some added layers of structure and tracking 
            #Will be more user friendly 
            #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            updateSelectInput(session, 
                              inputId = "behav_var", 
                              choices = colnames(rv$dat1))
            updateSelectInput(session, 
                              inputId = "timing_var", 
                              choices = colnames(rv$dat1))
            updateSelectInput(session, 
                              inputId = "coder_var", 
                              choices = colnames(rv$dat1))
            updateSelectInput(session, 
                              inputId = "behav_displ", 
                              choices = rv$code_dat$Codes)
        }
        
        else{
            showModal(modalDialog(title = "Error", 
                                  "Message: Check your file formats. Program accepts:\n\t Excel files \n\t CSV files \n\t Tab-delimited text files"))
        }
    })
    
    observeEvent(input$process, {
        
        showModal(modalDialog(title = "Processing Data", 
                              "Calculating agreement and kappas. May take a few moments."))
        #browser()
        
        #----------------------------------------------------------------------
        #Coverting imported variables, setting up data environment
        #----------------------------------------------------------------------
            
        rv$duration_codes <- unlist(as.character(rv$code_dat$Codes[rv$code_dat$Type == "Duration"]))
        rv$event_codes <- unlist(as.character(rv$code_dat$Codes[rv$code_dat$Type == "Event"]))
        rv$family_codes <- unlist(as.character(rv$code_dat$Codes[rv$code_dat$Family == "Y"]))
        
        all.codes <- c(rv$duration_codes, 
                       rv$event_codes)
        
        rv$coder_1 <- input$coder_1
        rv$coder_2 <- input$coder_2
        
        #----------------------------------------------------------------------
        #Creating a set of warnings - will help ensure fewer program crashes
        #These constraints should be set mainly on inputs
        #----------------------------------------------------------------------
        
        if(length(all.codes) == 0){
            showModal(modalDialog(title = "Error", 
                                  "Message: There is a problem with your coding file. Please check the file is formatted correctly."))
        }
        
        else if(is.null(rv$coder_1)){
            showModal(modalDialog(title = "Error", 
                                  "Message: There is a problem with your inputs. Please input a coder 1 ID value."))
        }
        
        else if(is.null(rv$coder_2)){
            showModal(modalDialog(title = "Error", 
                                  "Message: There is a problem with your inputs. Please input a coder 2 ID value."))
        }
        
        else if(!is.null(rv$coder_1) & 
                sum(grepl(rv$coder_1,
                          as.character(unlist(rv$dat1[input$coder_var])))) == 0){
            showModal(modalDialog(title = "Error", 
                                  "Message: There is a problem with your inputs. Your coder 1 ID does not appear to match any rows in the input data."))
            
        }
        
        else if(!is.null(rv$coder_2) & 
                sum(grepl(rv$coder_2,
                          as.character(unlist(rv$dat1[input$coder_var])))) == 0){
            showModal(modalDialog(title = "Error", 
                                  "Message: There is a problem with your inputs. Your coder 2 ID does not appear to match any rows in the input data."))
            
        }
        
        else if(!is.numeric(rv$dat1[1,input$timing_var])){
            showModal(modalDialog(title = "Error", 
                                  "Message: There is a problem with your timing variable. It is not numeric. Double check your coded data."))
        }
        
        else if(sum(grepl(paste0("[", paste(all.codes, collapse = ","), "]"),
                          as.character(unlist(rv$dat1[input$behav_var])))) == 0){
            showModal(modalDialog(title = "Error", 
                                  "Message: There may be a problem with your behavior variable and/or your coding file. None of the codes were found in the data set. Check your inputs carefully."))
        }
        
        else if(sum(cohort_list[[which(names(cohort_list) == input$cohort)]] == input$target_id) == 0){
            showModal(modalDialog(title = "Error", 
                                  "Message: There is a problem with your cohort selection and/or case ID. The case ID does not appear in the selected cohort set."))
        }
        
        else{
            rv$time.min <- min(rv$dat1[input$timing_var], na.rm = TRUE)
            rv$time.max <- max(rv$dat1[input$timing_var], na.rm = TRUE)
            
            raw.code.list<-list()
            fin.code.list<-list()
            IDs <- cohort_list[[which(names(cohort_list) == input$cohort)]]
            for(i in 1:length(all.codes)){
                
                if(sum(rv$family_codes==all.codes[i])==1){
                    raw.code.list[[i]] <- paste0(all.codes[i], IDs[IDs != input$target_id])
                    fin.code.list[[i]] <- paste(input$target_id, 
                                                paste0(all.codes[i], 
                                                       IDs[IDs != input$target_id]), 
                                                sep = "_")
                    names(raw.code.list[[i]]) <- all.codes[i]
                    names(fin.code.list[[i]]) <- all.codes[i]
                }
                
                else{
                    raw.code.list[[i]] <- all.codes[i]
                    fin.code.list[[i]] <- all.codes[i]
                    
                    names(raw.code.list[[i]]) <- all.codes[i]
                    names(fin.code.list[[i]]) <- all.codes[i]
                }
            }
            
            rv$raw_code_list<-raw.code.list
            #Getting a time variable that starts at zero 
            #This assumes that the file and the codes should/could start at 0
            Time <- seq(0, round(max(rv$dat1[input$timing_var], 
                                     na.rm = TRUE), 
                                 digits = 2), 
                        by = .01)
            
            rv$dat1[input$timing_var] <- round(rv$dat1[input$timing_var], digits = 2)
            
            #Getting separate values by data set:
            coder1_rows <- grep(rv$coder_1, as.character(as.matrix(rv$dat1[input$coder_var], ncol=1)))
            coder2_rows <- grep(rv$coder_2, as.character(as.matrix(rv$dat1[input$coder_var], ncol=1)))
            
            #browser()
            dat.tmp <- data.frame(Coder = c(rep(rv$coder_1, length(coder1_rows)), 
                                            rep(rv$coder_2, length(coder2_rows))), 
                                  Behavior = c(unlist(as.character(rv$dat1[coder1_rows,
                                                                           input$behav_var])), 
                                               unlist(as.character(rv$dat1[coder2_rows, 
                                                                           input$behav_var]))), 
                                  Time = c(unlist(rv$dat1[coder1_rows,
                                                          input$timing_var]), 
                                           unlist(rv$dat1[coder2_rows, 
                                                          input$timing_var])),
                                  ID = input$target_id)
            
            rv$dat1 <- dat.tmp
            
            rv$dat1_coder.1 <- rv$dat1[coder1_rows,]
            rv$dat1_coder.2 <- rv$dat1[coder2_rows,]
            
            rv$agree_list <- list() 
            rv$kappa_list <- list()
            
            rv$DF_list <- list()
            rv$total_list <- list()
            rv$conf_matrix_list <- list()
            
            for(c in 1:length(raw.code.list)){
                #c<-1
                #browser()
                
                if(sum(grepl(all.codes[c], rv$dat1$Behavior)) == 0){
                    #browser()
                    
                    type_tmp <- as.character(rv$code_dat$Type[rv$code_dat$Codes == all.codes[c]])
                    
                    rv$DF_list[[c]] <- data.frame(Coder = c(rep(rv$coder_1, nrow(rv$dat1_coder.1)), 
                                                            rep(rv$coder_2, nrow(rv$dat1_coder.2))), 
                                                  Time = c(rv$dat1_coder.1$Time, 
                                                           rv$dat1_coder.2$Time),
                                                  Value = 0, 
                                                  Behavior = all.codes[c], 
                                                  Behavior_typ = type_tmp, 
                                                  Match = 0)
                    
                    rv$total_list[[c]] <- data.frame(Coder_1 = 0, 
                                                     Coder_2 = 0, 
                                                     Agree = NA, 
                                                     Behavior = all.codes[c], 
                                                     Behavior_typ = type_tmp)
                    
                    rv$kappa_list[[c]] <- paste("No", all.codes[c], "behaviors coded")
                    rv$conf_matrix_list[[c]] <- paste("No", all.codes[c], "behaviors coded")
                    rv$agree_list[[c]] <- paste("No", all.codes[c], "behaviors coded")
                }
                
                if(sum(grepl(all.codes[c], rv$dat1$Behavior)) >= 1){
                    
                    if(length(raw.code.list[[c]]) == 1){
                        
                        if(sum(rv$duration_codes == all.codes[c]) == 1 &
                           sum(grepl(all.codes[c], rv$dat1$Behavior)) >= 1){
                            #browser()
                            
                            rv$DF_list[[c]] <- data.frame()
                            DF_coder.1 <- data.frame(Time = Time)
                            DF_coder.2 <- data.frame(Time = Time)
                            tmp.DF_coder.1 <- rv$dat1_coder.1[rv$dat1_coder.1$Behavior == raw.code.list[[c]],]
                            tmp.DF_coder.2 <- rv$dat1_coder.2[rv$dat1_coder.2$Behavior == raw.code.list[[c]],]
                            tmp.vec_coder.1 <- rep(0, length(Time))
                            tmp.vec_coder.2 <- rep(0, length(Time))
                            
                            if(nrow(tmp.DF_coder.1)>= 2){
                                
                                for(j in seq(1, nrow(tmp.DF_coder.1), by = 2)){
                                    
                                    tmp.vec_coder.1<-ifelse(Time >= tmp.DF_coder.1[j, "Time"] & 
                                                                Time <= tmp.DF_coder.1[j+1, "Time"], 
                                                            1, 
                                                            tmp.vec_coder.1)
                                }
                            }
                            
                            if(nrow(tmp.DF_coder.2) >= 2){
                                
                                for(j in seq(1, nrow(tmp.DF_coder.2), by = 2)){
                                    tmp.vec_coder.2<-ifelse(Time >= tmp.DF_coder.2[j, "Time"] & 
                                                                Time <= tmp.DF_coder.2[j+1, "Time"], 
                                                            1, 
                                                            tmp.vec_coder.2)
                                }
                            }
                            
                            #------------------------------------------------------------------------
                            #Attaching Results to final data set for each coder (will save at the end)
                            DF_coder.1<-cbind(DF_coder.1, 
                                              tmp.vec_coder.1)
                            colnames(DF_coder.1)[ncol(DF_coder.1)] <- all.codes[c]
                            
                            DF_coder.2<-cbind(DF_coder.2, 
                                              tmp.vec_coder.2)
                            colnames(DF_coder.2)[ncol(DF_coder.2)]<-all.codes[c]
                            
                            rv$agree_list[[c]] <- mean(tmp.vec_coder.1 == tmp.vec_coder.2)
                            
                            if(mean(tmp.vec_coder.1==tmp.vec_coder.2) == 1){
                                rv$kappa_list[[c]] <- "Perfect agreement"
                            }
                            
                            else{
                                rv$kappa_list[[c]] <- psych::cohen.kappa(cbind(tmp.vec_coder.1, 
                                                                               tmp.vec_coder.2))$kappa
                            }
                            
                            rv$DF_list[[c]] <- data.frame(Coder = c(rep(rv$coder_1, nrow(DF_coder.1)), 
                                                                    rep(rv$coder_2, nrow(DF_coder.2))), 
                                                          Time = rep(Time, 2), 
                                                          Value = c(tmp.vec_coder.1, 
                                                                    tmp.vec_coder.2), 
                                                          Behavior = all.codes[c], 
                                                          Behavior_typ = "Duration")
                            
                            #agreement below is overlapping presence of the behavior not total agreement
                            
                            rv$total_list[[c]] <- data.frame(Coder_1 = sum(tmp.vec_coder.1 == 1)/100, 
                                                             Coder_2 = sum(tmp.vec_coder.2 == 1)/100, 
                                                             Agree = sum(tmp.vec_coder.1 == 1 & tmp.vec_coder.2 == 1)/100, 
                                                             Behavior = all.codes[c], 
                                                             Behavior_typ = "Duration")
                            
                            mat_11 <- sum(tmp.vec_coder.1 == 0 & tmp.vec_coder.2 == 0) 
                            mat_12 <- sum(tmp.vec_coder.1 == 1 & tmp.vec_coder.2 == 0)
                            mat_21 <- sum(tmp.vec_coder.1 == 0 & tmp.vec_coder.2 == 1)
                            mat_22 <- sum(tmp.vec_coder.1 == 1 & tmp.vec_coder.2 == 1)
                            
                            mat.tmp <- matrix(c(mat_11, 
                                                mat_12, 
                                                mat_21, 
                                                mat_22), 
                                              byrow = TRUE, 
                                              ncol = 2) 
                            
                            colnames(mat.tmp) <- c("Absent", "Present")
                            rownames(mat.tmp) <- c("Absent", "Present")
                            
                            tab.tmp <- as.table(mat.tmp)
                            
                            if(mean(tmp.vec_coder.1 == tmp.vec_coder.2) == 1){
                                rv$conf_matrix_list[[c]] <- "Perfect agreement"
                            }
                            
                            else{
                                rv$conf_matrix_list[[c]] <- caret::confusionMatrix(tab.tmp, 
                                                                                   positive = "Present")
                            }
                        }
                        
                        if(sum(rv$event_codes == all.codes[c]) == 1 &
                           sum(grepl(all.codes[c], rv$dat1$Behavior)) >= 1){
                            
                            #browser()
                            tmp.DF_coder.1 <- rv$dat1_coder.1[rv$dat1_coder.1$Behavior == raw.code.list[[c]],]
                            tmp.DF_coder.2 <- rv$dat1_coder.2[rv$dat1_coder.2$Behavior == raw.code.list[[c]],]
                            
                            if(nrow(tmp.DF_coder.1) > 0){
                                
                                tmp.match_coder.1<-vector()
                                
                                for(r in 1:nrow(tmp.DF_coder.1)){
                                    
                                    time.min<-tmp.DF_coder.1[r, "Time"] - input$point_interval
                                    time.max<-tmp.DF_coder.1[r, "Time"] + input$point_interval
                                    
                                    if(nrow(tmp.DF_coder.2) > 0){
                                        tmp.match<-ifelse(sum(tmp.DF_coder.2["Time"] >= time.min & 
                                                                  tmp.DF_coder.2["Time"] <= time.max), 
                                                          1, 0)
                                    }
                                    
                                    else{
                                        tmp.match<-0
                                    }
                                    
                                    tmp.match_coder.1<-c(tmp.match_coder.1, tmp.match)
                                }
                            }  
                            
                            if(nrow(tmp.DF_coder.2) > 0){
                                
                                tmp.match_coder.2<-vector()
                                
                                for(r in 1:nrow(tmp.DF_coder.2)){
                                    
                                    time.min<-tmp.DF_coder.2[r, "Time"] - input$point_interval
                                    time.max<-tmp.DF_coder.2[r, "Time"] + input$point_interval
                                    
                                    if(nrow(tmp.DF_coder.1) > 0){
                                        tmp.match<-ifelse(sum(tmp.DF_coder.1["Time"] >= time.min & 
                                                                  tmp.DF_coder.1["Time"] <= time.max), 
                                                          1, 0)
                                    }
                                    
                                    else{
                                        tmp.match<-0
                                    }
                                    
                                    tmp.match_coder.2<-c(tmp.match_coder.2, tmp.match)
                                }
                            }
                            
                            if(nrow(tmp.DF_coder.1) == 0) {tmp.match_coder.1 <- vector()}
                            
                            if(nrow(tmp.DF_coder.2) == 0) {tmp.match_coder.2 <- vector()}
                            
                            if(nrow(tmp.DF_coder.1) == 0 & nrow(tmp.DF_coder.2) > 0 ){
                                mat_11 <- sum(tmp.match_coder.2) #both coders agree
                                mat_12 <- 0 #Coder 1 saw an event but coder 2 did not
                                mat_21 <- sum(tmp.match_coder.2 == 0) #Coder 2 saw event but coder 1 did not
                                mat_22 <- 0
                                
                                match_matrix <- matrix(c(mat_11, mat_12,
                                                         mat_21, mat_22), 
                                                       byrow = TRUE, 
                                                       nrow = 2)
                                
                                rv$agree_list[[c]] <- sum(diag(match_matrix))/sum(match_matrix)
                                rv$kappa_list[[c]] <- psych::cohen.kappa(match_matrix)$kappa
                                
                                rv$DF_list[[c]] <- data.frame(Coder = c(rep(rv$coder_1, nrow(tmp.DF_coder.1)), 
                                                                        rep(rv$coder_2, nrow(tmp.DF_coder.2))), 
                                                              Time = c(unlist(tmp.DF_coder.1["Time"]), 
                                                                       unlist(tmp.DF_coder.2["Time"])),
                                                              Value = c(rep(1, nrow(tmp.DF_coder.1)), 
                                                                        rep(1, nrow(tmp.DF_coder.2))),
                                                              Match = c(ifelse(tmp.match_coder.1 == 1, 
                                                                               "Match", 
                                                                               "No Match"),
                                                                        ifelse(tmp.match_coder.2 == 1, 
                                                                               "Match", 
                                                                               "No Match")),
                                                              Behavior = all.codes[c], 
                                                              Behavior_typ = "Event")
                                
                                rv$total_list[[c]] <- data.frame(Coder_1 = nrow(tmp.DF_coder.1), 
                                                                 Coder_2 = nrow(tmp.DF_coder.2), 
                                                                 Agree = mat_11/sum(match_matrix), 
                                                                 Behavior = all.codes[c], 
                                                                 Behavior_typ = "Event")
                                
                                colnames(match_matrix) <- c("Yes", "No")
                                rownames(match_matrix) <- c("Yes", "No")
                                
                                if(sum(match_matrix) >= 3){
                                    rv$conf_matrix_list[[c]] <- caret::confusionMatrix(match_matrix, 
                                                                                       positive = "Yes")
                                }
                                
                                else{
                                    rv$conf_matrix_list[[c]] <- "Insufficient number of events to calculate meaningful reliability estimates."
                                }
                            }
                            
                            if(nrow(tmp.DF_coder.2) == 0 & nrow(tmp.DF_coder.1) > 0 ){
                                mat_11 <- sum(tmp.match_coder.1) #both coders agree
                                mat_12 <- sum(tmp.match_coder.1 == 0) #Coder 1 saw an event but coder 2 did not
                                mat_21 <- 0 #Coder 2 saw event but coder 1 did not
                                mat_22 <- 0
                                
                                match_matrix <- matrix(c(mat_11, mat_12,
                                                         mat_21, mat_22), 
                                                       byrow = TRUE, 
                                                       nrow = 2)
                                
                                rv$agree_list[[c]]<-sum(diag(match_matrix))/sum(match_matrix)
                                rv$kappa_list[[c]]<-psych::cohen.kappa(match_matrix)$kappa
                                
                                rv$DF_list[[c]] <- data.frame(Coder = c(rep(rv$coder_1, nrow(tmp.DF_coder.1)), 
                                                                        rep(rv$coder_2, nrow(tmp.DF_coder.2))), 
                                                              Time = c(unlist(tmp.DF_coder.1["Time"]), 
                                                                       unlist(tmp.DF_coder.2["Time"])),
                                                              Value = c(rep(1, nrow(tmp.DF_coder.1)), 
                                                                        rep(1, nrow(tmp.DF_coder.2))),
                                                              Match = c(ifelse(tmp.match_coder.1 == 1, 
                                                                               "Match", 
                                                                               "No Match"),
                                                                        ifelse(tmp.match_coder.2 == 1, 
                                                                               "Match", 
                                                                               "No Match")),
                                                              Behavior = all.codes[c], 
                                                              Behavior_typ = "Event")
                                
                                rv$total_list[[c]] <- data.frame(Coder_1 = nrow(tmp.DF_coder.1), 
                                                                 Coder_2 = nrow(tmp.DF_coder.2), 
                                                                 Agree = mat_11/sum(match_matrix), 
                                                                 Behavior = all.codes[c], 
                                                                 Behavior_typ = "Event")
                                
                                colnames(match_matrix) <- c("Yes", "No")
                                rownames(match_matrix) <- c("Yes", "No")
                                
                                if(sum(match_matrix) >= 3){
                                    rv$conf_matrix_list[[c]] <- caret::confusionMatrix(match_matrix, 
                                                                                       positive = "Yes")
                                }
                                
                                else{
                                    rv$conf_matrix_list[[c]] <- "Insufficient number of events to calculate meaningful reliability estimates."
                                }
                            }
                            
                            if(nrow(tmp.DF_coder.2) > 0 & nrow(tmp.DF_coder.1) > 0 ){
                                mat_11 <- sum(tmp.match_coder.2) #both coders agree
                                mat_12 <- sum(tmp.match_coder.1 == 0) #Coder 1 saw an event but coder 2 did not
                                mat_21 <- sum(tmp.match_coder.2 == 0) #Coder 2 saw event but coder 1 did not
                                mat_22 <- 0 #Always zero (since there is no "negative" event code)
                                
                                match_matrix <- matrix(c(mat_11, mat_12,
                                                         mat_21, mat_22), 
                                                       byrow = TRUE, 
                                                       nrow = 2)
                                
                                rv$agree_list[[c]]<-sum(diag(match_matrix))/sum(match_matrix)
                                rv$kappa_list[[c]]<-psych::cohen.kappa(match_matrix)$kappa
                                
                                rv$DF_list[[c]] <- data.frame(Coder = c(rep(rv$coder_1, nrow(tmp.DF_coder.1)), 
                                                                        rep(rv$coder_2, nrow(tmp.DF_coder.2))), 
                                                              Time = as.numeric(c(unlist(tmp.DF_coder.1["Time"]), 
                                                                                  unlist(tmp.DF_coder.2["Time"]))),
                                                              Value = c(rep(1, nrow(tmp.DF_coder.1)), 
                                                                        rep(1, nrow(tmp.DF_coder.2))),
                                                              Match = c(ifelse(tmp.match_coder.1 == 1, 
                                                                               "Match", 
                                                                               "No Match"),
                                                                        ifelse(tmp.match_coder.2 == 1, 
                                                                               "Match", 
                                                                               "No Match")),
                                                              Behavior = all.codes[c], 
                                                              Behavior_typ = "Event")
                                
                                rv$total_list[[c]] <- data.frame(Coder_1 = nrow(tmp.DF_coder.1), 
                                                                 Coder_2 = nrow(tmp.DF_coder.2), 
                                                                 Agree = mat_11/sum(match_matrix), 
                                                                 Behavior = all.codes[c], 
                                                                 Behavior_typ = "Event")
                                
                                colnames(match_matrix) <- c("Yes", "No")
                                rownames(match_matrix) <- c("Yes", "No")
                                
                                if(sum(match_matrix) >= 3){
                                    rv$conf_matrix_list[[c]] <- caret::confusionMatrix(match_matrix, 
                                                                                       positive = "Yes")
                                }
                                
                                else{
                                    rv$conf_matrix_list[[c]] <- "Insufficient number of events to calculate meaningful reliability estimates."
                                }
                            }
                        }
                    }
                    
                    if(length(raw.code.list[[c]]) > 1){
                        
                        #--------------------------------------------------------------------------
                        #Extracting family codes for duration events:
                        if(sum(rv$duration_codes == all.codes[c]) == 1 &
                           sum(grepl(all.codes[c], rv$dat1$Behavior)) >= 1){
                            #browser()
                            
                            rv$DF_list[[c]] <- data.frame()
                            rv$total_list[[c]] <- data.frame()
                            conf_matrix_tmp <- list()
                            
                            DF_coder.1<-data.frame(Time = Time)
                            DF_coder.2<-data.frame(Time = Time)
                            tmp.codes <- unlist(as.character(raw.code.list[[c]]))
                            tmp.agree <- vector()
                            tmp.kappa <- vector()
                            #l <- 1
                            
                            for(l in 1:length(tmp.codes)){
                                
                                if(sum(grepl(tmp.codes[l], rv$dat1$Behavior)) == 0){
                                    
                                    DF_coder.1<-cbind(DF_coder.1, 
                                                      rep(0, length(Time)))
                                    colnames(DF_coder.1)[ncol(DF_coder.1)] <- tmp.codes[l]
                                    
                                    DF_coder.2<-cbind(DF_coder.2, 
                                                      rep(0, length(Time)))
                                    colnames(DF_coder.2)[ncol(DF_coder.2)] <- tmp.codes[l]
                                    
                                    tmp_duration_DF<-data.frame(Coder = c(rep(rv$coder_1, length(Time)), 
                                                                          rep(rv$coder_2, length(Time))),
                                                                Time = rep(Time, 2), 
                                                                Value = 0, 
                                                                Behavior = tmp.codes[l], 
                                                                Behavior_typ = "Duration")
                                    
                                    rv$DF_list[[c]] <- rbind(rv$DF_list[[c]], tmp_duration_DF)
                                    
                                    tmp_total_DF <- data.frame(Coder_1 = 0, 
                                                               Coder_2 = 0, 
                                                               Agree = NA, 
                                                               Behavior = tmp.codes[l], 
                                                               Behavior_typ = "Duration")
                                    
                                    rv$total_list[[c]] <- rbind(rv$total_list[[c]], tmp_total_DF)
                                    
                                    tmp.agree <- c(tmp.agree, NA)
                                    tmp.kappa <- c(tmp.kappa, NA)
                                    conf_matrix_tmp[[l]] <- paste("No", tmp.codes[l], "behaviors coded")
                                }
                                
                                if(sum(grepl(tmp.codes[l], rv$dat1$Behavior)) >= 1){ 
                                    tmp.DF_coder.1<-rv$dat1_coder.1[rv$dat1_coder.1$Behavior == tmp.codes[l],]
                                    tmp.DF_coder.2<-rv$dat1_coder.2[rv$dat1_coder.2$Behavior == tmp.codes[l],]
                                    tmp.vec_coder.1<-rep(0, length(Time))
                                    tmp.vec_coder.2<-rep(0, length(Time))

                                    
                                    if(nrow(tmp.DF_coder.1)>=2){
                                        
                                        for(j in seq(1, nrow(tmp.DF_coder.1), by=2)){
                                            tmp.vec_coder.1<-ifelse(Time >= tmp.DF_coder.1[j, "Time"] & 
                                                                        Time <= tmp.DF_coder.1[j+1, "Time"],
                                                                    1, tmp.vec_coder.1)
                                        }
                                    }
                                    
                                    if(nrow(tmp.DF_coder.2)>=2){
                                        
                                        for(j in seq(1, nrow(tmp.DF_coder.2), by=2)){
                                            tmp.vec_coder.2<-ifelse(Time >= tmp.DF_coder.2[j, "Time"] & 
                                                                        Time <= tmp.DF_coder.2[j+1, "Time"], 
                                                                    1, tmp.vec_coder.2)
                                        }
                                    }
                                    #------------------------------------------------------------------------
                                    #Attaching Results to final data set for each coder (will save at the end)
                                    DF_coder.1<-cbind(DF_coder.1, 
                                                      tmp.vec_coder.1)
                                    colnames(DF_coder.1)[ncol(DF_coder.1)] <- tmp.codes[l]
                                    
                                    DF_coder.2<-cbind(DF_coder.2, 
                                                      tmp.vec_coder.2)
                                    colnames(DF_coder.2)[ncol(DF_coder.2)] <- tmp.codes[l]
                                    
                                    tmp.agree<-c(tmp.agree, mean(tmp.vec_coder.1 == tmp.vec_coder.2))
                                    tmp.kappa<-c(tmp.kappa, psych::cohen.kappa(cbind(tmp.vec_coder.1, 
                                                                                     tmp.vec_coder.2))$kappa)
                                    
                                    tmp_duration_DF<-data.frame(Coder = c(rep(rv$coder_1, nrow(DF_coder.1)), 
                                                                          rep(rv$coder_2, nrow(DF_coder.2))),
                                                                Time = rep(Time, 2), 
                                                                Value = c(tmp.vec_coder.1, 
                                                                          tmp.vec_coder.2), 
                                                                Behavior = tmp.codes[l], 
                                                                Behavior_typ = "Duration")
                                    
                                    rv$DF_list[[c]] <- rbind(rv$DF_list[[c]], 
                                                             tmp_duration_DF)
                                    
                                    tmp_total_DF <- data.frame(Coder_1 = sum(tmp.vec_coder.1 == 1)/100, 
                                                               Coder_2 = sum(tmp.vec_coder.2 == 1)/100, 
                                                               Agree = sum(tmp.vec_coder.1 == 1 & tmp.vec_coder.2 == 1)/100, 
                                                               Behavior = tmp.codes[l], 
                                                               Behavior_typ = "Duration")
                                    
                                    rv$total_list[[c]] <- rbind(rv$total_list[[c]], 
                                                                tmp_total_DF)
                                    
                                    mat_11 <- sum(tmp.vec_coder.1 == 0 & tmp.vec_coder.2 == 0) 
                                    mat_12 <- sum(tmp.vec_coder.1 == 1 & tmp.vec_coder.2 == 0)
                                    mat_21 <- sum(tmp.vec_coder.1 == 0 & tmp.vec_coder.2 == 1)
                                    mat_22 <- sum(tmp.vec_coder.1 == 1 & tmp.vec_coder.2 == 1)
                                    
                                    mat.tmp <- matrix(c(mat_11, 
                                                        mat_12, 
                                                        mat_21, 
                                                        mat_22), 
                                                      byrow = TRUE, 
                                                      ncol = 2) 
                                    
                                    colnames(mat.tmp) <- c("Absent", "Present")
                                    rownames(mat.tmp) <- c("Absent", "Present")
                                    
                                    tab.tmp <- as.table(mat.tmp)
                                    
                                    if(mean(tmp.vec_coder.1==tmp.vec_coder.2) == 1){
                                        conf_matrix_tmp[[l]] <- paste("Perfect agreement - no summaries produced")
                                    }
                                    
                                    else{
                                        conf_matrix_tmp[[l]] <- caret::confusionMatrix(tab.tmp, 
                                                                                       positive = "Present")
                                    }
                                }
                            }
                            #browser()
                            tmp_tot_coder.1 <- ifelse(rowSums(DF_coder.1[,2:ncol(DF_coder.1)]) >= 1, 
                                                      1, 0)
                            tmp_tot_coder.2 <- ifelse(rowSums(DF_coder.2[,2:ncol(DF_coder.2)]) >= 1, 
                                                      1, 0)
                            tmp.vec_coder.1 <- rv$DF_list[[c]]$Value[rv$DF_list[[c]]$Coder == rv$coder_1]
                            tmp.vec_coder.2 <- rv$DF_list[[c]]$Value[rv$DF_list[[c]]$Coder == rv$coder_2]
                            
                            mat_11 <- sum(tmp.vec_coder.1 == 0 & tmp.vec_coder.2 == 0) 
                            mat_12 <- sum(tmp.vec_coder.1 == 1 & tmp.vec_coder.2 == 0)
                            mat_21 <- sum(tmp.vec_coder.1 == 0 & tmp.vec_coder.2 == 1)
                            mat_22 <- sum(tmp.vec_coder.1 == 1 & tmp.vec_coder.2 == 1)
                            
                            mat.tmp <- matrix(c(mat_11, 
                                                mat_12, 
                                                mat_21, 
                                                mat_22), 
                                              byrow = TRUE, 
                                              ncol = 2) 
                            
                            colnames(mat.tmp) <- c("Absent", "Present")
                            rownames(mat.tmp) <- c("Absent", "Present")
                            
                            tab.tmp <- as.table(mat.tmp)
                            
                            if(mean(tmp.vec_coder.1 == tmp.vec_coder.2) == 1){
                                conf_matrix_tmp[[l+1]] <- paste("Perfect agreement - no summaries produced")
                            }
                            
                            else{
                                conf_matrix_tmp[[l+1]] <- caret::confusionMatrix(tab.tmp, 
                                                                                 positive = "Present")
                            }
                            
                            tot_agree <- sum(rv$DF_list[[c]]$Value[rv$DF_list[[c]]$Coder == rv$coder_1] == 
                                                 rv$DF_list[[c]]$Value[rv$DF_list[[c]]$Coder == rv$coder_2])/(nrow(rv$DF_list[[c]])/2)
                            
                            tot_kappa <- psych::cohen.kappa(cbind(rv$DF_list[[c]]$Value[rv$DF_list[[c]]$Coder == rv$coder_1], 
                                                                  rv$DF_list[[c]]$Value[rv$DF_list[[c]]$Coder == rv$coder_2]))$kappa
                            
                            tmp_duration_DF<-data.frame(Coder = c(rep(rv$coder_1, nrow(DF_coder.1)), 
                                                                  rep(rv$coder_2, nrow(DF_coder.2))),
                                                        Time = rep(Time, 2), 
                                                        Value = c(tmp_tot_coder.1, 
                                                                  tmp_tot_coder.2),
                                                        Behavior = paste("Total", all.codes[c]), 
                                                        Behavior_typ = "Duration")
                            
                            rv$DF_list[[c]] <- rbind(rv$DF_list[[c]], 
                                                     tmp_duration_DF)
                            
                            tmp_total_DF <- data.frame(Coder_1 = sum(tmp_tot_coder.1 == 1)/100, 
                                                       Coder_2 = sum(tmp_tot_coder.2 == 1)/100, 
                                                       Agree = sum(tmp_tot_coder.1 == 1 &  tmp_tot_coder.2 == 1)/100, 
                                                       Behavior = paste("Total", all.codes[c]), 
                                                       Behavior_typ = "Duration")
                            
                            rv$total_list[[c]] <- rbind(rv$total_list[[c]], 
                                                        tmp_total_DF)
                            
                            tmp.agree <- c(tmp.agree, tot_agree)
                            tmp.kappa <- c(tmp.kappa, tot_kappa)
                            
                            names(tmp.agree) <- c(tmp.codes, paste("Total", all.codes[c]))
                            names(tmp.kappa) <- c(tmp.codes, paste("Total", all.codes[c]))
                            names(conf_matrix_tmp) <- c(tmp.codes, paste("Total", all.codes[c]))
                            
                            rv$agree_list[[c]] <- tmp.agree
                            rv$kappa_list[[c]] <- tmp.kappa
                            rv$conf_matrix_list[[c]] <- conf_matrix_tmp
                        }
                        
                        #--------------------------------------------------------------------------
                        #Extracting family codes for point events:
                        if(sum(rv$event_codes == all.codes[c]) == 1 &
                           sum(grepl(all.codes[c], rv$dat1$Behavior)) >= 1){
                            #browser()
                            
                            rv$DF_list[[c]] <- data.frame()
                            rv$total_list[[c]] <- data.frame()
                            conf_matrix_tmp <- list()
                            
                            tmp.codes <- unlist(as.character(raw.code.list[[c]]))
                            tmp.agree <- vector()
                            tmp.kappa <- vector()
                            
                            tot_match_matrix <- matrix(c(0, 0,
                                                         0, 0), 
                                                       byrow = TRUE, 
                                                       nrow = 2)
                            #l<-1
                            for(l in 1:length(tmp.codes)){
                                tmp.DF_coder.1<-rv$dat1_coder.1[rv$dat1_coder.1$Behavior == tmp.codes[l],]
                                tmp.DF_coder.2<-rv$dat1_coder.2[rv$dat1_coder.2$Behavior == tmp.codes[l],]
                                
                                if(sum(grepl(tmp.codes[l], rv$dat1$Behavior)) == 0){
                                    
                                    tmp_total_DF <- data.frame(Coder_1 = 0, 
                                                               Coder_2 = 0, 
                                                               Agree = NA, 
                                                               Behavior = tmp.codes[l], 
                                                               Behavior_typ = "Event")
                                    
                                    rv$total_list[[c]] <- rbind(rv$total_list[[c]], tmp_total_DF)
                                    
                                    tmp.agree <- c(tmp.agree, NA)
                                    tmp.kappa <- c(tmp.kappa, NA)
                                    conf_matrix_tmp[[l]] <- paste("No", tmp.codes[l], "behaviors coded")
                                }
                                
                                if(sum(grepl(tmp.codes[l], rv$dat1$Behavior)) >= 1){
                                    if(nrow(tmp.DF_coder.1) > 0){
                                        tmp.match_coder.1<-vector()
                                        
                                        for(r in 1:nrow(tmp.DF_coder.1)){
                                            time.min<-tmp.DF_coder.1[r, "Time"] - input$point_interval
                                            time.max<-tmp.DF_coder.1[r, "Time"] + input$point_interval
                                            
                                            if(nrow(tmp.DF_coder.2) > 0){
                                                tmp.match<-ifelse(sum(tmp.DF_coder.2["Time"] >= time.min & 
                                                                          tmp.DF_coder.2["Time"] <= time.max), 
                                                                  1, 0)
                                            }
                                            
                                            else{
                                                tmp.match<-0
                                            }
                                            
                                            tmp.match_coder.1<-c(tmp.match_coder.1, tmp.match)
                                        }
                                    }  
                                    
                                    if(nrow(tmp.DF_coder.2) > 0){
                                        tmp.match_coder.2<-vector()
                                        
                                        for(r in 1:nrow(tmp.DF_coder.2)){
                                            time.min<-tmp.DF_coder.2[r, "Time"] - input$point_interval
                                            time.max<-tmp.DF_coder.2[r, "Time"] + input$point_interval
                                            
                                            if(nrow(tmp.DF_coder.1) > 0){
                                                tmp.match<-ifelse(sum(tmp.DF_coder.1["Time"] >= time.min & 
                                                                          tmp.DF_coder.1["Time"] <= time.max), 
                                                                  1, 0)
                                            }
                                            
                                            else{
                                                tmp.match<-0
                                            }
                                            
                                            tmp.match_coder.2<-c(tmp.match_coder.2, tmp.match)
                                        }
                                    }  
                                    
                                    if(nrow(tmp.DF_coder.1) == 0) {tmp.match_coder.1 <- vector()}
                                    if(nrow(tmp.DF_coder.2) == 0) {tmp.match_coder.2 <- vector()}
                                    
                                    if(nrow(tmp.DF_coder.1) == 0 & nrow(tmp.DF_coder.2) > 0){
                                        mat_11 <- sum(tmp.match_coder.2) #both coders agree
                                        mat_12 <- 0 #Coder 1 saw an event but coder 2 did not
                                        mat_21 <- sum(tmp.match_coder.2 == 0) #Coder 2 saw event but coder 1 did not
                                        mat_22 <- 0
                                        
                                        match_matrix <- matrix(c(mat_11, mat_12,
                                                                 mat_21, mat_22), 
                                                               byrow = TRUE, 
                                                               nrow = 2)
                                        
                                        tot_match_matrix <- tot_match_matrix + match_matrix
                                        
                                        tmp.agree <- c(tmp.agree, sum(diag(match_matrix))/sum(match_matrix))
                                        tmp.kappa <- c(tmp.kappa, psych::cohen.kappa(match_matrix)$kappa)
                                        
                                        DF_tmp <- data.frame(Coder = c(rep(rv$coder_1, nrow(tmp.DF_coder.1)), 
                                                                       rep(rv$coder_2, nrow(tmp.DF_coder.2))), 
                                                             Time = as.numeric(c(unlist(tmp.DF_coder.1["Time"]), 
                                                                                 unlist(tmp.DF_coder.2["Time"]))),
                                                             Value = c(rep(1, nrow(tmp.DF_coder.1)), 
                                                                       rep(1, nrow(tmp.DF_coder.2))),
                                                             Match = c(ifelse(tmp.match_coder.1 == 1, 
                                                                              "Match", 
                                                                              "No Match"),
                                                                       ifelse(tmp.match_coder.2 == 1, 
                                                                              "Match", 
                                                                              "No Match")),
                                                             Behavior = tmp.codes[l], 
                                                             Behavior_typ = "Event")
                                        
                                        Total_tmp <- data.frame(Coder_1 = nrow(tmp.DF_coder.1), 
                                                                Coder_2 = nrow(tmp.DF_coder.2), 
                                                                Agree = mat_11, 
                                                                Behavior = tmp.codes[l], 
                                                                Behavior_typ = "Event")
                                        
                                        rv$DF_list[[c]]<-rbind(rv$DF_list[[c]], 
                                                               DF_tmp)
                                        
                                        rv$total_list[[c]]<-rbind(rv$total_list[[c]], 
                                                                  Total_tmp)
                                        
                                        colnames(match_matrix) <- c("Yes", "No")
                                        rownames(match_matrix) <- c("Yes", "No")
                                        
                                        if(sum(match_matrix) >= 3){
                                            conf_matrix_tmp[[l]] <- caret::confusionMatrix(as.table(match_matrix), 
                                                                                           positive = "Yes")
                                        }
                                        
                                        else{ 
                                            conf_matrix_tmp[[l]] <- "Insufficient number of events to calculate meaningful reliability estimates."
                                        }
                                    }
                                    
                                    if(nrow(tmp.DF_coder.2) == 0 & nrow(tmp.DF_coder.1) > 0){
                                        mat_11 <- sum(tmp.match_coder.1) #both coders agree
                                        mat_12 <- sum(tmp.match_coder.1 == 0) #Coder 1 saw an event but coder 2 did not
                                        mat_21 <- 0 #Coder 2 saw event but coder 1 did not
                                        mat_22 <- 0
                                        
                                        match_matrix <- matrix(c(mat_11, mat_12,
                                                                 mat_21, mat_22), 
                                                               byrow = TRUE, 
                                                               nrow = 2)
                                        
                                        tot_match_matrix <- tot_match_matrix + match_matrix
                                        
                                        tmp.agree <- c(tmp.agree, sum(diag(match_matrix))/sum(match_matrix))
                                        tmp.kappa <- c(tmp.kappa, psych::cohen.kappa(match_matrix)$kappa)
                                        
                                        DF_tmp <- data.frame(Coder = c(rep(rv$coder_1, nrow(tmp.DF_coder.1)), 
                                                                       rep(rv$coder_2, nrow(tmp.DF_coder.2))), 
                                                             Time = as.numeric(c(unlist(tmp.DF_coder.1["Time"]), 
                                                                                 unlist(tmp.DF_coder.2["Time"]))),
                                                             Value = c(rep(1, nrow(tmp.DF_coder.1)), 
                                                                       rep(1, nrow(tmp.DF_coder.2))),
                                                             Match = c(ifelse(tmp.match_coder.1 == 1, 
                                                                              "Match", 
                                                                              "No Match"),
                                                                       ifelse(tmp.match_coder.2 == 1, 
                                                                              "Match", 
                                                                              "No Match")),
                                                             Behavior = tmp.codes[l], 
                                                             Behavior_typ = "Event")
                                        
                                        Total_tmp <- data.frame(Coder_1 = nrow(tmp.DF_coder.1), 
                                                                Coder_2 = nrow(tmp.DF_coder.2), 
                                                                Agree = mat_11, 
                                                                Behavior = tmp.codes[l], 
                                                                Behavior_typ = "Event")
                                        
                                        rv$DF_list[[c]]<-rbind(rv$DF_list[[c]], 
                                                               DF_tmp)
                                        
                                        rv$total_list[[c]]<-rbind(rv$total_list[[c]], 
                                                                  Total_tmp)
                                        
                                        colnames(match_matrix) <- c("Yes", "No")
                                        rownames(match_matrix) <- c("Yes", "No")
                                        
                                        if(sum(match_matrix) >= 3){
                                            conf_matrix_tmp[[l]] <- caret::confusionMatrix(as.table(match_matrix), 
                                                                                           positive = "Yes")
                                        }
                                        
                                        else{ 
                                            conf_matrix_tmp[[l]] <- "Insufficient number of events to calculate meaningful reliability estimates."
                                        }
                                    }
                                    
                                    if(nrow(tmp.DF_coder.2) > 0 & nrow(tmp.DF_coder.1) > 0){
                                        mat_11 <- sum(tmp.match_coder.2) #both coders agree
                                        mat_12 <- sum(tmp.match_coder.1 == 0) #Coder 1 saw an event but coder 2 did not
                                        mat_21 <- sum(tmp.match_coder.2 == 0) #Coder 2 saw event but coder 1 did not
                                        mat_22 <- 0 #Always zero (since there is no "negative" event code)
                                        
                                        match_matrix <- matrix(c(mat_11, mat_12,
                                                                 mat_21, mat_22), 
                                                               byrow = TRUE, 
                                                               nrow = 2)
                                        
                                        tot_match_matrix <- tot_match_matrix + match_matrix
                                        
                                        tmp.agree <- c(tmp.agree, sum(diag(match_matrix))/sum(match_matrix))
                                        tmp.kappa <- c(tmp.kappa, psych::cohen.kappa(match_matrix)$kappa)
                                        
                                        DF_tmp <- data.frame(Coder = c(rep(rv$coder_1, nrow(tmp.DF_coder.1)), 
                                                                       rep(rv$coder_2, nrow(tmp.DF_coder.2))), 
                                                             Time = as.numeric(c(unlist(tmp.DF_coder.1["Time"]), 
                                                                                 unlist(tmp.DF_coder.2["Time"]))),
                                                             Value = c(rep(1, nrow(tmp.DF_coder.1)), 
                                                                       rep(1, nrow(tmp.DF_coder.2))),
                                                             Match = c(ifelse(tmp.match_coder.1 == 1, 
                                                                              "Match", 
                                                                              "No Match"),
                                                                       ifelse(tmp.match_coder.2 == 1, 
                                                                              "Match", 
                                                                              "No Match")),
                                                             Behavior = tmp.codes[l], 
                                                             Behavior_typ = "Event")
                                        
                                        Total_tmp <- data.frame(Coder_1 = nrow(tmp.DF_coder.1), 
                                                                Coder_2 = nrow(tmp.DF_coder.2), 
                                                                Agree = mat_11, 
                                                                Behavior = tmp.codes[l], 
                                                                Behavior_typ = "Event")
                                        
                                        rv$DF_list[[c]]<-rbind(rv$DF_list[[c]], 
                                                               DF_tmp)
                                        
                                        rv$total_list[[c]]<-rbind(rv$total_list[[c]], 
                                                                  Total_tmp)
                                        
                                        colnames(match_matrix) <- c("Yes", "No")
                                        rownames(match_matrix) <- c("Yes", "No")
                                        
                                        if(sum(match_matrix) >= 3){
                                            conf_matrix_tmp[[l]] <- caret::confusionMatrix(as.table(match_matrix), 
                                                                                           positive = "Yes")
                                        }
                                        
                                        else{ 
                                            conf_matrix_tmp[[l]] <- "Insufficient number of events to calculate meaningful reliability estimates."
                                        }
                                    }
                                    
                                    if(nrow(tmp.DF_coder.1) + nrow(tmp.DF_coder.2) == 0){
                                        tmp.agree<-c(tmp.agree, NA)
                                        tmp.kappa<-c(tmp.kappa, NA)
                                    }
                                }
                            }
                            
                            tot_agree <- sum(diag(tot_match_matrix))/sum(tot_match_matrix)
                            tot_kappa <- psych::cohen.kappa(tot_match_matrix)$kappa
                            
                            tmp.agree <- c(tmp.agree, tot_agree)
                            tmp.kappa <- c(tmp.kappa, tot_kappa)
                            
                            names(tmp.agree) <- c(tmp.codes, "Total")
                            names(tmp.kappa) <- c(tmp.codes, "Total")
                            
                            colnames(tot_match_matrix) <- c("Yes", "No")
                            rownames(tot_match_matrix) <- c("Yes", "No")
                            
                            if(sum(tot_match_matrix) >= 3){
                                conf_matrix_tmp[[l+1]] <- caret::confusionMatrix(as.table(tot_match_matrix), 
                                                                                 positive = "Yes")
                            }
                            
                            else{
                                conf_matrix_tmp[[l+1]] <- "Insufficient number of events to calculate meaningful reliability estimates."
                            }
                            names(conf_matrix_tmp) <- c(tmp.codes, paste("Total", all.codes[c]))
                            rv$agree_list[[c]] <- tmp.agree
                            rv$kappa_list[[c]] <- tmp.kappa
                            rv$conf_matrix_list[[c]] <- conf_matrix_tmp
                        }
                        
                        #                  else{
                        #                      rv$DF_list[[c]] <- paste("No", all.codes[c], "behaviors coded")
                        #                      rv$total_list[[c]] <- paste("No", all.codes[c], "behaviors coded")
                        #                      rv$kappa_list[[c]] <- paste("No", all.codes[c], "behaviors coded")
                        #                      rv$conf_matrix_list[[c]] <- paste("No", all.codes[c], "behaviors coded")
                        #                      rv$agree_list[[c]] <- paste("No", all.codes[c], "behaviors coded")
                        #                  }
                    }
                }
            }
            #browser()
            names(rv$agree_list) <- all.codes
            names(rv$kappa_list) <- all.codes
            names(rv$DF_list) <- all.codes
            names(rv$conf_matrix_list) <- all.codes
            names(rv$raw_code_list) <- all.codes
            names(rv$total_list) <- all.codes
            #browser()
            removeModal()
        }
    })
    
    output$code_dat <- renderTable({
        tab <- NULL
        if(!is.null(rv$code_dat)){
            tab <- rv$code_dat
        }
        tab
    })
    
    output$tab_coder1 <- renderTable({
        tab <- NULL
        #browser()
        if(!is.null(rv$coder_1)){
            tab <- head(rv$dat1_coder.1[,1:3], n = 10)
            }
        tab
        })
    
    output$used_coder1 <- renderTable({
        #browser()
        tab <- NULL
        all.codes <- c(rv$duration_codes, 
                       rv$event_codes)
        
        if(!is.null(rv$coder_1)){
            used <- unique(rv$dat1_coder.1$Behavior)
            present <- vector()
            for(c in 1:length(all.codes)){
                if(sum(grepl(all.codes[c], used)) >= 1){
                    present[c] <- "Y"
                }
                else{
                    present[c] <- "N"
                }
            }
            tab <- data.frame(Code = all.codes, 
                              Type = c(rep("Duration", length(rv$duration_codes)), 
                                       rep("Event", length(rv$event_codes))), 
                              Used = present)
        }
        tab
    })
    
    output$tab_coder2 <- renderTable({
        tab <- NULL
        if(!is.null(rv$coder_2)){
            tab <- head(rv$dat1_coder.2[,1:3], n = 10)
        }
        tab
    })
    
    output$used_coder2 <- renderTable({
        #browser()
        tab <- NULL
        all.codes <- c(rv$duration_codes, 
                       rv$event_codes)
        
        if(!is.null(rv$coder_2)){
            used <- unique(rv$dat1_coder.2$Behavior)
            present <- vector()
            
            for(c in 1:length(all.codes)){
               
                if(sum(grepl(all.codes[c], used)) >= 1){
                    present[c] <- "Y"
                }
               
                else{
                    present[c] <- "N"
                }
            }
            
            tab <- data.frame(Code = all.codes, 
                              Type = c(rep("Duration", length(rv$duration_codes)), 
                                       rep("Event", length(rv$event_codes))), 
                              Used = present)
        }
        tab
    })
    
    view_plot<-eventReactive(input$plot_displ, {
        #browser()
        p <- ggplot()
        
        if(is.null(rv$agree_list)){
            text <- 'Data has not been selected'
            p <- p + annotate("text", x = 4, y = 25, size=8, label = text)
        }
        
        if(!is.null(rv$agree_list)){
            
            if(sum(rv$duration_codes == input$behav_displ)== 1){
                
                DF_tmp <- rv$DF_list[[which(names(rv$DF_list) == input$behav_displ)]]
                
                if(nrow(DF_tmp[DF_tmp$Value == 1,]) > 0){
                    col_vals_1 <- c(RColorBrewer::brewer.pal(7, "Blues")[7])
                    names(col_vals_1) <- rv$coder_1
                    
                    g.Grp_coder.1<-ggplot(data = DF_tmp[DF_tmp$Value == 1 & 
                                                            !is.na(DF_tmp$Value) &
                                                            DF_tmp$Coder == rv$coder_1,])+
                        geom_tile(aes(x = Time, 
                                      y = Behavior, 
                                      fill = Coder), 
                                  height = .75, 
                                  alpha = .5, 
                                  show.legend = FALSE)+
                        labs(title = paste("Play Codes Coded by:", 
                                           rv$coder_1), 
                             subtitle = paste("Target ID:", input$target_id), 
                             y = "Play Code", 
                             x = "Time (s)")+
                        scale_x_continuous(limits = c(rv$time.min-1, 
                                                      rv$time.max+1))+
                        scale_fill_manual(values = col_vals_1)+
                        theme_bw()
                    
                    col_vals_2 <- c(RColorBrewer::brewer.pal(7, "Reds")[7])
                    names(col_vals_2) <- rv$coder_2
                    
                    g.Grp_coder.2<-ggplot(data = DF_tmp[DF_tmp$Value == 1 & 
                                                            !is.na(DF_tmp$Value) &
                                                            DF_tmp$Coder == rv$coder_2,])+
                        geom_tile(aes(x = Time, 
                                      y = Behavior,
                                      fill = Coder), 
                                  height = .75, 
                                  alpha = .5, 
                                  show.legend = FALSE)+
                        labs(title = paste("Play Codes Coded by:", 
                                           rv$coder_2), 
                             subtitle = paste("Target ID:", input$target_id), 
                             y = "Play Code", 
                             x = "Time (s)")+
                        scale_x_continuous(limits = c(rv$time.min-1, 
                                                      rv$time.max+1))+
                        
                        scale_fill_manual(values = col_vals_2)+
                        theme_bw()
                    
                    g.Grp_comb<-ggplot(data = DF_tmp[DF_tmp$Value==1 & 
                                                         !is.na(DF_tmp$Value),])+
                        geom_tile(aes(x = Time, 
                                      y = Behavior, 
                                      fill = Coder, 
                                      group = Coder), 
                                  height = .75, 
                                  alpha = .5, 
                                  show.legend = FALSE)+
                        labs(title = paste("Overlapping Play Codes:", rv$coder_1, 
                                           "&", 
                                           rv$coder_2), 
                             caption = "Prepared using codeR helpR (v0.0.2) \n Matthew Barstead, Ph.D. (c) 2019", 
                             y = "Play Code", 
                             x = "Time (s)")+
                        scale_x_continuous(limits = c(rv$time.min-1, 
                                                      rv$time.max+1))+
                        scale_fill_manual(values = c(col_vals_1, col_vals_2))+
                        theme_bw()
                    
                    p <- cowplot::plot_grid(g.Grp_coder.1, 
                                            g.Grp_coder.2,
                                            g.Grp_comb,
                                            nrow = 3, 
                                            align = "v")
                    
                }
                else { 
                    text <- 'Neither coder coded this behavior.'
                    p <- p + annotate("text", x = 4, y = 25, size=8, label = text)
                }
            }
        }
        
        if(sum(rv$event_codes == input$behav_displ)== 1){
            DF_tmp <- rv$DF_list[[which(names(rv$DF_list) == input$behav_displ)]]
            
            if(nrow(DF_tmp[DF_tmp$Coder == rv$coder_1,]) >= 1){
                g.Grp_coder.1<-ggplot(data = DF_tmp[DF_tmp$Value == 1 & 
                                                        !is.na(DF_tmp$Value) &
                                                        DF_tmp$Coder == rv$coder_1,])+
                    geom_point(aes(x = Time, 
                                   y = Behavior, 
                                   color = Match), 
                               alpha = .5, 
                               size = 15)+
                    labs(title = paste("Play Codes Coded by:", 
                                       rv$coder_1), 
                         subtitle = paste("Target ID:", input$target_id), 
                         y = "Play Code", 
                         x = "Time (s)")+
                    scale_x_continuous(limits = c(rv$time.min-1, 
                                                  rv$time.max+1))+
                    labs(title = paste("Point Event Play Codes:", rv$coder_1), 
                         y = "Play Code", 
                         x = "Time (s)")+
                    scale_color_manual(values = c("Match" = RColorBrewer::brewer.pal(7, "Blues")[7], 
                                                  "No Match" = RColorBrewer::brewer.pal(7, "Reds")[7]))+
                    theme_bw()
            }
            
            if(nrow(DF_tmp[DF_tmp$Coder == rv$coder_1,]) == 0){
                text<-paste(rv$coder_1, "did not record this behavior")
                
                g.Grp_coder.1<-ggplot(data = DF_tmp[DF_tmp$Value == 1 & 
                                                        !is.na(DF_tmp$Value) &
                                                        DF_tmp$Coder == rv$coder_1,], 
                                      aes(x = Time))+
                    annotate("text", x = mean(DF_tmp$Time), 
                                  y = 5, size=10, label = text)+
                    theme_bw()
            }
            
            if(nrow(DF_tmp[DF_tmp$Coder == rv$coder_2,]) >= 1){
                g.Grp_coder.2<-ggplot(data = DF_tmp[DF_tmp$Value == 1 & 
                                                        !is.na(DF_tmp$Value) &
                                                        DF_tmp$Coder == rv$coder_2,])+
                    geom_point(aes(x = Time, 
                                   y = Behavior, 
                                   color = Match), 
                               alpha = .5, 
                               size = 15)+
                    labs(title = paste("Play Codes Coded by:", 
                                       rv$coder_2), 
                         subtitle = paste("Target ID:", input$target_id), 
                         y = "Play Code", 
                         x = "Time (s)")+
                    scale_x_continuous(limits = c(rv$time.min-1, 
                                                  rv$time.max+1))+
                    labs(title = paste("Point Event Play Codes:", rv$coder_2), 
                         caption = "Prepared using codeR helpR (v0.0.2) \n Matthew Barstead, Ph.D. (c) 2019", 
                         y = "Play Code", 
                         x = "Time (s)")+
                    scale_color_manual(values = c("Match" = RColorBrewer::brewer.pal(7, "Blues")[7], 
                                                  "No Match" = RColorBrewer::brewer.pal(7, "Reds")[7]))+
                    theme_bw()
            }
            
            if(nrow(DF_tmp[DF_tmp$Coder == rv$coder_2,]) == 0){
                text<-paste(rv$coder_2, "did not record this behavior")
                
                g.Grp_coder.2<-ggplot(data = DF_tmp[DF_tmp$Value == 1 & 
                                                        !is.na(DF_tmp$Value) &
                                                        DF_tmp$Coder == rv$coder_2,], 
                                      aes(x = Time))+
                    annotate("text", x = mean(DF_tmp$Time), 
                             y = 5, size=10, label = text)+
                    labs(title = paste("Point Event Play Codes:", rv$coder_2), 
                         caption = "Prepared using codeR helpR (v0.0.2) \n Matthew Barstead, Ph.D. (c) 2019", 
                         y = "Play Code", 
                         x = "Time (s)")+
                    theme_bw()
            }
            
            p <- cowplot::plot_grid(g.Grp_coder.1, 
                                    g.Grp_coder.2,
                                    nrow = 2, 
                                    align = "v")
        }
        p
    })
    
    output$plot <- renderPlot({
        view_plot()
    })
    
    print_matrix <- eventReactive(input$plot_displ, {
        #browser()
        if(is.null(input$behav_displ)){
            tmp <- "No data selected or code set targeted"
        }
        
        if(sum(rv$family_codes == input$behav_displ) == 1){
            tmp <- rv$conf_matrix_list[[which(names(rv$conf_matrix_list) == input$behav_displ)]]
            if(length(tmp) > 1){
                tmp <- tmp[names(tmp) == paste("Total", input$behav_displ)]
                tmp <- tmp[[paste("Total", input$behav_displ)]]
            }
        }
        
        if(sum(rv$family_codes == input$behav_displ) == 0){
            tmp <- rv$conf_matrix_list[[which(names(rv$conf_matrix_list) == input$behav_displ)]]
        }
        
        if(sum(rv$duration_codes == input$behav_displ) == 1 & length(tmp) > 1){
            tmp$table <- tmp$table/100
        }
        
        tmp
    })
    
    output$conf_matrix_txt <- renderPrint({
        print_matrix()
    })
    
    observeEvent(input$save, {
        #browser()
        if(is.null(rv$dat1) | is.null(rv$agree_list)){
            showModal(modalDialog(title = "Error", 
                                  "Message: You do not seem to have loaded and/or processed any data yet."))
        }
        
        else if(is.null(input$target_id)){
            showModal(modalDialog(title = "Error", 
                                  "Message: You must specify a case ID. Do so and re-compute reliabilities, then save."))
        }
        
        else if(!is.null(rv$dat1) & 
                !is.null(rv$agree_list) & 
                !is.null(input$target_id)){
            
            save.image(file = paste0(paste0(rv$wd, 
                                            '/',
                                            input$target_id), 
                                     '.RData'))
        }
        
        else { 
            showModal(modalDialog(title = "Error", 
                                  "Something is not working. Check your settings carefully."))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
