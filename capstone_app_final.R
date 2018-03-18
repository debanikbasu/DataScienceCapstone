library(shiny)
library(stringr)
library(tm)
library(markdown)

### Loading the bigram, trigram and quadgram matrices
bg_data <- readRDS("myBigram.RData")
tg_data <- readRDS("myTrigram.RData")
qd_data <- readRDS("myQuadgram.RData")

### Splitting the text field in the matrices into columns and renaming them

# For Bigrams
split_text_bg <- str_split_fixed(as.character(bg_data$Text), " ", 2)
bg_data <- cbind(split_text_bg[,1],split_text_bg[,2],bg_data)[,-3]
names(bg_data) <- c("w1","w2","freq")

# For Trigrams
split_text_tg <- str_split_fixed(as.character(tg_data$Text), " ", 3)
tg_data <- cbind(split_text_tg[,1],split_text_tg[,2],split_text_tg[,3],tg_data)[,-4]
names(tg_data) <- c("w1","w2","w3","freq")

# For Quadgrams
split_text_qd <- str_split_fixed(as.character(qd_data$Text), " ", 4)
qd_data <- cbind(split_text_qd[,1],split_text_qd[,2],split_text_qd[,3],split_text_qd[,4],qd_data)[,-5]
names(qd_data) <- c("w1","w2","w3","w4","freq")

### Function for predicting the next word

predNextWord <- function(inp_word) {
      inp_word <- stripWhitespace(removeNumbers(removePunctuation(tolower(inp_word),preserve_intra_word_dashes = TRUE)))
      inp_word <- strsplit(inp_word, " ")[[1]]
      
      ## check for Bigram
      if (length(inp_word) == 1) {
            inp_word <- as.character(tail(inp_word,1))
            checkforBigram(inp_word)
      }
      
      ## check for Trigram
      else if (length(inp_word) == 2) {
            inp_word <- as.character(tail(inp_word,2))
            checkforTrigram(inp_word)
      }
      
      ## check for Quadgram
      else if (length(inp_word) >= 3) {
            inp_word <- as.character(tail(inp_word,3))
            checkforQuadgram(inp_word)
      }
      
}

## Create the Bigram, Trigram and Quadgram functions

checkforBigram <- function(inp_word) {
      
      #Check if there are any bigrams available
      if (identical(character(0),as.character(head(bg_data[bg_data$w1 == inp_word[1], 2], 1)))) {
            # No predicted words found yet.
            as.character("NA")
      }
      else {
            # Trying to predict the word using the bigram matrix
            bgr_out <- matrix(as.character(head(bg_data[bg_data$w1 == inp_word[1],2], 5)))
            bgr_out
      }
}


checkforTrigram <- function(inp_word) {
      
      #Check if there are any trigrams available
      if (identical(character(0),as.character(head(tg_data[tg_data$w1 == inp_word[1]
                                                      & tg_data$w2 == inp_word[2], 3], 1)))) {
            # No trigrams found. Using the last typed word to check for bigrams.
            as.character(predNextWord(inp_word[2]))
            
      }
      else {
            # Trying to predict the word using trigram matrix
            trg_out <- matrix(as.character(head(tg_data[tg_data$w1 == inp_word[1]
                                                   & tg_data$w2 == inp_word[2], 3], 5)))
            # Combining the trigram results with the most frequent bigram results
            bgr_out <- matrix(as.character(head(bg_data[bg_data$w1 == inp_word[2], 2], 5)))
            unique(rbind(trg_out,bgr_out))
            
      }
}

checkforQuadgram <- function(inp_word) {
     
      #Check if there are any quadgrams available
      if (identical(character(0),as.character(head(qd_data[qd_data$w1 == inp_word[1]
                                                      & qd_data$w2 == inp_word[2]
                                                      & qd_data$w3 == inp_word[3], 4], 1)))) {
            # No quadgrams found. Using the last two typed words to check for trigrams.
            as.character(predNextWord(paste(inp_word[2],inp_word[3],sep=" ")))
      }
      else {
            # Trying to predict the word using quadgram matrix
            quad_out <- matrix(as.character(head(qd_data[qd_data$w1 == inp_word[1] 
                                          & qd_data$w2 == inp_word[2]
                                          & qd_data$w3 == inp_word[3], 4], 5)))
            # Combining the quadgram results with the most frequent trigram results
            trg_out <- matrix(as.character(head(tg_data[tg_data$w1 == inp_word[2]
                                 & tg_data$w2 == inp_word[3], 3], 5)))
            unique(rbind(quad_out,trg_out))
            
      }       
}


### Create the UI elements

ui <- fluidPage(
            titlePanel("DATA SCIENCE CAPSTONE:"),
            h3("WORD PREDICTION USING N-GRAM LANGUAGE MODEL"),
            br(),
            br(),
            sidebarLayout(
                  sidebarPanel(
                        textInput("inputText", "ENTER YOUR TEXT / WORD HERE",value = ""),
                        hr(),
                        h5("Type in a word or sentence in the text box above. The predictions and suggested words will be displayed in the main panel."),
                        br(),
                        h6("Note: No need to press the spacebar or return key.")
                  ),
                  mainPanel(
                        h5(strong("PREDICTED WORD")),
                        HTML('<div style=background-color:#0097A7;padding:5px;color:white;width:150px>'),
                        textOutput('pred_word'),
                        HTML('</div>'),
                        br(),
                        h5(strong("OTHER SUGGESTIONS")),
                        HTML('<div style=background-color:#E0F7FA;width:150px;color:black>'),
                        tableOutput('pred_table'), 
                        tags$style('#pred_table td{border:0px;}'),
                        HTML('</div>'),
                        br(), br()
                  )
            )
      )


## Shiny Server Code
server <- function(input, output) {
      
      output$pred_table <- renderTable({
            result <- predNextWord(input$inputText)
            if(input$inputText == "" || result =="NA"){
                  output$pred_word <- renderText("No predictions")
                  result <- "No suggestions"
                  result
            }
            else {
                  # The first word in the return table is the best prediction
                  output$pred_word <- renderText({result[1]})
                  # The remaining words in the result table is provided as suggestions
                  result[-1]
            }
            
      },colnames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)