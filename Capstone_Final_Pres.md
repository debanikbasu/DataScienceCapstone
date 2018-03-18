Text Prediction using N-Gram Language Model
========================================================
author: Debanik Basu
date: March 2018
autosize: true

Introduction
========================================================

The goal of this project was to develop a text prediction model using a N-Gram language model. The project has been completed in the following stages:

- Research on the N-Gram Language Processing capabilities of R
- Acquire, pre-process and clean the text data
- Perform exploratory analysis
- Develop N-Grams
- Create predictive models
- Develop data product
- Create presentation


Building the model
========================================================

The following steps were followed while developing the predictive text model:

- A text corpus was created using a reasonably sized sample from the twitter, news and blogs data sets
- The corpus data was processed and cleaned by removing numbers, punctuations, multi-character whitespaces and profane words
- Various N-Grams and TermDocument matrices were created from this corpus and stored in individual files
- Various options were explored to improve the prediction speed and accuracy

The final model
========================================================

- The stupid back-off strategy was used in the final model
- Based on the 'n' words typed by the user, the corresponing N-grams were chosen. Also, the most frequent N-Grams based on the last 'n-1' words entered by the user was also displayed. This has better results and accuracy 
- To improve speed and reduce the complexity of the model, the N-Grams were chosed based on specific frequency thresholds

Shiny App
========================================================

- The Shiny App which was developed provides a text input box for users to type in their words/sentences
- The app detects the typed words and predicts the next word reactively 
- Along with the most likely prediction, the app also displays other suggested words based on the probabilities/frequencies

Resources
========================================================

- App Link: https://debanikbasu.shinyapps.io/capstoneproject/
- Github Repository: 
