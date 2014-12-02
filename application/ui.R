shinyUI(
     navbarPage(
                tags$style(type='text/css',
                "h1 {
                        font-family: Verdana, Geneva, sans-serif;
                        font-weight: 800;
                        line-height: 1.1;
                        color: #011C26;
                }
                
                h2 {
                        font-family: Verdana, Geneva, sans-serif;
                        font-weight: 600;
                        line-height: 1.1;
                        color: #588C8C;
                }
                
                h3 {
                        font-family: Verdana, Geneva, sans-serif;
                        font-weight: 480;
                        line-height: 1.0;
                        color: #00495C;
                }
                                
                h4 {
                        font-family: Verdana, Geneva, sans-serif;
                        font-weight: 510;
                        line-height: 1.1;
                        color: #011C26;
                }
                
                h5 {
                        font-family: Verdana, Geneva, sans-serif;
                        font-weight: 530;
                        line-height: 1.2;
                        color: #D9042B;
                }"),  
                
                tabPanel(h3("Word Prediction Application"),
                            
                           fluidRow(
                                   column(4,
                                          textInput("caption", h4("Type into the box:"), "What we do in"),
                                          tags$style(type='text/css', "#caption { width: 450px; }")
                                          ),
                                   column(5,
                                          h4('The text typed is:'),
                                          h5(textOutput("caption")),
                                          br(),
                                          h4('The predicted word is:'),
                                          h3(textOutput("predicted_word")),
                                          br(),
                                          h4('The N-Gram used for prediction is:'),
                                          h5(textOutput("ngram_used"))
                                          ),
                                   column(3,
                                          "This is the Final Project for ",   
                                          a("Coursera Data Science Capstone", href = "http://www.coursera.org/course/dsscapstone"),
                                          br(),
                                          "Author: ",a("Marius Radu", href = "mailto:radu_marius_florin@yahoo.com"),
                                          br(),
                                          a("GitHub Source Code", href = "https://github.com/radmar2002/TapNGram"),
                                          br(),
                                          "Date: 02 December 2014"
                                   )
                           )
                            
                            
                )
                
                 ,tabPanel(h3("Language Model Description"), includeMarkdown("D:/WORK_2014/Certification_Data_Science/Data_Science_Capstone/TapNGram/application/help_lang_model.Rmd"))
                
))
