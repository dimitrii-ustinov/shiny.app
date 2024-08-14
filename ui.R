
shinyUI(
  fluidPage(
    useShinyjs(),
    #Background
    tags$head(
      tags$style(HTML("
        body {
          background-image: url('background.jpg');
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          background-attachment: fixed;
          
        }
        .tab-panel {
          background-color: rgba(0, 0, 0, 0.7); 
          padding: 20px;
          border-radius: 10px;
        }
        
        h3 {
          color: #003366; 
        }
        .well {
          background-color: rgba(255, 255, 255, 0.6); 
        }
        .dataTable {
          background-color: rgba(0, 0, 0, 0.5); 
        }
      "))
    ),
    
    titlePanel("Assignment 2"),
    
    titlePanel("Student: Dimitrii Ustinov,
               StudentID: 34164880"),
    

   
       tabsetPanel(
                                ####EDA####
        tabPanel("EDA",
          tabsetPanel(
            
                                #####Table#####
            tabPanel("Table",
                     h3("Table overview"),
                     tabsetPanel(
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "eda_table_input_filter1", "Filter type", choices=c("none","top"), selected = "none"),
                           checkboxInput("eda_table_input_show_summary", "Show Summary", value = FALSE),
                           width = 4
                         ),
                         mainPanel(
          
                           tabPanel("Raw Data",
                                    DT::dataTableOutput(outputId = "eda_table_output_table1"),
                                    
                                    verbatimTextOutput(outputId = "eda_table_output_summary1")
                           )
                         )
                     )
                )
            ),
            
            #####Mosaic#####
            
            tabPanel("Mosaic",
                     h3("Mosaic plot of discrete variables"),
                     tabsetPanel(
                       sidebarLayout(
                         sidebarPanel(
                           selectizeInput(inputId = "eda_mosaic_input_show_variables", label = "Show variables:", choices = choicesA, 
                                          multiple = TRUE, selected = choicesA),
                                          #checkboxInput(inputId = "ShowPValues", label = "Show p-values", value = FALSE),
                                          width = 4
                           ),
                         mainPanel(
      
                           tabPanel("Visualisation",
                                    
                                    withSpinner(
                                      plotOutput(outputId = "eda_mosaic_output_plot")
                                    ),
                                    #verbatimTextOutput(outputId = "PValueOutput")
                           )
                         )
                     )
                )
            ),
            
            #####Pairs#####
            tabPanel("Pairs plot",
                     h3("Pairs plot"),
                     tabsetPanel(
                       sidebarLayout(
                         sidebarPanel(
                           selectizeInput(inputId = "eda_pairs_input_choosevar",
                                          label = "Choose variables for pairs plot:",
                                          choices = setdiff(colnames(original_data), "CODE"),
                                          multiple = TRUE,
                                          selected = c("POPULATION", "GDP")),
                           selectInput(inputId = "eda_pairs_input_coloring", label = "Coloring factor",
                                       choices = choicesA, selected = "HEALTHCARE_BASIS"),
                           width = 4
                         ),
                         mainPanel(
                           tabPanel("Visualisation", 
                                    
                                    withSpinner(
                                      plotOutput(outputId = "eda_pairs_output_plot")
                                    )
                                  )
                         )
                     )
                )
            ),
            
            #####Corrgram#####
            tabPanel("Corrgram",
                     h3("Pairs plot"),
                     tabsetPanel(
                       sidebarLayout(
                         sidebarPanel(checkboxInput(inputId = "eda_corrgram_input_use_absolute", label = "Use absolute correlation", value = TRUE),
                                      selectInput(inputId = "eda_corrgram_input_method", label = "Correlation method",
                                                  choices = c("pearson","spearman","kendall"), selected = "pearson"),
                                      selectInput(inputId = "eda_corrgram_input_grouping", label = "Grouping method",
                                                  choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"),
                                                  selected = "HC"),
                                      
                                      width = 4
                         ),
                         mainPanel(
                           tabPanel("Visualisation", 
                                    
                                    hr(),
                                    withSpinner(
                                      plotOutput(outputId = "eda_corrgram_output_plot")
                                    ),
       
                           )
                         )
                     )
                )
            ),
            
            #####Missing data#####
            tabPanel("Missing values",
                     h3("Missing"),
                     tabsetPanel(
                       sidebarLayout(
                         sidebarPanel(checkboxInput(inputId = "eda_missing_input_cluster", label = "Cluster missingness", value = FALSE),
                                      checkboxInput(inputId = "eda_missing_input_sort", label = "Sort missingness", value = FALSE),
                                      width = 4
                                      ),
                         mainPanel(
                           tabPanel("Visualisation", 
                                    hr(),
                                    withSpinner(
                                      plotOutput(outputId = "eda_missing_output_plot")
                                    )
                                    
                           )
                         )
                     )
                )
            ),
            #####Boxplot#####
            tabPanel("Boxplot",
                     h3("Plot name"),
                     tabsetPanel(
                       sidebarLayout(
                         sidebarPanel(
                           checkboxInput(inputId = "eda_boxplot_input_standardise", label = "Show standardized", value = FALSE),
                           checkboxInput(inputId = "eda_boxplot_input_outliers", label = "Show outliers", value = TRUE),
                           sliderInput(inputId = "eda_boxplot_input_range", label = "IQR Multiplier",
                                       min = 0, max = 5, step = 0.1, value = 1.5),
                           width = 4
                         ),
                         mainPanel(
                           tabPanel("Visualisation", 
                                    withSpinner(
                                      plotOutput(outputId = "eda_boxplot_output_plot")
                                    )
                                    
                                    
                           )
                         )
                     )
                )
            ),
            
            
            #####Raising value#####
            tabPanel("Raising",
                     h3("Raising value"),
                     tabsetPanel(
                       sidebarLayout(
                         sidebarPanel(
                           checkboxInput(inputId = "eda_raising_input_standardised", label = "Show standardized", value = FALSE),
                           width = 4
                           
                         ),
                         mainPanel(
                           tabPanel("Visualisation",
                                    withSpinner(
                                      
                                      plotlyOutput(outputId = "eda_raising_output_plot")
                                     )
                                    
                            )
                         )
                      )
                  )
            ),
        )
     
    ),
    
                              ####DATA PROCESSING####
    tabPanel("Data Processing",
             tabsetPanel(
               #####Controllers#####
               
                        sidebarLayout(
                                    sidebarPanel(
                                      h3("Common Controls"),
                                      sliderInput(inputId = "VarThreshold", label = "Threshold of variable missingness", 
                                                  min = 1, max = 100, value = 50, post = "%"),
                                      sliderInput(inputId = "ObsThreshold", label = "Threshold of observations missingness",
                                                  min = 1, max = 100, value = 50, post = "%"),
                                      checkboxInput(inputId = "cleaning_missing_input_shadow", label = "Add Missingness Shadow Variables", value = FALSE),
                                      selectizeInput(inputId = "cleaning_common_input_delete_cols", label = "Omit columns", choices = NULL, 
                                                     multiple = TRUE, selected = NULL),
                                      selectizeInput(inputId = "cleaning_common_input_delete_rows", label = "Omit rows", choices = NULL, 
                                                     multiple = TRUE, selected = NULL),
                                      
                                      
                          ),
                          mainPanel(
                            tabsetPanel(
               
                  #####Table after changes#####
                                     tabPanel("Table",
                                              h3("Table after changes applied"),
                                              selectInput(inputId = "cleaning_view_input_filter1", "Filter type", choices=c("none","top"), selected = "none"),
                                              DT::dataTableOutput(outputId = "cleaning_view_output_table1"),
                                              
                                              checkboxInput("cleaning_table_input_show_summary", "Show Summary", value = FALSE),
                                              verbatimTextOutput(outputId = "cleaning_table_output_summary"),
                                              ),
                  
                  
                  #####Missing Data Patterns#####
                                     tabPanel("Missing Data Patterns",
                                              h3("Missing Data Pattens"),
                                              #Model parameters
                                              selectInput(inputId = "cleaning_missing_input_patterns", label = "Visualisation method",
                                                          choices = list("vismiss" = "vismiss","nanair" = "nanair"),
                                                          selected = "nanair"),
                                              #Values
                                               withSpinner(
                                                 plotOutput(outputId = "cleaning_missing_output_patterns")
                                               ),
                                              # sliderInput(inputId = "cleaning_missing_input_nsets", label = "Number of variables",
                                              #             min = 0, max = ncol(original_data), step = 1, value = 5),
                                              #Adjustments
                                              conditionalPanel(
                                                condition = "input.cleaning_missing_input_patterns == 'nanair'",
                                                sliderInput(inputId = "cleaning_missing_input_nsets", label = "Number of variables",
                                                            min = 0, max = ncol(original_data), step = 1, value = 5)
                                              ),
                                              conditionalPanel(
                                                condition = "input.cleaning_missing_input_patterns == 'vismiss'",
                                                # sliderInput(inputId = "cleaning_missing_input_nsets", label = "Number of variables",
                                                #             min = 0, max = ncol(original_data), step = 1, value = 5)
                                                checkboxInput(inputId = "cleaning_missing_input_cluster", label = "Cluster missingness", value = FALSE),
                                                checkboxInput(inputId = "cleaning_missing_input_sort", label = "Sort missingness", value = FALSE),
                                              ),
                                     ),
                            

                  #####Predict informative missingness#####
                                     tabPanel("Informative missingness",
                                              h3("Predict Informative Missingness"),
                                              withSpinner(
                                                plotOutput(outputId = "cleaning_predict_output_tree")
                                              ),
                                     ),

                  
                  #####Prepare and train the model, plot residuals#####
                                    tabPanel("Model Training",
                                             
                                             #Model parameters
                                             h3("Model Parameters"),
                                             selectInput(inputId = "cleaning_model_input_naValues", label = "Deal with missing values",
                                                         choices = list("Omit NA" = "omitNA","Impute using KNN" = "KNNimpute", "Bagged trees imputation" = "bagImpute"),
                                                         selected = "omitNA"),
                                             checkboxInput(inputId = "cleaning_model_input_center", label = "Center", value = FALSE),
                                             checkboxInput(inputId = "cleaning_model_input_scale", label = "Scale", value = FALSE),
                                             # checkboxInput(inputId = "cleaning_model_input_yeojohnson", label = "Yeo-Johnson Transformation", value = FALSE),
                                             actionButton(inputId = "cleaning_model_input_trainButton", label = "Train", icon = icon("play")),
                                             
                                             # Line separator
                                             tags$hr(style = "border-top: 1px solid #A9A9A9;"),
                                             
                                             # Checkboxes
                                             div(style = "display: flex; justify-content: space-between;",
                                                 checkboxInput(inputId = "cleaning_model_input_show_summary", label = "Show Model Summary", value = TRUE),
                                                 checkboxInput(inputId = "cleaning_model_input_training_RMSE", label = "Training RMSE-Lambda", value = FALSE),
                                                 checkboxInput(inputId = "cleaning_model_input_training_importance", label = "Variable Importance", value = FALSE)
                                             ),
                                             
                                             
                                             
                                             #Values
                                             conditionalPanel(
                                               condition = "input.cleaning_model_input_show_summary == true",
                                                 withSpinner(
                                                  #cat("Model Summary"),
                                                  verbatimTextOutput(outputId = "cleaning_train_output_residual_summary")
                                                 ),
                                             ),
                                             conditionalPanel(
                                               condition = "input.cleaning_model_input_training_RMSE",
                                               withSpinner(
                                                 plotOutput(outputId = "cleaning_model_output_training_RMSE")
                                               ),
                                             ),
                                             conditionalPanel(
                                               condition = "input.cleaning_model_input_training_importance",
                                               withSpinner(
                                                 plotOutput(outputId = "cleaning_model_output_training_importance")
                                               ),
                                             ),
                                    ),
                  
                                    tabPanel("Model Testing",id = "cleaning_model_test_tab",
                                             
                                             #Model parameters
                                             h3("Button"),
                                            
                                             actionButton(inputId = "cleaning_model_input_testButton", label = "Test", icon = icon("exclamation-circle")),
                                             
                                             # Line separator
                                             tags$hr(style = "border-top: 1px solid #A9A9A9;"),
                                             h3("Predictions"),
                                             # Checkboxes
                                             verbatimTextOutput(outputId = "cleaning_test_output_residual_summary"),
                                             div(style = "display: flex; justify-content: space-between;",
                                                 checkboxInput(inputId = "cleaning_model_input_testing_ResPred", label = "Test Residuals vs Predicted", value = FALSE),
                                                 checkboxInput(inputId = "cleaning_model_input_testing_actual_pred", label = "Actual vs Predicted", value = FALSE),
                                             ),
                                             
                                             
                                             #Values
                                            
                                             conditionalPanel(
                                               condition = "input.cleaning_model_input_testing_ResPred",
                                               withSpinner(
                                                 plotOutput(outputId = "cleaning_model_output_testing_ResPred")
                                               ),
                                             ),
                                             conditionalPanel(
                                               condition = "input.cleaning_model_input_testing_actual_pred",
                                               withSpinner(
                                                 plotOutput(outputId = "cleaning_model_output_testing_actual_pred")
                                               ),
                                             ),
                                             
                                             
                                    ),
                  
                                    #####Residual Outliers#####
                  
                                    tabPanel("Residual Outliers",
                                             h3("Residual Outliers"),
                                            
                                             #Values

                                             div(style = "display: flex; justify-content: space-between;",
                                                 checkboxInput(inputId = "cleaning_model_input_outliers_test", label = "Test", value = FALSE),
                                                 checkboxInput(inputId = "cleaning_model_input_outliers_train", label = "Train", value = FALSE),
                                             ),
                                             
                                             #Values
                                             
                                             conditionalPanel(
                                               condition = "input.cleaning_model_input_outliers_test == true",
                                               withSpinner(
                                                 plotOutput(outputId = "cleaning_model_output_outliers_box_test")
                                               ),
                                             ),
                                             conditionalPanel(
                                               condition = "input.cleaning_model_input_outliers_train == true",
                                               withSpinner(
                                                 plotOutput(outputId = "cleaning_model_output_outliers_box_train")
                                               ),
                                             ),
                                             
                                             sliderInput(inputId = "cleaning_model_input_testing_outliers_iqr", label = "IQR",
                                                         min = 0, max = 5, step = 0.5, value = 1.5
                                                         ),
                                             checkboxInput(inputId = "cleaning_model_input_show_probability", label = "Probability of occurence of outliers", value = FALSE),
                                             verbatimTextOutput(outputId = "cleaning_table_output_outlier_probability"),
                                    ),
                                             
                                    
                        ) #tabsetpanel
               
              
                
              ) #tabsetPanel in Cleaning
                        ))
           ) #TabPanel in cleaning
        ) #TabsetPanel for both pages
    ) #FluaidPage
) #ShinyUI
