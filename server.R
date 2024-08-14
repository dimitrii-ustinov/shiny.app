shinyServer(function(input, output, session) {
  set.seed(11)
  
 
  ####EDA####
      ######Table#####
  # 
  
  output$eda_table_output_table1 <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(original_data),
                  filter = list(position = input$eda_table_input_filter1),
                  style = "Bootstrap"
                  )
  })
  output$eda_table_output_summary1 <- renderPrint({
    if (input$eda_table_input_show_summary) {
      cat("Summary of Dataset:\n")
      cat("Number of Variables:", ncol(original_data), "\n")
      cat("Number of Observations:", nrow(original_data), "\n\n")
      summary(original_data)
    }
  })
  
  
      ######Mosaic######
  
  output$eda_mosaic_output_plot <- renderPlot({
    selected_variables <- head(input$eda_mosaic_input_show_variables)
    formula <- as.formula(paste("~",paste(selected_variables, collapse = " + ")))
    vcd::mosaic(formula = formula, data = original_data, main = "Mosaic plot", legend = TRUE, shade = TRUE)
  })
  # output$PValueOutput <- renderPrint({
  #   if (input$ShowPValues) {
  #     p_value_matrix
  #   }
  # })
  
 
      ######Pairs######

  output$eda_pairs_output_plot <- renderPlot({
    selected_vars <- input$eda_pairs_input_choosevar
    selected <- original_data[, selected_vars, drop = FALSE]
    color <- original_data[[input$eda_pairs_input_coloring]]
    GGally::ggpairs(data = selected,  mapping = aes(colour = color))+
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16)) +
            ggplot2::labs(title = "Pairs plot")
  })
  
      ######Corrgram######
  
  output$eda_corrgram_output_plot <- renderPlot({
    corr_method_label <- switch(input$eda_corrgram_input_method,
                                "pearson" = "Pearson",
                                "spearman" = "Spearman",
                                "kendall" = "Kendall")
    
    plot_title <- sprintf("Correlation of factors using %s correlation", corr_method_label)
    
    


    corrgram(num_data,
             order = input$eda_corrgram_input_grouping,
             abs = input$eda_corrgram_input_use_absolute,
             cor.method = input$eda_corrgram_input_method,
             lower.panel=panel.shade,
             upper.panel=panel.pie,
             text.panel = panel.txt,
             main = plot_title)
  })
  
 
  
     ######Missing data######
  
  output$eda_missing_output_plot <- renderPlot({


        vis_miss(original_data, cluster = input$eda_missing_input_cluster, sort_miss = input$eda_missing_input_sort) +
          labs(title = "Missingness of the data")
      })
  
 
  
      ######Boxplot######
  
  output$eda_boxplot_output_plot <- renderPlot({
    data1 <- as.matrix(num_data)
    data1 <- scale(data1, center = input$eda_boxplot_input_standardise, scale = input$eda_boxplot_input_standardise)
    car::Boxplot(y = data1, ylab = 'Sensor Readings', use.cols = TRUE, notch = FALSE, varwidth = FALSE,
            horizontal = FALSE, outline = input$eda_boxplot_input_outliers,
            col = brewer.pal(n = dim(original_data)[2], name = "RdBu"),
            range = input$eda_boxplot_input_range, main = "Boxplots of Numeric Variables",
            id = ifelse(input$eda_boxplot_input_outliers, list(n = Inf, location = "avoid"), FALSE))



  })
 
    ######Rising-value chart######
  output$eda_raising_output_plot <- renderPlotly({
    
    
    
    num_data1 <- num_data
    for (col in 1:ncol(num_data1)) {
      num_data1[, col] <- num_data1[order(num_data1[, col]), col] 
    }
    num_data1 <- scale(x = num_data1, center = input$eda_raising_input_standardised, scale = input$eda_raising_input_standardised)
    p <- plot_ly()
    
    for (i in 1:ncol(num_data1)) {
      p <- add_lines(p, x = seq(1, 100, length.out = nrow(num_data1)), y = num_data1[, i],
                     name = colnames(num_data1)[i])
    }
    
    p <- p %>% layout(
      title = "Raising value plot",
      xaxis = list(title = "Order"),
      yaxis = list(title = "Value")
    )
    
    p
  })
  ####Processing####
  
    ######Go reactive######
  
  getData <- reactive({
    #setwd("P:\\Downloads\\DATA423\\Assignment 2\\Assignment2")
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    data  <- read.csv("Ass2Data.csv",na.strings = c("NA","N/A"), header = TRUE, stringsAsFactors = TRUE)
    # Standardize the missing values
    data[data == -99] <- NA
    data$CODE <- as.character(data$CODE)
    data$GOVERN_TYPE <- as.character(data$GOVERN_TYPE)
    data[data ==  "--"] <- NA
    data$GOVERN_TYPE <- as.factor(data$GOVERN_TYPE)
    data$HEALTHCARE_COST_shadow <- as.numeric(is.na(data$HEALTHCARE_COST))
    data$HEALTHCARE_COST_shadow[is.na(data$num)] <- 0
    data$HEALTHCARE_COST[is.na(data$HEALTHCARE_COST)] <- 0 
    #Created using ChatGPT
    #________________
    if (input$cleaning_missing_input_shadow == TRUE){
      for (col_name in names(data)) {
        # if (col_name == "HEALTHCARE_COST") {
        #   next
        # }
        # Check if the column contains any missing values
        if (any(is.na(data[[col_name]]))) {
          shadow_column <- ifelse(is.na(data[[col_name]]), 1, 0)
          data[[paste0("shadow_", col_name)]] <- shadow_column
        }
      }
    }
    #________________
    return(data)
  })
  
  getCleanData1 <- reactive({
    # remove excessively missing Vars
    data <- getData()
    vRatio <- apply(X = data, MARGIN = 2, FUN = missing_proportion)
    data[, vRatio < input$VarThreshold]
    
  })  
  
  getCleanData2 <- reactive({
    # remove excessively missing Obs
    data <- getCleanData1()
    oRatio <- apply(X = data, MARGIN = 1, FUN = missing_proportion)
    data[oRatio < input$ObsThreshold, ]
   
  })  
  
  
  getCleanData3 <- reactive({
    data <- getCleanData2()
    data$idNum <- as.numeric(str_extract(data$CODE, "\\d+"))
    data$CODE <- NULL
    data
  })
  
  
  getNumericData <- reactive({
    data <- getCleanData3()
    idnum <- data$idNum
    idnum
  })
  
  getColumnNames <- reactive({
    data <- getCleanData3()
    col_names <- colnames(as.data.frame(data))
    col_names
  })
  

  observe({
    updateSelectizeInput(session, "cleaning_common_input_delete_cols",
                         choices = getColumnNames())
    updateSelectizeInput(session, "cleaning_common_input_delete_rows",
                         choices = getNumericData())
    # updateSliderInput(session, "cleaning_missing_input_nsets",
    #                   max = length(getColumnNames()))
  })
  
  #Created with help of ChatGPT
  #________________
  getOmitColumn <- reactive({
    data <- getCleanData3()
    selected_cols <- input$cleaning_common_input_delete_cols
    
    if (is.null(selected_cols) || length(selected_cols) == 0) {
      data
    }
    
    # Filter data to exclude selected IDs
    data <- data[, !colnames(data) %in% selected_cols, drop = FALSE ]
    data
  })
  #________________
  

  getFilteredData <- reactive({
    data <- getOmitColumn()
    selected_ids <- input$cleaning_common_input_delete_rows
    
    if (is.null(selected_ids) || length(selected_ids) == 0) {
      data
    }
    
    # Filter data to exclude selected IDs
    data <- data[!data$idNum %in% as.numeric(selected_ids), ]
    data
  })

  
 
  
  ######Reactive table######
  output$cleaning_view_output_table1 <- DT::renderDataTable({
    DT::datatable(
        data = as.data.frame(getFilteredData()),
        filter = list(position = input$cleaning_view_input_filter1),
        style = "Bootstrap"
    )
  })
  
  output$cleaning_table_output_summary <- renderPrint({
    if (input$cleaning_table_input_show_summary) {
      cat("Summary of Dataset:\n")
      cat("Number of Variables:", ncol(getFilteredData()), "\n")
      cat("Number of Observations:", nrow(getFilteredData()), "\n\n")
      summary(getFilteredData())
    }
  })
  
  
  ######Missing data in cleaning######
  
  output$cleaning_missing_output_patterns <- renderPlot({
    if (input$cleaning_missing_input_patterns == "nanair") {
      
      naniar::gg_miss_upset(data = getFilteredData(), nsets = input$cleaning_missing_input_nsets) 
    }
    else if (input$cleaning_missing_input_patterns == "vismiss"){
      visdat::vis_miss(getFilteredData(),cluster = input$cleaning_missing_input_cluster, sort_miss = input$cleaning_missing_input_sort) #+
         #  labs(title = paste("Thresholds VarMiss:", input$VarThreshold, "ObsMiss:", input$ObsThreshold))
      
    }

    
  })
  

  ######Predictive missingness######
  output$cleaning_predict_output_tree <- renderPlot({
  data <- getFilteredData()
  #data$idNum <- as.numeric(str_extract(data$CODE, "\\d+"))
  data$missingness <- apply(X = is.na(data), MARGIN = 1, FUN = sum)

  tree <- caret::train(missingness ~ .-OBS_TYPE,
                       data = data,
                       method = "rpart",
                       na.action = na.rpart) # na.rpart means "rpart will deal with missing predictors intrinsically"
  
  rpart.plot(tree$finalModel,
             main = "Predicting the number of missing variables in an observation",
             sub = "Check whether the outcome variable is an important variable",
             roundint = TRUE,
             clip.facs = TRUE)

  })

######Create a recipe######
  
  getRecipe <- reactive({
    
    
    data <- getFilteredData()  
    rec <- recipes::recipe(DEATH_RATE ~ ., data = data) %>%
      update_role("idNum", new_role = "id") %>%
      update_role("OBS_TYPE", new_role = "split")
    
    #Recipe updates
    if (input$cleaning_model_input_naValues == "KNNimpute") {
      rec <- rec %>% step_impute_knn(all_predictors(), neighbors = 5)
    }
    if (input$cleaning_model_input_naValues == "bagImpute") {
      rec <- rec %>% step_impute_bag(all_predictors())
    }
    if (input$cleaning_model_input_center) {
      rec <- rec %>% step_center(all_numeric_predictors())
    }
    if (input$cleaning_model_input_scale) {
      rec <- rec %>% step_scale(all_numeric_predictors())
    }
    # if (input$cleaning_model_input_yeojohnson) {
    #   rec <- rec %>% step_YeoJohnson(all_numeric_predictors())
    # }
    
    rec <- rec %>% step_dummy(all_nominal_predictors(), one_hot = FALSE)
    
    
    rec
  })
  
  getTrainData <- reactive({
    data <- getFilteredData()
    if (input$cleaning_model_input_naValues == "omitNA") {
      train <- na.omit(subset(data, OBS_TYPE == "Train"))
    }
    else{
      train <- subset(data, OBS_TYPE == "Train")
    }
    train
  })
  
      ######Create a model######
  #Create a model using recipe
  trainModel <- reactive({
    req(input$cleaning_model_input_trainButton)
    isolate({
      train <- getTrainData()
    
    
    train_control <- trainControl(
      method = "cv",    # Cross-validation
      number = 10       # Number of folds
    )
    set.seed(11)
    model <- caret::train(getRecipe(),
                          data = train,
                          method = "glmnet",
                          trControl = train_control,
                          tuneLength = 10)
    model
    
    })
  })

  #####Train Model Summary#####
  output$cleaning_train_output_residual_summary <- renderPrint({
    
    req(trainModel(), input$cleaning_model_input_trainButton)  # Ensure trainModel is ready
    isolate({
    print(trainModel())
    })
  })
  
  
  
  getTestData <- reactive({
    data <- getFilteredData()
    if (input$cleaning_model_input_naValues == "omitNA") {
      test <- na.omit(subset(data, OBS_TYPE == "Test"))
    }
    else{
      test <- subset(data, OBS_TYPE == "Test")
    }
    test
  })
  ######Test Model Predictions######
  
  testModelPred <- reactive({
  req(input$cleaning_model_input_testButton)
  isolate({
    data <- getTestData()
    set.seed(11)
  #predictions <- NULL
  predictions <- predict(trainModel(), newdata = data)
  predictions
    })
  })
  
  ######Train Model Predictions######
  
  trainModelPred <- reactive({
    req(input$cleaning_model_input_testButton)
    isolate({
      data <- getTrainData()
      set.seed(11)
      #predictions <- NULL
      predictions <- predict(trainModel(), newdata = data)
      predictions
    })
  })
  
 
  ######RMSE######
  getRMSE <- reactive({
    residuals <- getTestData()$DEATH_RATE - testModelPred()
    rmse_value <- sqrt(mean(residuals^2, na.rm = TRUE))
    rmse_value
    
  })
  
  #Added an error layer using ChatGPT
  #________________
  output$cleaning_test_output_residual_summary <- renderPrint({

    req(testModelPred(), input$cleaning_model_input_testButton)  # Ensure testModelPred is ready
    isolate({
      tryCatch({
        # Display RMSE and model summary if available
        #cat("Test RMSE:", getRMSE(), "\n\n")
        model_results <- testModelPred()
        if (is.null(model_results) || length(model_results) == 0) {
          # If no model results, print message to train model first
          cat("You need to train your model first\n")
        } else {
          # Display RMSE and summary if model results are available
          cat("Test RMSE:", getRMSE(), "\n\n")
          print(summary(model_results))
        }
      }, error = function(e) {
        cat("Error in processing model: ", e$message, "\n")
      })
    })
  })
  #________________
  
  #####Train RMSE#####
  output$cleaning_model_output_training_RMSE <- renderPlot({
    data <- trainModel()
    plot(data)
    
  })
  

  #####Variable Importance#####
  output$cleaning_model_output_training_importance <- renderPlot({
    importance <- varImp(trainModel(), scale = TRUE)

    # Plot variable importance
    plot(importance, main = "Variable Importance")

  })
  
  ######Residuals VS Predicted######
  output$cleaning_model_output_testing_ResPred <- renderPlot({
    
    data <- getTestData()
    data$Predictions <- testModelPred()
    residuals <- data$DEATH_RATE - data$Predictions
    
    #Residulas vs predicted
    ggplot(data, aes(x = DEATH_RATE, y = residuals)) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  
      labs(x = "Predicted Values", y = "Residuals", title = "Residual Plot") +
      theme_minimal()  
    
  })
  
  ######Scatter plot of Actual vs Predicted######
  
  output$cleaning_model_output_testing_actual_pred <- renderPlot({
    data <- getTestData()
    data$Predictions <- testModelPred()
    
    #Scatterplot of Actual vs Predicted
    ggplot(data, aes(x = DEATH_RATE, y = Predictions)) +
      geom_point(size = 2) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
      labs(x = "Actual", y = "Predicted", title = "Actual vs. Predicted Values") +
      theme_minimal()
    
  })
  
  
  
  
  ######Residuals Visualizations######
  
  output$cleaning_model_output_outliers_box_test <- renderPlot({
    
    test <- getTestData()
    #Outlier boxplots
    coef <- input$cleaning_model_input_testing_outliers_iqr
    
    predictions_test <- testModelPred()
    
    test_res <- test$DEATH_RATE - predictions_test

    test$residuals <- test_res

    limits_test <- boxplot.stats(x = test$residuals, coef = coef)$stats
    label_test <- ifelse(test$residuals < limits_test[1] | test$residuals > limits_test[5], test$idNum, NA)

    #Boxplot with labels for Test:
    
    ggplot(data = test, mapping = aes(x = residuals, y = 1, label = label_test)) +
      geom_boxplot(coef = coef, outlier.colour = "red") +
      ggrepel::geom_text_repel(max.overlaps = 20) +
      labs(title = paste("Model outlier boxplots for test subset at IQR multiplier of", coef), x = "Residuals")+
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
    
  output$cleaning_model_output_outliers_box_train <- renderPlot({
    train <- getTrainData()

    #Outlier boxplots
    coef <- input$cleaning_model_input_testing_outliers_iqr

    predictions_train <- trainModelPred()

    train_res <- train$DEATH_RATE - predictions_train

    train$residuals <- train_res

    limits_train <- boxplot.stats(x = train$residuals, coef = coef)$stats
    label_train <- ifelse(train$residuals < limits_train[1] | train$residuals > limits_train[5], train$idNum, NA)

    #Boxplot with labels for Train:
    ggplot(data = train, mapping = aes(x = residuals, y = 1, label = label_train)) +
      geom_boxplot(coef = coef, outlier.colour = "red") +
      ggrepel::geom_text_repel(max.overlaps = 20) +
      labs(title = paste("Model outlier boxplots for train subset at IQR multiplier of", coef), x = "Residuals")+
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  output$cleaning_table_output_outlier_probability <- renderPrint({
    if (input$cleaning_model_input_show_probability) {
      prob <- (pnorm(q = 2.7) -0.5) * 2  #prob -2.7 < Z < +2.7
      outliers <- round((1 - prob) * nrow(getFilteredData()))
      
      cat("Probability of occurence of outliers is:", outliers, "\n\n")
      
    }
  })

}) #ShinyServer function
  

