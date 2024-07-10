library(shiny)
library(bslib)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Subsetting data"),
  sidebarLayout(
    sidebarPanel(
      helpText("Answer the following questions to subset the data as needed"),
      
      sliderInput("response_rate",
                  label = "Response rate:",
                  min = 0, max = 1, value = c(0.8, 1),
                  ticks = FALSE),
      sliderInput("achievable_gain",
                  label = "Achievable gain:",
                  min = -5, max = 5, value = c(-1, 1),
                  ticks = FALSE),
      radioButtons("calc_prereq",
                   label = "Include courses with a calc pre-req?",
                   choices = c("Yes", "No"),
                   selected = "No"),
      checkboxGroupInput("school_type",
                         label = "Schools:",
                         choices = c("High School",
                                     "Community College",
                                     "Baccalaureate College",
                                     "Master's",
                                     "Doctoral Universities"),
                         selected = c("Community College",
                                      "Baccalaureate College",
                                      "Master's",
                                      "Doctoral Universities")),
      sliderInput("class_size",
                  label = "Class size:",
                  min = 0, max = 100, value = c(10, 100),
                  ticks = FALSE),
      selectInput("pre_post_tests",
                  label = "Pre-test vs. post-test responses:",
                  choices = c("Students who took the pre-test",
                              "Students who took the post-test",
                              "Students who took both the pre- and post-tests"),
                  selected = "Students who took both the pre- and post-tests"),
      radioButtons("imputation",
                   label = "Imputate missing values?",
                   choices = c("Yes", "No"),
                   selected = "Yes"),
      # selectInput("answer_type",
      #             label = "How would you like student responses formatted?",
      #             choices = c("Correct vs. incorrect",
      #                         "Actual responses",
      #                         "Both correct vs. incorrect and actual responses"),
      #          selected = "Both correct vs. incorrect and actual responses"),
      checkboxGroupInput("overall_category",
                         label = "Choose overall question categories of interest:",
                         choices = c("Attitudes",
                                     "Concepts",
                                     "Demographics")),
      conditionalPanel(
        condition = "input.overall_category.indexOf('Attitudes') > -1",
        checkboxGroupInput("attitudes_sub", 
                           label = "Attitudes Subcategories: ",
                           choices = c("Affect",
                                       "Cognitive Competence",
                                       "Difficulty",
                                       "Effort",
                                       "Interest",
                                       "Value"))),
      conditionalPanel(
        condition = "input.overall_category.indexOf('Concepts') > -1",
        checkboxGroupInput("concepts_sub",
                           label = "Concepts Subcategories: ",
                           choices = c("Data Collection",
                                       "Descriptive Statistics",
                                       "Confidence Intervals",
                                       "Scope of Conclusions",
                                       "Significance",
                                       "Simulation"))),
      selectInput("right_wrong",
                  label = "How would you like student responses formatted?: ",
                  choices = c("Correct vs. Incorrect",
                              "Original answers")
        
      )
    ),
    mainPanel(
      textOutput("response_rate"),
      textOutput("achievable_gain"),
      textOutput("calc_prereq"),
      textOutput("school_type"),
      textOutput("class_size"),
      textOutput("imputation"),
      textOutput("answer_type"),
      textOutput("pre_post_tests"),
      uiOutput("overall_category"),
      textOutput("right_wrong"),
      textOutput("dt_heading"),
      tableOutput("data_table")
    )
  )
)

server <- function(input, output) {
  output$response_rate <- renderText({
    paste("Response rate:", input$response_rate[1], "to", input$response_rate[2])
  })
  
  output$achievable_gain <- renderText({
    paste("Achievable gain:", input$achievable_gain[1], "to", input$achievable_gain[2])
  })
  
  output$calc_prereq <- renderText({
    paste("Include courses with a calculus pre-req:", input$calc_prereq)
  })
  
  output$school_type <- renderText({
    paste("Schools:", paste(input$school_type, collapse = ", "))
  })
  
  output$class_size <- renderText({
    paste("Class size:", input$class_size[1], "to", input$class_size[2], "students")
  })
  
  output$imputation <- renderText({
    paste("Imputate missing values:", input$imputation)
  })
  
  # output$answer_type <- renderText({
  #   paste("Answer type:", input$answer_type)
  # })
  
  output$pre_post_tests <- renderText({
    paste("Pre-test vs. post-test responses:", input$pre_post_tests)
  })
  
  
  output$overall_category <- renderUI({
    if (length(input$overall_category) == 0) {
      paste("Overall categories: None selected")
    }
    
    overall_category_text <- paste("Overall categories:", paste(input$overall_category, collapse = ", "))
    attitudes_sub_text <- ""
    concepts_sub_text <- ""
    
    if ("Attitudes" %in% input$overall_category) {
      attitudes_sub_text <- paste( 
        "Attitudes Subcategories: ", 
        if (length(input$attitudes_sub) > 0) {
          paste(input$attitudes_sub, collapse = ", ")
        } else {
          "None selected"
        }, sep = "\n")
    }
    
    if ("Concepts" %in% input$overall_category) {
      concepts_sub_text <- paste(
        "\nConcepts Subcategories: ", 
        if (length(input$concepts_sub) > 0) {
          paste(input$concepts_sub, collapse = ", ")
        } else {
          "None selected"
        }, sep = "")
    }
    
    HTML(paste(overall_category_text), "<br>", 
         paste(attitudes_sub_text), "<br>",
         paste(concepts_sub_text))
    
    })
    output$right_wrong <- renderText({
      paste("Incorrect or correct: ", input$right_wrong)
    })
  
  
  output$dt_heading <- renderText({
    "Preview of data table:"
  })
  
  
  allYrsFinal <- read_csv("FinalFiles2023/Data/All Years Final Public - with Vars.csv")
  
  output$data_table <- renderTable({
    filteredData <- allYrsFinal |>
      filter(c.rr.pre >= input$response_rate[1], c.rr.pre <= input$response_rate[2],
             c.rr.post >= input$response_rate[1], c.rr.post <= input$response_rate[2],
             ach.gain.24 > input$achievable_gain[1], ach.gain.24 < input$achievable_gain[2],
             case_when(
               input$pre_post_tests == "Students who took the pre-test" ~ !is.na(opt.out.pre),
               input$pre_post_tests == "Students who took the post-test" ~ !is.na(opt.out.post),
               input$pre_post_tests == "Students who took both the pre- and post-tests" ~ !is.na(opt.out.pre) & !is.na(opt.out.post),
             ),
             if (input$calc_prereq == "No") (math.prereq != "Calculus" | is.na(math.prereq)) else TRUE,
             carnegie.classification %in% input$school_type,
             class.size.end >= input$class_size[1], class.size.end <= input$class_size[2]) 
    
    
    
    selected_columns <- c()
    
    # attitudes subcategories
    affect <- c("^q6d", "^q7e", "^q7h", "^q7i", "^q8h")
    competence <- c("^q6e", "^q7a", "^q8f", "^q9a", "^q9b", "^q9e")
    difficulty <- c("^6f", "^q6g", "^q6h", "^8b", "^q8d", "^q8j", "^q9d", "^q9f")
    effort <- c("^q6a", "^q6b", "^q7d", "^q8g")
    interest <- c("^q7b", "^q7j", "^q8c", "^q8i")
    value_ <- c("^q6i", "^q6j", "^q7c", "^q7f", "^q7g", "^q8a", "^q8e", "^q9c")
    
    # concepts subcategories
    confidence <- c("^q1[8-9]","^q20", "^q26", "^q45", "^46")
    data_collection <- c("^q16", "^q42")
    descriptive_stats <- c("^q17", "^q21", "^q3[2-3]", "^q36", "^q4[0-4]")
    conclusions <- c("^q22", "^q25")
    significance <- c("^q2[3-4]", "^q2[7-9]", "^q3[0-1]", "^q4[3-4]", "^q47")
    simulation <- c("^q3[3-5]", "^q3[7-9]")
    
    
    if ("Attitudes" %in% input$overall_category) {
      subcategories_selected <- FALSE
      if ("Affect" %in% input$attitudes_sub) {
        selected_columns <- c(selected_columns, grep(paste(affect, collapse = "|"), 
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      if ("Cognitive Competence" %in% input$attitudes_sub){
        selected_columns <- c(selected_columns, grep(paste(competence, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      if("Difficulty" %in% input$attitudes_sub){
        selected_columns <- c(selected_columns, grep(paste(difficulty, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      if("Effort" %in% input$attitudes_sub){
        selected_columns <- c(selected_columns, grep(paste(effort, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      if("Interest" %in% input$attitudes_sub){
        selected_columns <- c(selected_columns, grep(paste(interest, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
      }
      if("Value" %in% input$attitudes_sub){
        selected_columns <- c(selected_columns, grep(paste(value_, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      if (subcategories_selected == FALSE){
        selected_columns <- c(selected_columns, grep("^q[6-9]", colnames(allYrsFinal), value = TRUE))
      }
      
    }
    
    if ("Concepts" %in% input$overall_category) {
      subcatgories_selected <- FALSE
      if("Confidence Intervals" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(confidence, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      if("Data Collection" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(data_collection, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      
      if("Descriptive Statistics" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(descriptive_stats, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      
      if("Scope of Conclusions" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(conclusions, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      
      if("Significance" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(significance, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      
      if("Simulation" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(simulation, collapse = "|"),
                                                     colnames(allYrsFinal), value = TRUE))
        subcategories_selected <- TRUE
      }
      
      if(subcatgories_selected == FALSE) { 
        selected_columns <- c(selected_columns, grep("^q1[6-9]", colnames(allYrsFinal), value = TRUE), 
                              grep("^q2[0-9]", colnames(allYrsFinal), value = TRUE), 
                              grep("^q3[0-9]", colnames(allYrsFinal), value = TRUE), 
                              grep("^q4[0-7]", colnames(allYrsFinal), value = TRUE))
      }
      if (input$right_wrong == "Correct vs. Incorrect") {
        filteredData <- filteredData %>%
          mutate(across(starts_with("q"), ~ ifelse(grepl("\\*\\*$", .), "correct", "incorrect")))
      }
    }
    
    if ("Demographics" %in% input$overall_category) {
      selected_columns <- c(selected_columns, grep("^q4[8-9]", colnames(allYrsFinal), value = TRUE), 
                            grep("^q5[0-9]", colnames(allYrsFinal), value = TRUE))
    }
    
    non_question_columns <- colnames(allYrsFinal)[!grepl("^q", colnames(allYrsFinal))]
    selected_columns <- c(non_question_columns, selected_columns)
    
    
    filteredData <- filteredData |> select(all_of(selected_columns))
    
    

    head(filteredData)
  })
}

shinyApp(ui = ui, server = server)


