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
      selectInput("answer_type",
                  label = "How would you like student responses formatted?",
                  choices = c("Correct vs. incorrect",
                              "Actual responses",
                              "Both correct vs. incorrect and actual responses"),
                  selected = "Both correct vs. incorrect and actual responses"),
      checkboxGroupInput("overall_category",
                         label = "Choose overall question categories of interest:",
                         choices = c("Attitudes",
                                     "Concepts",
                                     "Demographics"))
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
      textOutput("overall_category"),
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
  
  output$answer_type <- renderText({
    paste("Answer type:", input$answer_type)
  })
  
  output$pre_post_tests <- renderText({
    paste("Pre-test vs. post-test responses:", input$pre_post_tests)
  })
  
  output$overall_category <- renderText({
    paste("Overall categories chosen:", paste(input$overall_category, collapse = ", "))
  })
  
  output$dt_heading <- renderText({
    "Preview of data table:"
  })
  
  allYrsFinal <- read_csv("FinalFiles2023/Data/All Years Final Public - with Vars.csv")
  
  output$data_table <- renderTable({
    filtered_data <- allYrsFinal %>%
      filter(c.rr.pre >= input$response_rate[1], c.rr.pre <= input$response_rate[2],
             c.rr.post >= input$response_rate[1], c.rr.post <= input$response_rate[2],
             ach.gain.24 > input$achievable_gain[1], ach.gain.24 < input$achievable_gain[2],
             case_when(
               input$pre_post_tests == "Students who took the pre-test" ~ !is.na(opt.out.pre),
               input$pre_post_tests == "Students who took the post-test" ~ !is.na(opt.out.post),
               input$pre_post_tests == "Students who took both the pre- and post-tests" ~ !is.na(opt.out.pre) & !is.na(opt.out.post)
             ),
             if (input$calc_prereq == "No") (math.prereq != "Calculus" | is.na(math.prereq)) else TRUE,
             carnegie.classification %in% input$school_type,
             class.size.end >= input$class_size[1], class.size.end <= input$class_size[2])
    
    selected_columns <- c()
    
    if ("Attitudes" %in% input$overall_category) {
      selected_columns <- c(selected_columns, grep("^q[6-9]", colnames(filtered_data), value = TRUE))
    }
    
    if ("Concepts" %in% input$overall_category) {
      selected_columns <- c(selected_columns, grep("^q1[6-9]", colnames(filtered_data), value = TRUE), 
                            grep("^q2[0-9]", colnames(filtered_data), value = TRUE), 
                            grep("^q3[0-9]", colnames(filtered_data), value = TRUE), 
                            grep("^q4[0-7]", colnames(filtered_data), value = TRUE))
    }
    
    if ("Demographics" %in% input$overall_category) {
      selected_columns <- c(selected_columns, grep("^q4[8-9]", colnames(filtered_data), value = TRUE), 
                            grep("^q5[0-9]", colnames(filtered_data), value = TRUE))
    }
    
    non_question_columns <- colnames(filtered_data)[!grepl("^q", colnames(filtered_data))]
    selected_columns <- c(non_question_columns, selected_columns)
    

    allYrsFinal <- allYrsFinal |> select(all_of(selected_columns))
    
    
    head(allYrsFinal)
  })
}

shinyApp(ui = ui, server = server)
