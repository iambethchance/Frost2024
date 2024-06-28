library(shiny)
library(bslib)
library(tidyverse)

ui <- page_sidebar(
  title = "Subsetting data",
  sidebar = sidebar(
    helpText(
      "Answer the following questions to subset the data as needed"
    ),
    
    # response rate: slider with adjustable min
    sliderInput("response_rate",
                label = "Response rate:",
                min = 0, max = 1, value = c(0.8, 1),
                ticks = FALSE),
    # achievable gain: slider with adjustable min and max
    sliderInput("achievable_gain",
                label = "Acheivable gain:",
                min = -5, max = 5, value = c(-1, 1),
                ticks = FALSE),
    # # include iscam: multiple choice (yes remove, no don't remove)
    #     radioButtons("iscam",
    #                  label = "Include ISCAM?",
    #                  choices = c("Yes", "No"),
    #                  selected = "Yes"),
    #   # replace with type of textbook? (select all that apply)
    #     checkboxGroupInput("textbooks",
    #                        label = "Textbooks:",
    #                        choices = c("Textbook 1",
    #                                    "Textbook 2",
    #                                    "Textbook 3"),
    #                        selected = c("Textbook 1",
    #                                     "Textbook 2",
    #                                     "Textbook 3")),
    # remove calculus math pre-req: multiple choice (yes remove, no don't remove)
    radioButtons("calc_prereq",
                 label = "Include courses with a calc pre-req?",
                 choices = c("Yes", "No"),
                 selected = "No"),
    # type of students (hs, community college, etc.): select all that apply
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
    # class size: slider with adjustible min and max
    sliderInput("class_size",
                label = "Class size:",
                min = 0, max = 100, value = c(10, 100),
                ticks = FALSE),
    # # remove low section response rate?
    #     sliderInput("section_rr",
    #             label = "Section response rate:",
    #             min = 0, max = 1, value = c(0.25, 1),
    #             ticks = FALSE),
    # pre-test vs. post-test vs. both
    selectInput("pre_post_tests",
                label = "Pre-test vs. post-test responses:",
                choices = c("Students who took the pre-test",
                            "Students who took the post-test",
                            "Students who took both the pre- and post-tests"),
                selected = "Students who took both the pre- and post-tests"),
    # imputation: multiple choice (yes imputate missing values, no don't imputate missing values)
    radioButtons("imputation",
                 label = "Imputate missing values?",
                 choices = c("Yes", "No"),
                 selected = "Yes"),
    # type of answers: drop-down (correct/incorrect, actual responses, both)
    selectInput("answer_type",
                label = "How would you like student responses formatted?",
                choices = c("Correct vs. incorrect",
                            "Actual responses",
                            "Both correct vs. incorrect and actual responses"),
                selected = "Both correct vs. incorrect and actual responses")
  ),
  mainPanel(
    textOutput("response_rate"),
    textOutput("achievable_gain"),
    # textOutput("iscam"),
    # textOutput("textbooks"),
    textOutput("calc_prereq"),
    textOutput("school_type"),
    textOutput("class_size"),
    # textOutput("section_rr"),
    textOutput("imputation"),
    textOutput("answer_type"),
    textOutput("pre_post_tests"),
    textOutput("dt_heading"),
    tableOutput("data_table")
  )
)

server <- function(input, output) {
  output$response_rate <- renderText({
    paste("Response rate:", input$response_rate[1], "to", input$response_rate[2])
  })
  
  output$achievable_gain <- renderText({
    paste("Achievable gain:", input$achievable_gain[1], "to", input$achievable_gain[2])
  })
  
  # output$iscam <- renderText({
  #   paste("Include ISCAM:", input$iscam)
  # })
  # 
  # output$textbooks <- renderText({
  #   out <- "Textbooks:"
  #   prev <- FALSE
  #   if (!is.na(input$textbooks[1])) {
  #     out <- paste(out, input$textbooks[1])
  #     prev <- TRUE
  #   }
  #   if (!is.na(input$textbooks[2])) {
  #     if (prev) {
  #       out <- paste(out, input$textbooks[2], sep = ", ")
  #     } else {
  #       out <- paste(out, input$textbooks[2])
  #       prev <- TRUE
  #     }
  #   }
  #   if (!is.na(input$textbooks[3])) {
  #     if (prev) {
  #       out <- paste(out, input$textbooks[3], sep = ", ")
  #     } else {
  #       out <- paste(out, input$textbooks[3])
  #     }
  #   }
  #   out
  # })
  
  output$calc_prereq <- renderText({
    paste("Include courses with a calculus pre-req:", input$calc_prereq)
  })
  
  output$school_type <- renderText({
    out <- "Schools:"
    prev <- FALSE
    if (!is.na(input$school_type[1])) {
      out <- paste(out, input$school_type[1])
      prev <- TRUE
    }
    if (!is.na(input$school_type[2])) {
      if (prev) {
        out <- paste(out, input$school_type[2], sep = ", ")
      } else {
        out <- paste(out, input$school_type[2])
        prev <- TRUE
      }
    }
    if (!is.na(input$school_type[3])) {
      if (prev) {
        out <- paste(out, input$school_type[3], sep = ", ")
      } else {
        out <- paste(out, input$school_type[3])
        prev <- TRUE
      }
    }
    if (!is.na(input$school_type[4])) {
      if (prev) {
        out <- paste(out, input$school_type[4], sep = ", ")
      } else {
        out <- paste(out, input$school_type[4])
        prev <- TRUE
      }
    }
    if (!is.na(input$school_type[5])) {
      if (prev) {
        out <- paste(out, input$school_type[5], sep = ", ")
      } else {
        out <- paste(out, input$school_type[5])
        prev <- TRUE
      }
    }
    out
  })
  
  output$class_size <- renderText({
    paste("Class size:", input$class_size[1], "to", input$class_size[2], "students")
  })
  
  # output$section_rr <- renderText({
  #   paste("Section response rate:", input$section_rr[1], "to", input$section_rr[2])
  # })
  
  output$imputation <- renderText({
    paste("Imputate missing values:", input$imputation)
  })
  
  output$answer_type <- renderText({
    paste("Answer type:", input$answer_type)
  })
  
  output$pre_post_tests <- renderText({
    paste("Pre-test vs. post-test responses:", input$pre_post_tests)
  })
  
  output$dt_heading <- renderText({
    "Preview of data table:"
  })
  
  allYrsFinal <- read_csv("FinalFiles2023/Data/All Years Final Public - with Vars.csv")
  output$data_table <- renderTable({
    allYrsFinal |>
      filter(c.rr.pre >= input$response_rate[1], c.rr.pre <= input$response_rate[2],
             c.rr.post >= input$response_rate[1], c.rr.post <= input$response_rate[2],
             ach.gain.24 > input$achievable_gain[1], ach.gain.24 < input$achievable_gain[2],
             case_when(
               input$pre_post_tests == "Students who took the pre-test" ~ (is.na(opt.out.pre) == FALSE),
               input$pre_post_tests == "Students who took the post-test" ~ (is.na(opt.out.post) == FALSE),
               input$pre_post_tests == "Students who took both the pre- and post-tests" ~ (is.na(opt.out.pre) == FALSE & is.na(opt.out.post) == FALSE)
             ),
             if (input$calc_prereq == "No") (math.prereq != "Calculus" | is.na(math.prereq) == T) else TRUE,
             carnegie.classification %in% input$school_type,
             class.size.end >= input$class_size[1], class.size.end <= input$class_size[2]
      ) |>
      head()
  })
}

shinyApp(ui = ui, server = server)

