library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "subsetting data",
  sidebar = sidebar(
    helpText(
      "write description here"
    ),

# response rate: slider with adjustable min
    sliderInput("response_rate",
                label = "Response rate:",
                min = 0, max = 1, value = c(0.8, 1),
                ticks = FALSE),
# achievable gain: slider with adjustable min and max
    sliderInput("achievable gain",
                label = "Acheivable gain:",
                min = -5, max = 5, value = c(-1, 1),
                ticks = FALSE),
# include iscam: multiple choice (yes remove, no don't remove)
    radioButtons("iscam",
                 label = "Include ISCAM?",
                 choices = c("Yes" = 1, "No" = 2),
                 selected = 2),
  # replace with type of textbook? (select all that apply)
    checkboxGroupInput("textbooks",
                       label = "Textbooks:",
                       choices = c("Textbook 1" = 1,
                                   "Textbook 2" = 2,
                                   "Textbook 3" = 3),
                       selected = c(1, 2, 3)),
# remove calculus math pre-req: multiple choice (yes remove, no don't remove)
    radioButtons("calc_prereq",
                 label = "Include courses with a calc pre-req?",
                 choices = c("Yes" = 1, "No" = 2),
                 selected = 2),
# type of students (hs, community college, etc.): select all that apply
    checkboxGroupInput("student_type",
                   label = "Type of students:",
                   choices = c("High school courses" = 1,
                               "Community college courses" = 2,
                               "4-year college courses" = 3),
                   selected = c(2, 3)),
# class size: slider with adjustible min and max
    sliderInput("class_size",
            label = "Class size:",
            min = 0, max = 100, value = c(10, 100),
            ticks = FALSE),
# remove low section response rate?
    sliderInput("section_response_rate",
            label = "Section response rate:",
            min = 0, max = 1, value = c(0.25, 1),
            ticks = FALSE),
# imputation: multiple choice (yes imputate missing values, no don't imputate missing values)
    radioButtons("imputation",
                 label = "Imputate missing values?",
                 choices = c("Yes" = 1, "No" = 2),
                 selected = 1),
# type of answers: drop-down (correct/incorrect, actual responses, both)
    selectInput("answer_type",
                label = "How would you like student responses formatted?",
                choices = c("Correct vs. incorrect" = 1,
                            "Actual responses" = 2,
                            "Both correct vs. incorrect and actual responses" = 3),
                selected = 3),
# pre-test vs. post-test vs. both
    selectInput("pre_post_tests",
                label = "Pre-test vs. post-test responses:",
                choices = c("Students who took the pre-test" = 1,
                            "Students who took the post-test" = 2,
                            "Students who took both the pre- and post-tests" = 3),
                selected = 3)
  )
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)

