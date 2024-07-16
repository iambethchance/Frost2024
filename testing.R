library(shiny)
library(bslib)
library(tidyverse)
library(plyr)
library(Amelia)

ui <- fluidPage(
  titlePanel("Subsetting data"),
  sidebarLayout(
    sidebarPanel(
      helpText("Answer the following questions to subset the data as needed"),

      checkboxGroupInput("year",
                label = "Years:",
                choices = c("2014-15 school year",
                            "2015-16 school year",
                            "2016-17 school year"),
                selected = c("2014-15 school year",
                             "2015-16 school year",
                             "2016-17 school year")),
    selectInput("pre_post_tests",
            label = "Pre-test vs. post-test responses:",
            choices = c("Students who took the pre-test",
                        "Students who took the post-test",
                        "Students who took both the pre- and post-tests"),
            selected = "Students who took both the pre- and post-tests"),
    sliderInput("response_rate",
                label = HTML('<a href="ResponseRate.html" target="_blank">Response rate:</a>'),
                min = 0, max = 1, value = c(0.8, 1),
                ticks = FALSE),
    sliderInput("achievable_gain",
                label = HTML('<a href="AchievableGain.html" target="_blank">Achievable gain:</a>'),
                min = -5, max = 5, value = c(-1, 1),
                ticks = FALSE),
    checkboxGroupInput("textbooks",
                       label = HTML('<a href="Textbooks.html" target="_blank">Textbooks:</a>'),
                       choices = c("ISI",
                                   "ISI1st",
                                   "OtherSBI",
                                   "NotSBI",
                                   "NotSBI2",
                                   "ISCAM",
                                   "Other"),
                       selected = c("ISI",
                                    "ISI1st",
                                    "OtherSBI",
                                    "NotSBI",
                                    "NotSBI2",
                                    "Other")),
    radioButtons("calc_prereq",
                 label = "Include courses with a calc pre-req?",
                 choices = c("Yes", "No"),
                 selected = "No"),
    checkboxGroupInput("school_type",
                   label = HTML('<a href="SchoolType.html" target="_blank">Schools:</a>'),
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
            min = 0, max = 450, value = c(10, 450),
            ticks = FALSE),
# # remove low section response rate?
#     sliderInput("section_rr",
#             label = "Section response rate:",
#             min = 0, max = 1, value = c(0.25, 1),
#             ticks = FALSE),
    radioButtons("imputation",
                 label = "Impute missing values?",
                 choices = c("Yes", "No"),
                 selected = "No"),
    checkboxGroupInput("overall_category",
                   label = "Choose overall question categories of interest:",
                   choices = c("Attitudes",
                               "Concepts",
                               "Demographics")),
    conditionalPanel(condition = "input.overall_category.indexOf('Attitudes') > -1",
          checkboxGroupInput("attitudes_sub", 
                     label = "Attitudes Subcategories: ",
                     choices = c("Affect",
                                 "Cognitive Competence",
                                 "Difficulty",
                                 "Effort",
                                 "Interest",
                                 "Value"))),
    conditionalPanel(condition = "input.overall_category.indexOf('Concepts') > -1",
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
    textOutput("year"),
    textOutput("pre_post_tests"),
    textOutput("response_rate"),
    textOutput("achievable_gain"),
    textOutput("textbooks"),
    textOutput("calc_prereq"),
    textOutput("school_type"),
    textOutput("class_size"),
    # textOutput("section_rr"),
    textOutput("imputation"),
    uiOutput("overall_category"),
    textOutput("right_wrong"),
    downloadButton("downloadData", "Download"),
    uiOutput("dt_heading"),
    tableOutput("data_table")
  )
))

server <- function(input, output, session) {
  output$year <- renderText({
    out <- "Year(s):"
    prev <- FALSE
    if (!is.na(input$year[1])) {
      out <- paste(out, input$year[1])
      prev <- TRUE
    }
    if (!is.na(input$year[2])) {
      if (prev) {
        out <- paste(out, input$year[2], sep = ", ")
      } else {
        out <- paste(out, input$year[2])
        prev <- TRUE
      }
    }
    if (!is.na(input$year[3])) {
      if (prev) {
        out <- paste(out, input$year[3], sep = ", ")
      } else {
        out <- paste(out, input$year[3])
      }
    }
    out
  })
  
  output$pre_post_tests <- renderText({
    paste("Pre-test vs. post-test responses:", input$pre_post_tests)
  })
  
  output$response_rate <- renderText({
    paste("Response rate:", input$response_rate[1], "to", input$response_rate[2])
  })
  
  output$achievable_gain <- renderText({
    paste("Achievable gain:", input$achievable_gain[1], "to", input$achievable_gain[2])
  })

  output$textbooks <- renderText({
    out <- "Textbooks:"
    prev <- FALSE
    if (!is.na(input$textbooks[1])) {
      out <- paste(out, input$textbooks[1])
      prev <- TRUE
    }
    if (!is.na(input$textbooks[2])) {
      if (prev) {
        out <- paste(out, input$textbooks[2], sep = ", ")
      } else {
        out <- paste(out, input$textbooks[2])
        prev <- TRUE
      }
    }
    if (!is.na(input$textbooks[3])) {
      if (prev) {
        out <- paste(out, input$textbooks[3], sep = ", ")
      } else {
        out <- paste(out, input$textbooks[3])
        prev <- TRUE
      }
    }
    if (!is.na(input$textbooks[4])) {
      if (prev) {
        out <- paste(out, input$textbooks[4], sep = ", ")
      } else {
        out <- paste(out, input$textbooks[4])
        prev <- TRUE
      }
    }
    if (!is.na(input$textbooks[5])) {
      if (prev) {
        out <- paste(out, input$textbooks[5], sep = ", ")
      } else {
        out <- paste(out, input$textbooks[5])
        prev <- TRUE
      }
    }
    if (!is.na(input$textbooks[6])) {
      if (prev) {
        out <- paste(out, input$textbooks[6], sep = ", ")
      } else {
        out <- paste(out, input$textbooks[6])
      }
    }
    out
  })
  
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
  
  output$imputation <- renderText({
    paste("Impute missing values:", input$imputation)
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
  
  
  allYrsFinal <- read_csv("FinalFiles2023/Data/All Years Final Public - with Vars.csv")
  postSub <- read.csv("All Years Final Public.csv", stringsAsFactors = TRUE)
  filteredData <- reactive({
    filteredData <- postSub
    
    if (input$imputation == "Yes") {
      data_amelia <- filteredData

      data_amelia$is.major.course <- as.character(data_amelia$is.major.course)
      data_amelia$field.major <- as.character(data_amelia$field.major)
      data_amelia$grade.expectation.pre <- as.character(data_amelia$grade.expectation.pre)
      data_amelia$grade.expectation.post <- as.character(data_amelia$grade.expectation.post)
      data_amelia$gender <- as.character(data_amelia$gender)
      data_amelia$has.taken.stat.course <- as.character(data_amelia$has.taken.stat.course)
      data_amelia$prev.stat.course <- as.character(data_amelia$prev.stat.course)
      data_amelia$status <- as.character(data_amelia$status)
      data_amelia$math.satact.flag <- as.character(data_amelia$math.satact.flag)

      data_amelia$is.major.course[data_amelia$is.major.course == "major course"] <- 1
      data_amelia$is.major.course[data_amelia$is.major.course == "not major course"] <- 0
      data_amelia$field.major[data_amelia$field.major == "Arts and humanities"] <- 1
      data_amelia$field.major[data_amelia$field.major == "Natural and applied sciences"] <- 2
      data_amelia$field.major[data_amelia$field.major == "Other"] <- 3
      data_amelia$field.major[data_amelia$field.major == "Social sciences"] <- 4

      data_amelia$grade.expectation.pre <- mapvalues(data_amelia$grade.expectation.pre,
                                                     from = c("A+", "A", "A-", "B+", "B", "B-","C+", "C", "C-",
                                                              "D+", "D", "D-", "F"),
                                                     to = c(1:13))


      data_amelia$grade.expectation.post <- mapvalues(data_amelia$grade.expectation.post,
                                                      from = c("A+", "A", "A-", "B+", "B", "B-",
                                                               "C+", "C", "C-", "D+", "D", "D-", "F"),
                                                      to = c(1:13))

      data_amelia$gender[data_amelia$gender == "Female"] <- 1
      data_amelia$gender[data_amelia$gender == "Male"] <- 0
      data_amelia$has.taken.stat.course[data_amelia$has.taken.stat.course == "No"] <- 0
      data_amelia$has.taken.stat.course[data_amelia$has.taken.stat.course == "Yes"] <- 1
      data_amelia$prev.stat.course[data_amelia$prev.stat.course == "No"] <- 0
      data_amelia$prev.stat.course[data_amelia$prev.stat.course == "Yes (Not AP)"] <- 1
      data_amelia$prev.stat.course[data_amelia$prev.stat.course == "Yes (AP)"] <- 2

      data_amelia$status <- mapvalues(data_amelia$status,
                                      from = c("Other", "High School Student", "Freshman in college", "Sophomore in college",
                                               "Junior in college", "Senior in college", "Fifth or more year in college",
                                               "Non-traditional/part-time student in college", "Graduate Student"),
                                      to = c(0:8))

      data_amelia$math.satact.flag[data_amelia$math.satact.flag == "SAT"] <- 1
      data_amelia$math.satact.flag[data_amelia$math.satact.flag == "ACT"] <- 2

      data_amelia$is.major.course <- as.numeric(data_amelia$is.major.course)
      data_amelia$field.major <- as.numeric(data_amelia$field.major)
      data_amelia$grade.expectation.pre <- as.numeric(data_amelia$grade.expectation.pre)
      data_amelia$grade.expectation.post <- as.numeric(data_amelia$grade.expectation.post)
      data_amelia$gender <- as.numeric(data_amelia$gender)
      data_amelia$has.taken.stat.course <- as.numeric(data_amelia$has.taken.stat.course)
      data_amelia$prev.stat.course <- as.numeric(data_amelia$prev.stat.course)
      data_amelia$status <- as.numeric(data_amelia$status)
      data_amelia$math.satact.flag <- as.numeric(data_amelia$math.satact.flag)
      
      # ord_vars <- names(data_amelia[c(162:175)])
      ord_vars <- names(data_amelia[c(225:238)])
      
      # gpa_bound <- matrix(c(176, 1.0, 4.0), nrow = 1, ncol = 3)
      gpa_bound <- matrix(c(239, 1.0, 4.0), nrow = 1, ncol = 3)

      # myidvars <- names(data_amelia[c(1:161, 184:203, 205:235, 237:257, 270:297)]) # remove 236?
      myidvars <- names(data_amelia[c(1:224, 247:266, 268:318, 331:334, 337:387)])
      # myidvars <- names(data_amelia[c(1:224, 247:266, 268:318, 331:334, 337:387)])

      data_imputed <- amelia(data_amelia, m=1,
                       idvars = myidvars,
                       bounds = gpa_bound,
                       noms = c("gender", "has.taken.stat.course"),
                       ords = c(ord_vars, "grade.expectation.pre", "grade.expectation.post",
                                "prev.stat.course", "status", "math.satact.flag"))

      data_imputed <- data_imputed$imputations$imp1
      
      data_imputed$gpa <- round(data_imputed$gpa, 2)

      data_imputed$is.major.course[data_imputed$is.major.course == 1] <- "major course"
      data_imputed$is.major.course[data_imputed$is.major.course == 0] <- "not major course"
      data_imputed$field.major[data_imputed$field.major == 1] <- "Arts and humanities"
      data_imputed$field.major[data_imputed$field.major == 2] <- "Natural and applied sciences"
      data_imputed$field.major[data_imputed$field.major == 3] <- "Other"
      data_imputed$field.major[data_imputed$field.major == 4] <- "Social sciences"

      data_imputed$grade.expectation.pre <- mapvalues(data_imputed$grade.expectation.pre,
                                                      from = c(1:13),
                                                      to = c("A+", "A", "A-", "B+", "B", "B-","C+", "C", "C-",
                                                             "D+", "D", "D-", "F"))


      data_imputed$grade.expectation.post <- mapvalues(data_imputed$grade.expectation.post,
                                                       from = c(1:13),
                                                       to = c("A+", "A", "A-", "B+", "B", "B-",
                                                              "C+", "C", "C-", "D+", "D", "D-", "F"))

      data_imputed$gender[data_imputed$gender == 1] <- "Female"
      data_imputed$gender[data_imputed$gender == 0] <- "Male"
      data_imputed$has.taken.stat.course[data_imputed$has.taken.stat.course == 0] <- "No"
      data_imputed$has.taken.stat.course[data_imputed$has.taken.stat.course == 1] <- "Yes"
      data_imputed$prev.stat.course[data_imputed$prev.stat.course == 0] <- "No"
      data_imputed$prev.stat.course[data_imputed$prev.stat.course == 1] <- "Yes (Not AP)"
      data_imputed$prev.stat.course[data_imputed$prev.stat.course == 2] <- "Yes (AP)"

      data_imputed$status <- mapvalues(data_imputed$status,
                                       from = c(0:8),
                                       to = c("Other", "High School Student", "Freshman in college", "Sophomore in college",
                                              "Junior in college", "Senior in college", "Fifth or more year in college",
                                              "Non-traditional/part-time student in college", "Graduate Student"))

      data_imputed$is.white[data_imputed$is.white == 1] <- T
      data_imputed$is.white[data_imputed$is.white == 0] <- F

      data_imputed$math.satact.flag[data_imputed$math.satact.flag == 1] <- "SAT"
      data_imputed$math.satact.flag[data_imputed$math.satact.flag == 2] <- "ACT"

      data_imputed$is.major.course <- as.factor(data_imputed$is.major.course)
      data_imputed$field.major <- as.factor(data_imputed$field.major)
      data_imputed$grade.expectation.pre <- as.factor(data_imputed$grade.expectation.pre)
      data_imputed$grade.expectation.post <- as.factor(data_imputed$grade.expectation.post)
      data_imputed$gender <- as.factor(data_imputed$gender)
      data_imputed$has.taken.stat.course <- as.factor(data_imputed$has.taken.stat.course)
      data_imputed$prev.stat.course <- as.factor(data_imputed$prev.stat.course)
      data_imputed$status <- as.factor(data_imputed$status)
      data_imputed$math.satact.flag <- as.factor(data_imputed$math.satact.flag)

    #
      data_imputed$affect.change <- data_imputed$affect.post - data_imputed$affect.pre
      data_imputed$cognitive.competence.change <- data_imputed$cognitive.competence.post - data_imputed$cognitive.competence.pre
      data_imputed$difficulty.change <- data_imputed$difficulty.post - data_imputed$difficulty.pre
      data_imputed$effort.change <- data_imputed$effort.post - data_imputed$effort.pre
      data_imputed$interest.change <- data_imputed$interest.post - data_imputed$interest.pre
      data_imputed$value.change <- data_imputed$value.post - data_imputed$value.pre
    #
      
      data_imputed$answered.pre <- 1
      data_imputed$answered.pre[data_imputed$pre.perc.24 == 0] <- 0

      data_imputed$answered.post <- 1
      data_imputed$answered.post[data_imputed$post.perc.24 == 0] <- 0

      data_imputed$answered.both <- 1
      data_imputed$answered.both[data_imputed$pre.perc.24 == 0 | data_imputed$post.perc.24 == 0] <- 0

    #
      data_imputed <- data_imputed |>
        group_by(instructor.section, year) |>
        mutate(#section.num.responded = n(),
               add_count(name = section.num.responded),
               section.pre.responded = sum(answered.pre, na.rm = T),
               section.post.responded = sum(answered.post, na.rm = T),
               section.both.responded = sum(answered.both, na.rm = T),
               section.c.rr = section.both.responded/class.size.start,
               section.ach.gain.24 = mean(ach.gain.24, na.rm = T),
               section.pre.perc.24 = mean(pre.perc.24, na.rm = T),
               section.post.perc.24 = mean(post.perc.24, na.rm = T),
               section.gpa = mean(gpa, na.rm = T),
               section.satact.zscore = mean(satact.zscore, na.rm = T),
               section.affect.pre = mean(affect.pre, na.rm = T),
               section.affect.post = mean(affect.post, na.rm = T),
               section.cognitive.competence.pre = mean(cognitive.competence.pre, na.rm = T),
               section.cognitive.competence.post = mean(cognitive.competence.post, na.rm = T),
               section.difficulty.pre = mean(difficulty.pre, na.rm = T),
               section.difficulty.post = mean(difficulty.post, na.rm = T),
               section.effort.pre = mean(effort.pre, na.rm = T),
               section.effort.post = mean(effort.post, na.rm = T),
               section.interest.pre = mean(interest.pre, na.rm = T),
               section.interest.post = mean(interest.post, na.rm = T),
               section.value.pre = mean(value.pre, na.rm = T),
               section.value.post = mean(value.post, na.rm = T),
               section.overall.attitude.pre = mean(overall.attitude.pre, na.rm = T),
               section.overall.attitude.post = mean(overall.attitude.post, na.rm = T),
               var.pre = var(pre.perc.24, na.rm = T),
               var.post = var(post.perc.24, na.rm = T)
        ) |>
        ungroup() |>
        mutate(d = (section.post.perc.24 - section.pre.perc.24) / sqrt((var.pre+var.post)/2))
    #  
      
      data_imputed$class.session.length[is.na(data_imputed$class.session.length) == T] <- "Unknown"
      data_imputed$class.size.start.ind[is.na(data_imputed$class.size.start.ind) == T] <- "Unknown"
      data_imputed$class.size.end.ind[is.na(data_imputed$class.size.end.ind) == T] <- "Unknown"
      data_imputed$class.meet.weeks.ind[is.na(data_imputed$class.meet.weeks.ind) == T] <- "Unknown"
      data_imputed$time.meet[is.na(data_imputed$time.meet) == T] <- "Unknown"
      data_imputed$incentive.pre[is.na(data_imputed$incentive.pre) == T] <- "Unknown"
      data_imputed$incentive.post[is.na(data_imputed$incentive.post) == T] <- "Unknown"
      data_imputed$math.prereq[is.na(data_imputed$math.prereq) == T] <- "Unknown"
      data_imputed$student.type[is.na(data_imputed$student.type) == T] <- "Unknown"
      data_imputed$firstgen[is.na(data_imputed$firstgen) == T] <- "Unknown"
      data_imputed$race.origin[is.na(data_imputed$race.origin) == T] <- "Unknown"
      data_imputed$is.white[is.na(data_imputed$is.white) == T] <- "Unknown"
      data_imputed$gaise.familiar[is.na(data_imputed$gaise.familiar) == T] <- "Unknown"
      data_imputed$department.type[is.na(data_imputed$department.type) == T] <- "Unknown"
      data_imputed$is.stats.department[is.na(data_imputed$is.stats.department) == T] <- "Unknown"
      data_imputed$analyzing.data.experience[is.na(data_imputed$analyzing.data.experience) == T] <- "Unknown"
      data_imputed$position.classification[is.na(data_imputed$position.classification) == T] <- "Unknown"
      data_imputed$advanced.stats.degree.type[is.na(data_imputed$advanced.stats.degree.type) == T] <- "Unknown"
      data_imputed$lecture.type[is.na(data_imputed$lecture.type) == T] <- "Unknown"
      data_imputed$TA[is.na(data_imputed$TA) == T] <- "Unknown"
      data_imputed$admin.pre.location[is.na(data_imputed$admin.pre.location) == T] <- "Unknown"
      data_imputed$admin.post.location[is.na(data_imputed$admin.post.location) == T] <- "Unknown"
      data_imputed$admin.pre.time[is.na(data_imputed$admin.pre.time) == T] <- "Unknown"
      data_imputed$admin.post.time[is.na(data_imputed$admin.post.time) == T] <- "Unknown"
      data_imputed$isi.workshop[is.na(data_imputed$isi.workshop) == T] <- "Unknown"
      data_imputed$days.meet[is.na(data_imputed$days.meet) == T] <- "Unknown"
      data_imputed$years.teaching.intro.stats.binned[is.na(data_imputed$years.teaching.intro.stats.binned) == T] <- "Unknown"
      data_imputed$years.teaching.experience.binned[is.na(data_imputed$years.teaching.experience.binned) == T] <- "Unknown"
      data_imputed$percent.lecture.binned[is.na(data_imputed$percent.lecture.binned) == T] <- "Unknown"

      filteredData <- data_imputed
    }
  
    filteredData <- filteredData |>
      mutate(school.year = case_when(endsWith(as.character(instructor.section), "14-15") ~ "2014-15 school year",
                                     endsWith(as.character(instructor.section), "15-16") ~ "2015-16 school year",
                                     endsWith(as.character(instructor.section), "16-17") ~ "2016-17 school year")) |>
      filter(school.year %in% input$year) |>
      select(-school.year) |>
      filter(case_when(input$pre_post_tests == "Students who took the pre-test"
                          ~ (c.rr.pre >= input$response_rate[1] & c.rr.pre <= input$response_rate[2]),
                       input$pre_post_tests == "Students who took the post-test"
                          ~ (c.rr.post >= input$response_rate[1] & c.rr.post <= input$response_rate[2]),
                       input$pre_post_tests == "Students who took both the pre- and post-tests"
                          ~ (c.rr.pre >= input$response_rate[1] & c.rr.pre <= input$response_rate[2] & c.rr.post >= input$response_rate[1] & c.rr.post <= input$response_rate[2]
                       )),
             # c.rr.pre >= input$response_rate[1], c.rr.pre <= input$response_rate[2],
             # c.rr.post >= input$response_rate[1], c.rr.post <= input$response_rate[2],
             ach.gain.24 > input$achievable_gain[1], ach.gain.24 < input$achievable_gain[2],
             case_when(
               input$pre_post_tests == "Students who took the pre-test" ~ (is.na(opt.out.pre) == FALSE),
               input$pre_post_tests == "Students who took the post-test" ~ (is.na(opt.out.post) == FALSE),
               input$pre_post_tests == "Students who took both the pre- and post-tests" ~ (is.na(opt.out.pre) == FALSE & is.na(opt.out.post) == FALSE)
             ),
             if (input$calc_prereq == "No") (math.prereq != "Calculus" | is.na(math.prereq) == T) else TRUE,
             textbook.classification %in% input$textbooks,
             carnegie.classification %in% input$school_type,
             class.size.end >= input$class_size[1], class.size.end <= input$class_size[2]
      )
    
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
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      if ("Cognitive Competence" %in% input$attitudes_sub){
        selected_columns <- c(selected_columns, grep(paste(competence, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      if("Difficulty" %in% input$attitudes_sub){
        selected_columns <- c(selected_columns, grep(paste(difficulty, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      if("Effort" %in% input$attitudes_sub){
        selected_columns <- c(selected_columns, grep(paste(effort, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      if("Interest" %in% input$attitudes_sub){
        selected_columns <- c(selected_columns, grep(paste(interest, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
      }
      if("Value" %in% input$attitudes_sub){
        selected_columns <- c(selected_columns, grep(paste(value_, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      if (subcategories_selected == FALSE){
        selected_columns <- c(selected_columns, grep("^q[6-9]", colnames(filteredData), value = TRUE))
      }
      
    }
    
    if ("Concepts" %in% input$overall_category) {
      subcatgories_selected <- FALSE
      if("Confidence Intervals" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(confidence, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      if("Data Collection" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(data_collection, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      
      if("Descriptive Statistics" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(descriptive_stats, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      
      if("Scope of Conclusions" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(conclusions, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      
      if("Significance" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(significance, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      
      if("Simulation" %in% input$concepts_sub){
        selected_columns <- c(selected_columns, grep(paste(simulation, collapse = "|"),
                                                     colnames(filteredData), value = TRUE))
        subcategories_selected <- TRUE
      }
      
      if(subcatgories_selected == FALSE) { 
        selected_columns <- c(selected_columns, grep("^q1[6-9]", colnames(filteredData), value = TRUE), 
                              grep("^q2[0-9]", colnames(filteredData), value = TRUE), 
                              grep("^q3[0-9]", colnames(filteredData), value = TRUE), 
                              grep("^q4[0-7]", colnames(filteredData), value = TRUE))
      }
      if (input$right_wrong == "Correct vs. Incorrect") {
        filteredData <- filteredData |>
          mutate(across(starts_with("q"), ~ if_else(grepl("\\*\\*$", as.character(.)), "correct", "incorrect")))
        print(filteredData$q43.post.c)
      }
    }
    
    if ("Demographics" %in% input$overall_category) {
      selected_columns <- c(selected_columns, grep("^q4[8-9]", colnames(filteredData), value = TRUE), 
                            grep("^q5[0-9]", colnames(filteredData), value = TRUE))
    }
    
    non_question_columns <- colnames(filteredData)[!grepl("^q", colnames(filteredData))]
    selected_columns <- c(non_question_columns, selected_columns)
    
    
    filteredData <- filteredData |>
      select(all_of(selected_columns))
    
    filteredData
  })
    
  output$downloadData <- downloadHandler(
    filename = function() {
      "AllYearsFinalSubset.csv"
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
  
  output$dt_heading <- renderUI({
    HTML("<br>Preview of data:")
  })
  
  output$data_table <- renderTable({
    head(filteredData(), n = 10)
  })
  
}

shinyApp(ui = ui, server = server)

