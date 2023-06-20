library(shiny)
library(shinydashboard)
library(DBI)
library(DT)
library(RMySQL)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(caret)
library(textdata)
#library(e1071)
library(syuzhet)
library(sentimentr)
library(wordcloud)
library(shinyWidgets)


ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Clothing Review"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("fas fa-home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Add Review", tabName = "add_review", icon = icon("plus")),
      menuItem("Update Review", tabName = "update_review", icon = icon("pencil")),
      menuItem("Delete Review", tabName = "delete_review", icon = icon("trash")),
      menuItem("Sentimental Analysis", tabName = "model", icon = icon("dashboard"))
    )
    
  ),
 # dashboardBody(
  #  tabItems(
   #   tabItem(tabName = "home",
    #          h1("Home")
 dashboardBody(
   tags$style(".content-wrapper {background-image: url('https://www.riverparksquare.com/media/images/_1200x630_crop_center-center_82_none/priscilla-du-preez-228220-unsplash.jpg'); background-size: cover;}"),
   tabItems(
     tabItem(tabName = "home",
             h1("Overview of the application"),
             br(),
             br(),
             
             p("Our RShiny web application provides a user-friendly interface for exploring and managing a dataset of women's e-commerce clothing reviews. With our dynamic dashboard, you can quickly visualize and analyze key metrics such as product ratings, review sentiments, and more.",style = "font-family: Display; font-size: 20px;line-height: 1.5;font-weight:bold;"),
             br(),
             p("Our application is designed to make it easy for you to perform CRUD operations on the dataset. You can add new reviews to the database, update existing reviews, and even delete reviews that are no longer relevant. We've also included javascript validations to help prevent errors and ensure that your CRUD operations are executed smoothly.Our web application is connected to an SQL database, ensuring that your data is always secure and up-to-date.",style = "font-family:Display; font-size: 20px;line-height: 1.5;font-weight:bold;"),
             br(),
             p("The interface is intuitive and user-friendly, making it easy for you to navigate and interact with the data.Whether you're a data analyst or a business owner, our application provides a powerful tool for gaining insights into this important dataset.",style = "font-family: Display; font-size: 20px;line-height: 1.5;font-weight:bold;"),
             br(),
             p("We're excited to offer this RShiny web application as a valuable resource for anyone looking to explore and manage women's e-commerce clothing review data.",style = "font-family: Display; font-size: 20px;line-height: 1.5;font-weight:bold;"),
             br(),
             br(),
             br(),
             br(),
             p("If you have any questions or feedback, please don't hesitate to reach out to us.",style = "font-family: Display; font-size: 17px;line-height: 1.5;")
                 
 
 
 
  ),
    
     tabItem(
       tabName = "dashboard",
       
       fluidRow(
         column(
           width = 3,
           sliderInput(
             "age_slider",
             "Age Range",
             min = 18,
             max = 100,
             value = c(18, 100),
             step = 1
           )
         ),
         column(
           width = 3,
           textInput("clothing_input", "Clothing ID:"),
         ),
         column(
           width = 3,
           selectInput(
             "division_input",
             "Division Name:",
             choices = c("All", "General", "General Petite", "Initmates"),
             selected = "All"
           )
         ),
         column(
           width = 3,
           selectInput(
             "department_input",
             "Department Name:",
             choices = c("All", "Bottoms", "Dresses", "Intimate", "Jackets", "Tops", "Trend"),
             selected = "All"
           )
         ),
         column(
           width = 3,
           selectInput(
             "class_input",
             "Class Name:",
             choices = c("All", "Blouses", "Casual bottoms", "Chemises", "Dresses", "Fine gauge", "Intimates", "Jackets", "Jeans", "Knits", "Layering", "Legwear", "Outerwear", "Lounge", "Pants", "Shorts", "Skirts", "Sleep", "Sweaters", "Swim", "Trend"),
             selected = "All"
           )
         )
       ),
       
      
     fluidRow(
       column(
         width = 4,
         plotOutput("age_histogram")
       ),
       column(
         width = 4,
         plotOutput("two")
       ),
       column(
         width = 4,
         plotOutput("fifthhistogram")
       )
     ),
     fluidRow(
       column(
         width = 12,
         plotOutput("one")
       )
     ),
     fluidRow(
       column(
         width = 6,
         plotOutput("fourthhistogram")
       ),
       column(
         width = 6,
         plotOutput("three")
       )
     )
     
     ),
     
      tabItem(tabName = "add_review",
              fluidRow(
                box(width = 12,
                    title = "Add Review",
                    solidHeader = TRUE,
                    status = "danger",
                    uiOutput("review_id"),
                    textInput("name", "Name", ""),
                    textInput("clothing_id", "Clothing ID", ""),
                    numericInput("age", "Age", value = 0, min = 0, max = 120),
                    textInput("title", "Title", ""),
                    textAreaInput("review_text", "Review Text", ""),
                    numericInput("rating", "Rating", value = 0, min = 0, max = 5),
                    radioButtons("recommended", "Would you recommend this item?", c("Yes" = 1, "No" = 0)),
                   # textInput("division_name", "Division Name", ""),
                    #textInput("department_name", "Department Name", ""),
                    selectInput("division_name", "Division Name:",
                                choices = c("General","General Petite","Initmates"),
                                selected = "All"),
                    selectInput("department_name", "Department Name:",
                                choices = c("Bottoms","Dresses","Intimate","Jackets","Tops","Trend"),
                                selected = "All"),
                    #textInput("class_name", "Class Name", ""),
                    selectInput("class_name", "Class Name:",
                               choices = c("Blouses","Casual bottoms","Chemises","Dresses","Fine gauge","Intimates","Jackets","Jeans","Knits","Layering","Legwear","Outerwear","Lounge","Pants","Shorts","Skirts","Sleep","Sweaters","Swim","Trend"),
                               selected = "All"),
                    actionButton("submit", "Submit")
                    
                )
              )
      ),
      tabItem(tabName = "update_review",
              #sidebarLayout(
               # sidebarPanel(
                #  textInput("search_name", "Name", ""),
                 # actionButton("search", "Search Reviews")
                #),
                
                #mainPanel(
                 # dataTableOutput("search_results"),
                #  actionButton("update_reviews", "Update Selected Reviews")
                #)
              #)
              fluidRow(
                box(width = 12,
                    title = "Enter your name to view your reviews",
                    solidHeader = TRUE,
                    status = "danger",
                    textInput("search_name", "Name", ""),
                    actionButton("search", "Search Reviews"),
                    br(),
                    br(),
                    dataTableOutput("search_results")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Update your review here",
                    solidHeader = TRUE,
                    status = "danger",
                    textInput("ureview_id","Review ID",""),
                    #uiOutput("name", "Name", ""),
                    textInput("uclothing_id", "Clothing ID", ""),
                    numericInput("uage", "Age", value = 0, min = 0, max = 120),
                    textInput("utitle", "Title", ""),
                    textAreaInput("ureview_text", "Review Text", ""),
                    numericInput("urating", "Rating", value = 0, min = 0, max = 5),
                    radioButtons("urecommended", "Would you recommend this item?", c("Yes" = 1, "No" = 0)),
                   # textInput("udivision_name", "Division Name", ""),
                   selectInput(
                     "udivision_name",
                     "Division Name:",
                     choices = c("All", "General", "General Petite", "Initmates"),
                     selected = "All"
                   ),
                    #textInput("udepartment_name", "Department Name", ""),
                   selectInput(
                     "udepartment_name",
                     "Department Name:",
                     choices = c("All", "Bottoms", "Dresses", "Intimate", "Jackets", "Tops", "Trend"),
                     selected = "All"
                   ),
                    #textInput("uclass_name", "Class Name", ""),
                   selectInput("uclass_name", "Class Name:",
                               choices = c("Blouses","Casual bottoms","Chemises","Dresses","Fine gauge","Intimates","Jackets","Jeans","Knits","Layering","Legwear","Outerwear","Lounge","Pants","Shorts","Skirts","Sleep","Sweaters","Swim","Trend"),
                               selected = "All"),
                    actionButton("update_reviews", "Update")
                    
                )
              )
      ),
      tabItem(tabName = "delete_review",
              fluidRow(
                box(width = 12,
                    title = "Enter your name to view your reviews",
                    solidHeader = TRUE,
                    status = "danger",
                    textInput("dsearch_name", "Name", ""),
                    actionButton("dsearch", "Search Reviews"),
                    br(),
                    br(),
                    textInput("delete_review_id", "Review ID to be deleted", ""),
                    actionButton("delete", "Delete"),
                    br(),
                    br(),
                    br(),
                    br(),
                    dataTableOutput("dsearch_results")
                )
              )
      ),
  tabItem(tabName = "model",
          #h1("Model")
          textInput("input_review_id", label = "Enter the Review ID:"),
          verbatimTextOutput("sentiment_output"),
          #wordcloud2Output("wordcloud")
  )
  
    )
  )
) 


con <- dbConnect(RMySQL::MySQL(),
                 dbname='reviews',
                 host='localhost',
                 port=3306,
                 user='root',
                 password='Niranjana@25')


server <- function(input, output, session) {
  useShinyjs()
  get_max_review_id <- function() {
    result <- dbGetQuery(con, "SELECT MAX(Review_Id) as max_review_id FROM reviews")
    max_review_id <- result$max_review_id + 1
    return(max_review_id)
  }
  
  review_id <- reactive({
    review_id <- get_max_review_id()
    return(review_id)
  })
  
  #output$review_id <- renderUI({
   # h3(paste("Review ID:", review_id()))
  #})

  output$review_id <- renderText({
    paste("Review ID:", review_id())
  })
  
 
  observeEvent(input$submit, {
    
    # validate inputs
    if (is.null(input$name) || input$name == "") {
      showNotification("Error: Name cannot be empty!", type = "error")
    } else if (is.null(input$clothing_id) || input$clothing_id == "") {
      showNotification("Error: Clothing ID cannot be empty!", type = "error")
    } else if (input$age == 0) {
      showNotification("Error: Age cannot be 0!", type = "error")
    } else if (is.null(input$title) || input$title == "") {
      showNotification("Error: Title cannot be empty!", type = "error")
    } else if (is.null(input$review_text) || input$review_text == "") {
      showNotification("Error: Review text cannot be empty!", type = "error")
    } else if (input$rating == 0) {
      showNotification("Error: Rating cannot be 0!", type = "error")
    } else if (is.null(input$division_name) || input$division_name == "") {
      showNotification("Error: Division name cannot be empty!", type = "error")
    } else if (is.null(input$department_name) || input$department_name == "") {
      showNotification("Error: Department name cannot be empty!", type = "error")
    } else if (is.null(input$class_name) || input$class_name == "") {
      showNotification("Error: Class name cannot be empty!", type = "error")
    } else {
      review_id <- get_max_review_id()
      # insert new review
      dbGetQuery(con, paste0("INSERT INTO reviews (Name,Review_Id, Clothing_id, Age, Title, Review_Text, Rating, Recommended_IND, Division_Name, Department_Name, Class_Name) VALUES ('",input$name, "',", review_id, ",", input$clothing_id, ",", input$age, ",'", input$title, "','", input$review_text, "',", input$rating, ",", input$recommended, ",'", input$division_name, "','", input$department_name, "','", input$class_name, "')"))
      showNotification("Successfully added your review with review_id: ", review_id, "!", type = "success")
    }
  })
  
  search_reviews <- function(search_name) {
    if (is.null(search_name) || search_name == "") {
      showNotification("Error: Name cannot be empty!", type = "error")
    } else {
      reviews <- dbGetQuery(con, paste0("SELECT * FROM reviews WHERE name='", search_name, "'"))
      return(reviews)
    }
  }
  
  review_data <- reactive({
    results <- search_reviews(input$search_name)
    if (nrow(results) > 0) {
      results
    }
  })
  
  observeEvent(input$search, {
    results <- search_reviews(input$search_name)
    if (nrow(results) > 0) {
      output$search_results <- renderDataTable({
        data.frame(results)
      }, options = list(pageLength = 10), selection = "multiple")
    } else {
      showNotification("No reviews found!", type = "warning")
    }
  })
  
 # observeEvent(input$update_reviews, {
  #  selected_reviews <- input$search_results_rows_selected
   # selected_review_ids <- review_data()[review_data()$Review_Id %in% selected_reviews, "Review_Id"]
    
  #  if(length(selected_reviews) == 0){
   #   showNotification("Error: No reviews selected!", type = "error")
  #  } else {
      ## Get the selected reviews from the data table
      #selected_data <- review_data()[review_data()$Review_Id %in% selected_reviews, ]
   #   selected_data <- review_data()[review_data()$Review_Id %in% selected_review_ids, ]
    #  print(selected_data)
      
     # # Loop through each selected review and update it in the database
      #for(i in 1:nrow(selected_data)){
       # review_id <- as.integer(selected_data[i, "Review_Id"])
        #if (is.na(review_id)) {
         # showNotification("Error: Review ID cannot be NA!", type = "error")
          #next
        #}
        #name <- selected_data[i, "Name"]
        #clothing_id <- as.integer(selected_data[i, "Clothing_id"])
        #age <- as.integer(selected_data[i, "Age"])
        #title <- selected_data[i, "Title"]
        #review_text <- selected_data[i, "Review_Text"]
        #rating <- as.integer(selected_data[i, "Rating"])
        #recommended <- as.integer(selected_data[i, "Recommended_IND"])
        #division_name <- selected_data[i, "Division_Name"]
        #department_name <- selected_data[i, "Department_Name"]
        #class_name <- selected_data[i, "Class_Name"]
        
        # Update the review in the database
      #  query <- paste0("UPDATE reviews SET Name = '", name, "', Clothing_id = ", clothing_id, ", Age = ", age, ", Title = '", title, "', Review_Text = '", review_text, "', Rating = ", rating, ", Recommended_IND = ", recommended, ", Division_Name = '", division_name, "', Department_Name = '", department_name, "', Class_Name = '", class_name, "' WHERE Review_Id = ", review_id)
       # print(query)
        #dbGetQuery(con, query)
      #}
      
  
  #    showNotification("Successfully updated the selected reviews!", type = "success")
   # }
  #})
  observeEvent(input$update_reviews, {
    if (nchar(input$ureview_id) == 0) {
      showNotification("Review ID cannot be empty!", type = "error")
    } else if (nchar(input$uclothing_id) == 0) {
      showNotification("Clothing ID cannot be empty!", type = "error")
    } else if (input$uage == 0) {
      showNotification("Age cannot be empty!", type = "error")
    } else if (nchar(input$utitle) == 0) {
      showNotification("Title cannot be empty!", type = "error")
    } else if (nchar(input$ureview_text) == 0) {
      showNotification("Review Text cannot be empty!", type = "error")
    } else if (input$urating == 0) {
      showNotification("Rating cannot be empty!", type = "error")
    } else if (is.na(input$urecommended)) {
      showNotification("Would you recommend this item? cannot be empty!", type = "error")
    } else if (nchar(input$udivision_name) == 0) {
      showNotification("Division Name cannot be empty!", type = "error")
    } else if (nchar(input$udepartment_name) == 0) {
      showNotification("Department Name cannot be empty!", type = "error")
    } else if (nchar(input$uclass_name) == 0) {
      showNotification("Class Name cannot be empty!", type = "error")
    } else {
      query <- paste0("UPDATE reviews SET Clothing_id = ", input$uclothing_id,
                      ", Age = ", input$uage,
                      ", Title = '", input$utitle,
                      "', Review_Text = '", input$ureview_text,
                      "', Rating = ", input$urating,
                      ", Recommended_IND = ", input$urecommended,
                      ", Division_Name = '", input$udivision_name,
                      "', Department_Name = '", input$udepartment_name,
                      "', Class_Name = '", input$uclass_name,
                      "' WHERE Review_Id = ", input$ureview_id)
      result <- dbSendQuery(con, query)
      if (dbHasCompleted(result)) {
        showNotification("Successfully updated your review!", type = "success")
      } else {
        showNotification("Error updating review, Check your review ID !", type = "error")
      }
    }
  })
  
  dsearch_reviews <- function(dsearch_name) {
    if (is.null(dsearch_name) || dsearch_name == "") {
      showNotification("Error: Name cannot be empty!", type = "error")
    } else {
      dreviews <- dbGetQuery(con, paste0("SELECT * FROM reviews WHERE name='", dsearch_name, "'"))
      return(dreviews)
    }
  }
  
  dreview_data <- reactive({
    dresults <- dsearch_reviews(input$dsearch_name)
    if (nrow(dresults) > 0) {
      dresults
    }
  })
  
  observeEvent(input$dsearch, {
    dresults <- dsearch_reviews(input$dsearch_name)
    if (nrow(dresults) > 0) {
      output$dsearch_results <- renderDataTable({
        data.frame(dresults)
      }, options = list(pageLength = 10), selection = "multiple")
    } else {
      showNotification("No reviews found!", type = "warning")
    }
  })
  
  
  observeEvent(input$delete, {
    if (is.null(input$delete_review_id) || input$delete_review_id == "") {
      showNotification("Review ID is mandatory to delete a review!", type = "danger")
    } else {
      
      dbSendQuery(con, paste0("DELETE FROM reviews WHERE Review_Id = ", input$delete_review_id))
      showNotification("Successfully deleted your review!", type = "success")
    }
  })
  
 # womens_ecommerce_data <- dbGetQuery(con, "SELECT * FROM reviews")
  
  #filtered_data <- reactive({
   # data <- womens_ecommerce_data
  #  data <- filter(data, Age >= input$age_slider[1] & Age <= input$age_slider[2])
  #  if (input$division_input != "All") {
   #   data <- filter(data, Division_Name == input$division_input)
  #  }
  #  if (input$department_input != "All") {
  #    data <- filter(data, Department_Name == input$department_input)
  #  }
  #  if (input$class_input != "All") {
  #    data <- filter(data, Class_Name == input$class_input)
  #  }
  
   # return(data)
  #})
  
  womens_ecommerce_data <- dbGetQuery(con, "SELECT * FROM reviews")
  
  filtered_data <- reactive({
    data <- womens_ecommerce_data
    data <- filter(data, Age >= input$age_slider[1] & Age <= input$age_slider[2])
    if (input$division_input != "All") {
      data <- filter(data, Division_Name == input$division_input)
    }
    if (input$department_input != "All") {
      data <- filter(data, Department_Name == input$department_input)
    }
    if (input$class_input != "All") {
      data <- filter(data, Class_Name == input$class_input)
    }
    data <- if (!is.null(input$clothing_input) && input$clothing_input != "") {
      filter(womens_ecommerce_data, Clothing_id == input$clothing_input)
    } else {
      womens_ecommerce_data
    }
    return(data)
  })
  
  
  
  output$age_histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = Age)) +
      geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
      ggtitle("Histogram of Age") +
      xlab("Age") +
      ylab("Count")
  })

  
  output$fourthhistogram <- renderPlot({
    ggplot(filtered_data(), aes(x = Age, y = Rating, color = Class_Name)) +
      geom_point(size = 3) +
      ggtitle("Scatter Plot of Age and Rating by Class") +
      xlab("Age") +
      ylab("Rating") +
      theme_bw()
  })
  
  
  output$fifthhistogram <- renderPlot({
    data <- filtered_data()
    rec_counts <- data %>% 
      group_by(Clothing_id, Recommended_IND) %>% 
      summarise(count = n())
    ggplot(rec_counts, aes(x = Clothing_id, y = count, fill = Recommended_IND)) + 
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Clothing ID") +
      ylab("No of people who reviewed this cloth") +
      ggtitle("How likely a particular cloth is good and recommended to another user?")
  })
  
  
  
  output$one <- renderPlot({
  ggplot(filtered_data(), aes(x = Age, y = Recommended_IND, group = Department_Name, color = Department_Name)) +
    geom_line() +
    ggtitle("Trend of Recommended Indicator by Age and Division") +
    xlab("Age") +
    ylab("Recommended Indicator")
  })
  
  output$two <- renderPlot({
    ggplot(filtered_data(), aes(x = Division_Name, y = Age, fill = Recommended_IND)) +
      geom_violin() +
      ggtitle("Distribution of Age by Division and Recommended Indicator") +
      xlab("Division Name") +
      ylab("Age") +
      scale_fill_manual(values = c("red", "green"))
  })


  
  output$three <- renderPlot({
  ggplot(filtered_data(), aes(x = Department_Name, y = Division_Name)) +
    geom_tile(aes(fill = Age)) +
    ggtitle("Heatmap of Age by Department and Division") +
    xlab("Department Name") +
    ylab("Division Name") +
    scale_fill_gradient(low = "white", high = "blue")
    
})
  output$sentiment_output <- renderText({
    # Get the Review_id from the input text
    review_id <- input$input_review_id
    
    # Build the SQL query to retrieve the Review_Text for the given Review_id
    query <- paste0("SELECT Review_Text FROM reviews WHERE Review_id = '", review_id, "'")
    
    # Execute the SQL query and retrieve the Review_Text
    review_text <- dbGetQuery(con, query)$Review_Text
    
    # Check if review_text is empty or NULL
    if (is.null(review_text) || review_text == "") {
      return("Error: Review text not found.")
    }
    
    # Perform sentiment analysis using the sentimentr package
    sentiment_scores <- sentiment_by(review_text, by = NULL)
    print(class(sentiment_scores))
    # Check if sentiment_scores has zero rows
    if (nrow(sentiment_scores) == 0) {
      return("Error: Sentiment scores not found.")
    }
    
    
    sentiment_label <- ifelse(sentiment_scores$ave_sentiment > 0, "positive",
                              ifelse(sentiment_scores$ave_sentiment < 0, "negative", "neutral"))
    
    # Output the sentiment label to the user
    paste("The sentiment of Review ", review_id, " is:", sentiment_label)
  })
  
  #datas <- womens_ecommerce_data %>%
   # select(Title) %>%
  #  filter(!is.na(Title))
  
  
#  output$wordcloud <- renderWordcloud2({
 #   wordcloud2(datas$Title, size = 0.8, shape = "ellipse", color = "random-light", backgroundColor = "white")
#  })
  
}

shinyApp(ui=ui, server=server)