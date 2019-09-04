# Import the necessary packages
library(tidyverse)
library(openxlsx)
library(readxl)
library(shiny)
library(shinydashboard)
library(lubridate)
library(basictabler)
library(shinyWidgets)
library(magrittr)
library(kableExtra)
library(knitr)
library(RSQLite)
library(shinythemes)

# Connect to the local SQLite database, read in all the data and disconnect
con <- dbConnect(SQLite(), "meal_plan_data.db")
meals_data <- dbReadTable(con, "meal_plan")
dbDisconnect(con)

# Function to ensure the order of meals goes Breakfast, Lunch, Dinner, Snack even if
# an individual doesn't have recipes for all of these meals
reorder_meals <- function(x) {
  y <- recode(x, Breakfast = 1, Lunch = 2, Dinner = 3, Snack = 4)
  x[order(y)]
}

# Get the date of the most recent Monday (allowing people to create a meal plan for the
# week already started)
most_recent_monday <- Sys.Date() - ddays(as.numeric(strftime(Sys.Date(), "%u")) - 1)

# Vector of mondays that the user can choose to start their meal plan
start_date_choices <- seq.Date(most_recent_monday, most_recent_monday + dweeks(4), by = 7) %>%
  set_names(map(., ~format(., "%d %B, %Y")))

# Modal to let user choose whether to create a meal plan or upload a new recipe
pre_start_modal <- function() {
  modalDialog(
    fluidRow(
    column(6, align = "center", actionButton("plan", "Create a Meal Plan")),
    column(6, align = "center", actionButton("upload", "Upload a New Recipe"))
    ),
    
    footer = NULL
    
  )
}

# Modal to get the user's name to get their specific recipes and the start date for the meal
# plan
startup_modal <- function() {
  modalDialog(
    
    selectInput("name", h4("Select Your Name"), 
                choices = meals_data %>% pull(Name) %>% unique()),
    
    selectInput("start_date", "For week starting:", choices = start_date_choices),
    
    footer = tagList(
      actionButton("submit_name", "Submit")
    )
  )
}


# Modal to create and upload a recipe to the SQLite database
meal_upload_modal <- function() {
  modalDialog(size = "l", title = h3("Upload a Recipe", align = "center"),
    fluidRow(
      column(6, align = "center", 
             textInput("up_name", "Enter your name")),
      column(6, align = "center", 
             textInput("up_recipe", "Give a short name for your recipe"))
      ),
    fluidRow(
      column(6, align = "center", 
             selectInput("up_meal", "What meal is this recipe for?", 
                         choices = c("", "Breakfast", "Lunch", "Dinner", "Snack"))),
      column(6, align = "center", 
             numericInput("num_ingred", "How many ingredients?", value = 1,
                             min = 0, max = 15))
    ),
    fluidRow(
      column(12, align = "center", h4("Add Ingredients to your Recipe"))
    ),
    uiOutput("ingred_upload"),
    fluidRow(
      # Error message appears if not all fields are completed
      column(12, align = "center", span(textOutput("upload_error"), style="color:red"))
    ),
    
    footer = tagList(
      actionButton("upload_recipe", "Create Recipe")
    )
  )
}

# Create inputs to add ingredients to a recipe
get_ingred_inputs <- function(num) {
  fluidRow(
    column(4, textInput(paste0("ingred", num), "Ingredient")),
    column(4, textInput(paste0("quant", num), "Quantity")),
    column(4, textInput(paste0("units", num), "Units (e.g. g, ml, item)"))
  )
}

# List of days with abbreviations and long-forms
days <- list(
  c("mon", "Monday"),
  c("tue", "Tuesday"),
  c("wed", "Wednesday"),
  c("thu", "Thursday"),
  c("fri", "Friday"),
  c("sat", "Saturday"),
  c("sun", "Sunday")
)


# Function to generate inputs to select a recipe for a given day and meal
create_meal_input <- function(day, meal, meals_data) {
  meal_choices <- c("") %>%
    c(meals_data %>% filter(Meal == meal) %>%
    pull(Recipe) %>%
    unique())
  pickerInput(paste0(day[1], substring(meal, 1, 1)), meal, choices = meal_choices)
}

# Function to create a column of inputs for each meal on a given day
create_input_col <- function(day, width=NULL, meals_data) {
  column(width, align = "center",
         h4(day[2]),
         map(meals_data %>% pull(Meal) %>% unique() %>%
               reorder_meals(),
             ~create_meal_input(day = day[1], ., meals_data = meals_data))
  )
}


#---------------------------------------- UI -------------------------------------------

ui <- fluidPage(
  # Use the 'readable' bootstrap theme from shinythemes package
  theme = shinytheme("readable"),
  # Create a centered title panel 
  titlePanel(h1("Meal Plan", align = "center"), windowTitle = "Meal Plan"),
  br(),
  # Display text showing the week that the meal plan is for
  h4(textOutput("week_starting"), align = "center"),
  br(),
  # Display all the recipe choice inputs
  uiOutput("inputs_table"),
  br(),
  
  fluidRow(
    column(width = 12, align = "center",
           # Show a table displaying the recipes selected for each meal for each day
           tableOutput("meal_plan")
    )
  ),
  br(),
  fluidRow(
    column(width = 12, align = "center",
           # Display the shopping list calculated from the recipe choices
           tableOutput("shopping_list")
    )
  ),
  # Display the download bttns (from ShinyWidgets) that allow download to Excel or PDF
  uiOutput("download_bttns"),
  br()
)

#----------------------------------------- SERVER -----------------------------------------
server <- function(input, output, session) {
  
  # First, show the modal to choose whether to create a meal plan or upload a new recipe
  showModal(pre_start_modal())
  
# -------------------------- UPLOAD A RECIPE SERVER LOGIC
  # If the 'upload' button is pressed on the pre_start modal, show the meal upload modal
  observeEvent(input$upload, {
    removeModal()
    showModal(meal_upload_modal())
  })
  
  # uiOutput to display ingredient inputs depending on the number of the ingredients chosen
  # by the user
  output$ingred_upload <- renderUI({
    map(1:input$num_ingred, get_ingred_inputs)
  })
  
  # When the user attempts to upload a recipe:
  observeEvent(input$upload_recipe, {
    
    # Collect together all the inputs on the recipe upload modal
    inputs <- c("up_name", "up_meal", "up_recipe", "num_ingred",
                map_chr(1:input$num_ingred, ~paste0("ingred", .)),
                map_chr(1:input$num_ingred, ~paste0("quant", .)),
                map_chr(1:input$num_ingred, ~paste0("units", .)))
    
    # If all the inputs are 'truthy' i.e. completed then:
    if(all(map_lgl(inputs, ~isTruthy(input[[.]])))) {
      # Put the input values in a tibble that contains all the data for a new recipe
      new_recipe <- 
        tibble(Name = input$up_name,
               Meal = input$up_meal,
               Recipe = input$up_recipe,
               Ingredient = map_chr(1:input$num_ingred, ~input[[paste0("ingred", .)]]),
               Quantity = map_chr(1:input$num_ingred, ~input[[paste0("quant", .)]]),
               Units = map_chr(1:input$num_ingred, ~input[[paste0("units", .)]])
        )
      # Connect to the SQLite database
      con <- dbConnect(SQLite(), "meal_plan_data.db")
      # Append the new recipe table to the pre-existing recipe data in the database
      dbAppendTable(con, "meal_plan", new_recipe)
      # Disconnect from the database and remove the modal
      dbDisconnect(con)
      removeModal()
      # Send a sweet alert (pop-up message) to the user indicating that the upload was 
      # successful
      sendSweetAlert(session, 
                     text = "Recipe Successfully Uploaded! Reload this page to upload another recipe or create a meal plan.", 
                     type = "success", btn_labels = "Close")
    # When not all inputs have been completed:
    } else {
      # Do nothing except render an error message
      output$upload_error <- renderText({
        "One of the fields is empty, please enter all fields before attempting to upload"
      })
    }
  })

#------------------------------------ CREATE MEAL PLAN SERVER LOGIC
  
  # When the user opts to create a meal plan, show the startup modal
  observeEvent(input$plan, {
    removeModal()
    showModal(startup_modal())
  })
  
  # Remove the modal when the submit button is pressed
  observeEvent(input$submit_name, {
    removeModal()
  })
  
  # Render message to show the week the meal plan is being made for
  observeEvent(input$submit_name, ignoreInit = TRUE, {
    output$week_starting <- renderText({
      paste("For Week Starting", strftime(input$start_date, "%A %d %B, %Y"))
    })
  })
  
  # Using the name inputted in the startup modal, filter the data imported from SQLite
  meals_detail <- eventReactive(input$submit_name, ignoreInit = TRUE, {
    meals_data %>% filter(Name == input$name)
  })
  
  # Create the inputs table by mapping the create_input_col function over each day of the week
  output$inputs_table <- renderUI({
    fluidRow(
      column(1),
      map(days[1:5], create_input_col, meals_data = meals_detail(), width = 2),
      column(1),
      map(days[6:7], create_input_col, meals_data = meals_detail(), width = 4),
      column(4, br(), br(), br(), br(), br(), br(), br(),
             actionButton("submit_meals", h4("Create Meal Plan"), width = "100%"), br())
    )
  })
  
  # Collect together the meal plan inputs in a tibble
  meal_plan_inputs <- eventReactive(input$submit_meals, {
    tibble(
      Day = map_chr(days, 2),
      Snack = map_chr(days, ~input[[paste0(.[1], "S")]]),
      Breakfast = map_chr(days, ~input[[paste0(.[1], "B")]]),
      Lunch = map_chr(days, ~input[[paste0(.[1], "L")]]),
      Dinner = map_chr(days, ~input[[paste0(.[1], "D")]])
    )
  })
  
  # For the meal plan table, transpose the tibble so the rows are meals and the columns are 
  # days of the week
  meal_plan_output <- eventReactive(input$submit_meals, {
    meal_plan_inputs() %>%
      column_to_rownames("Day") %>%
      t() %>%
      as_tibble(rownames = "Meal")
  })
  
  # Render the meal plan table
  output$meal_plan <- renderTable({
    meal_plan_output()
  })
  
  # For each recipe chosen, replicate the ingredient data the number of times it is chosen
  # Then group the data by ingredient and find the total quantity to generate the shopping list
  shopping_list_data <- eventReactive(input$submit_meals, ignoreInit = TRUE, {
    meals_detail_temp <- meals_detail() %>%
      select(Recipe, Ingredient, Quantity, Units) %>%
      group_split(Recipe) %>%
      set_names(map_chr(., ~pull(., Recipe) %>% unique())) %>%
      map(~select(., -Recipe))
      
    meal_plan_inputs() %>%
      select(-Day) %>%
      unlist(use.names = F) %>%
      # Create a frequency table from the recipes chosen
      table() %>% 
      as.list() %>%
      {map2(., names(.), ~replicate(.x, meals_detail_temp[[.y]], simplify = FALSE) %>%
              bind_rows())} %>%
      bind_rows() %>%
      group_by(Ingredient, Units) %>%
      summarise(Quantity = sum(Quantity)) %>%
      ungroup() %>%
      transmute(Item = paste(Quantity, Units, Ingredient)) %>%
      mutate(Item = str_remove(Item, " item"))
  })
  
  # Render the shopping list table  
  output$shopping_list <- renderTable({
    shopping_list_data()
  })
  
  # Render the download bttns when the meal plan is created
  observeEvent(input$submit_meals, {
    output$download_bttns <- renderUI({
      fluidRow(
        column(2),
        column(4, align = "center", downloadBttn("download_excel", "Export to Excel", size = "lg")),
        column(4, align = "center", downloadBttn("download_pdf", "Export to PDF", size = "lg")),
        column(2)
      )
    })
  })
  
  # For Excel downloads, create a workbook with two worksheets, one for the meal plan and 
  # one for the shopping list
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0(input$name, "_Meal Plan_", input$start_date, ".xlsx")
    },
    content = function(file) {
      WB <- createWorkbook()
      addWorksheet(WB, sheetName = "Meal Plan")
      addWorksheet(WB, sheetName = "Shopping List")
      writeDataTable(WB, sheet = "Meal Plan", x = meal_plan_output())
      writeDataTable(WB, sheet = "Shopping List", x = shopping_list_data())
      saveWorkbook(WB, file, overwrite = TRUE)
    }
  )
  
  # For PDF downloads, run an RMarkdown script to generate a two-page PDF, with the shopping
  # list followed by the meal plan
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0(input$name, "_Meal Plan_", input$start_date, ".pdf")
    },
    content = function(file) {
      mp <- meal_plan_output() %>%
        # Create linebreaks in the meal plan table to make it take up more space
        mutate_all(~paste0("\n\n", ., "\n\n\n")) %>%
        mutate_all(linebreak) %>%
        set_colnames(linebreak(paste0("\n", colnames(.), "\n\n")))
      sl <- shopping_list_data()
      tempReport <- file.path(tempdir(), "meal_plan_export.Rmd")
      file.copy("meal_plan_export.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file)
    }
  )
  
}

shinyApp(ui, server)
