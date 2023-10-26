library(shiny)
library(readxl)
library(torch)

# Define UI
ui <- fluidPage(
  titlePanel("Telemore Biology Disorder and TEC Prediction "),
  sidebarLayout(
    sidebarPanel(
      textInput("ref", "Ref:", ""),
      textInput("position", "Position:", ""),
      textInput("mut", "Mut:", ""),
      actionButton("predictButton", "Predict")
    ),
    mainPanel(
      h3("Predicted Mean_TEC:"),
      verbatimTextOutput("predictionText")
    )
  )
)

# Load your trained model
model <- torch_load("/Users/koissi/Desktop/Oncology/TERT/Model_TERT/TBD/trained_model.pth")

# Load the dictionaries
tert_rasp <- read_excel("/Users/koissi/Downloads/TERT_input.xlsx", sheet = "TERT_RASP_scores") %>% 
  mutate(Position = as.numeric(Position)) %>% 
  select(RSAP_score, Ref, Mut, Position)

tert_bioc <- read_excel("/Users/koissi/Downloads/TERT_input.xlsx", sheet = "Biochemical_secondary_structure") %>% 
  select(Position, Residue, Ref_amino_acid_properties, median_RASP, Secondary_structure)

# Define server
server <- function(input, output, session) {
  observeEvent(input$predictButton, {
    # Get user inputs
    ref <- input$ref
    position <- as.numeric(input$position)
    mut <- input$mut
    
    # Define the Domain variable based on Position
    domain <- ifelse(position >= 0 & position <= 120, "TEN",
                     ifelse(position >= 121 & position <= 300, "Linker",
                            ifelse(position >= 301 & position <= 400, "TRBD",
                                   ifelse(position >= 401 & position <= 750, "Reverse transcriptase",
                                          ifelse(position >= 751 & position <= 1100, "CTE",
                                                 NA)))))
    
    # Debugging: Print input values
    print(paste("Ref:", ref))
    print(paste("Position:", position))
    print(paste("Mut:", mut))
    
    # Load helper functions
    source("/Users/koissi/Desktop/Oncology/TERT/Model_TERT/TBD/fetch_RASP_scores.R")
    source("/Users/koissi/Desktop/Oncology/TERT/Model_TERT/TBD/fetch_biochemical_structure.R")
    
    # print("tert_rasp:")
    # print(tert_rasp)
    # print("tert_bioc:")
    # print(tert_bioc)
    
    # print(unique(tert_rasp$Ref))
    # print(unique(tert_rasp$Position))
    # print(unique(tert_rasp$Mut))
    # 
    
    
    # Fetch data from dictionaries based on user inputs
    rasp_score <- tryCatch({
      fetch_RASP_scores(tert_rasp, Ref, position, mut)
    }, error = function(e) {
      print(paste("Error fetching RASP scores:", e$message))
      return(NULL)
    })
    
    biochemical_structure <- tryCatch({
      fetch_biochemical_structure(tert_bioc, Residue, position)
    }, error = function(e) {
      print(paste("Error fetching biochemical structure:", e$message))
      return(NULL)
    })
    
    if (is.null(rasp_score) || is.null(biochemical_structure)) {
      # Handle data retrieval errors gracefully
      print("Data retrieval failed. Check the input data and dictionaries.")
      return(NULL)
    }
    
    # Create a data frame with the fetched data
    input_data <- data.frame(Position = position, RSAP_score = rasp_score)
    
    # Debugging: Print input data
    print("Input Data:")
    print(input_data)
    
    # Convert data frame to tensors
    e_cont <- input_data %>% as.matrix() %>% torch_tensor(., dtype = torch_float32())
    
    # Create a data frame for categorical data
    cat_data <- data.frame(Ref = ref, Mut = mut)
    
    # Convert categorical data to integer factors
    cat_data <- cat_data %>%
      mutate_all(~ as.integer(factor(.)))  # Convert to integer factors
    
    # Convert categorical data to tensor with dtype torch_int()
    e_cat <- cat_data %>%
      as.matrix() %>%
      torch_tensor(., dtype = torch_int())
    
    # Make predictions using the model
    predicted_TEC <- tryCatch({
      model(e_cont, e_cat)
    }, error = function(e) {
      print(paste("Prediction error:", e$message))
      return(NULL)
    })
    
    if (is.null(predicted_TEC)) {
      # Handle prediction errors gracefully
      print("Prediction failed. Check the input data and model.")
      return(NULL)
    }
    
    # Extract the mean_TEC prediction
    predicted_mean_TEC <- as.array(predicted_TEC)[1, 1]
    
    # Debugging: Print predicted mean_TEC
    print(paste("Predicted Mean_TEC:", round(predicted_mean_TEC, 2)))
    
    # Display the predicted mean_TEC to the user
    output$predictionText <- renderText({
      paste("Predicted Mean_TEC:", round(predicted_mean_TEC, 2))
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
