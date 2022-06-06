library (dplyr)
library (ggplot2)
library (plotly)
library(shiny)
library(stringr)
library (purrr)
library(shiny)
options(shiny.maxRequestSize=30*1024^2) # data max 30 mb 

reading_df <- function (file_path){
  return (read.csv(file_path, na.strings = "."))
}

counting_unique <- function (vec) {
  return (length (unique (vec) ))
}

s_unique <- function (nums){
  return (sort (unique (nums)))
}

# return column names with class numeric or integer 
colnames_numint  <- function (df){
  return (names (sapply (df, class )[sapply (df, class ) == "integer" | 
                                       sapply (df, class ) == "numeric"] ))
}

colnames_char  <- function (df){
  return (names (sapply (df, class )[sapply (df, class ) == "character"] ))
}

colnames_unique  <- function (df, n = 20, less_than = TRUE){
  if (less_than){return (names (sapply (df, counting_unique )[sapply (df, counting_unique ) < n] ))}
  else {return (names (sapply (df, counting_unique )[sapply (df, counting_unique ) > n] ))}
}


stat_box_data <- function(y) {
  return( 
    data.frame(
      y = 1.05*max(y),  #may need to modify this depending on your data
      label = paste('n =', length(y), '\n',
                    'median =', round(median(y, na.rm = T), 2), '\n')
    )
  )
}

stat_box_data2 <- function(y) {
  return( 
    data.frame(
      y = .95 * median(y),  #may need to modify this depending on your data
      label = round(median(y, na.rm = T), 2)
    )
  )
}

remove_duplicate <- function (df, column){
  return (df %>% filter (!duplicated (get(id_col))))
}

covariates_plot <- function (df, cats, conts){
  result <- list ()
  i <- 1
  for (cat in cats){
    for (cont in conts){
      result[[paste0 ("fig",i)]] <- ggplotly(ggplot (df) + 
                                               aes (x = as.character (get(cat)), y = get (cont), fill = as.character (get (cat))) + 
                                               geom_boxplot () + 
                                               stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5,vjust = 0.9) +
                                               #stat_summary(fun.data = stat_box_data2, geom = "text", hjust = 0.5,vjust = 0.9) +
                                               scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
                                               labs (x = cat, y = cont, fill = cat)  )
      i <- i + 1
    }
  }
  return (result)
}

ui <- fluidPage(
  fileInput("file", "File input:"),
  selectInput("id_covs", "ID Columns",NA),
  selectInput("cat_covs", "Categorical Columns",NA,multiple = TRUE),
  selectInput("conti_covs", "Continuous Columns",NA,multiple = TRUE),
  uiOutput("multi_plt_covs")
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session ) {
  
  multi_plot_covs <- function (num){
    output[[paste0("plot_covs_",num)]] <- renderPlotly(li()[[paste0("fig", num)]])
  }
  
  df_unfiltered <- eventReactive(input$file,{
    req (input$file)
    
    return (reading_df(input$file$datapath))
  })
  
  observeEvent(df_unfiltered(),{
    
    req(df_unfiltered())
    
    ### Covariates Tab ###
    updateSelectInput(session, "id_covs", label = "ID Columns",
                      selected = "NMID",
                      choices = names(df_unfiltered()))
    updateSelectInput(session, "cat_covs", label = "Categorical Columns",
                      selected = c("SEX", "RACE"),
                      choices = names(df_unfiltered()))
    updateSelectInput(session, "conti_covs", label = "Continuous Columns", 
                      selected = c('AGEBL','BSABL'),
                      choices = names(df_unfiltered()))
  })
  
  df <- eventReactive(input$id_covs,{
    df <- df_unfiltered() %>% filter (MDV == 0)
    df <- df %>% filter (!duplicated(get(input$id_covs)))
    return (df)
  })
  
  n_plot_covs <- reactive(length(input$cat_covs) * length (input$conti_covs))
  
  
  li <- reactive (covariates_plot(df(), input$cat_covs, input$conti_covs))
  
  output$multi_plt_covs <- renderUI({
    map(paste0 ("plot_covs_",1:n_plot_covs()), ~ plotlyOutput(.x, height = "300px"))
    
    map (1:n_plot_covs(),~multi_plot_covs(.x))
  })
  
  
}


shinyApp(ui = ui, server = server)

