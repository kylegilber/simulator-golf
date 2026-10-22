library(shiny)
library(dplyr)
library(data.table)

round_data <- fread(file = "tournament_scorecard_data.csv")
shot_data <- fread(file = "shot_data.csv")

par3_rounds <- round_data %>% 
  filter(Tee == "Par3") %>% 
  slice_head(n = 20)

par3_rounds_used <- par3_rounds %>% 
  slice_min(Differential, n = 8, with_ties = FALSE) %>% 
  arrange(desc(`Registration Data`))

par3_handicap <- round(mean(par3_rounds_used$Differential), 1)
combo_rounds <- round_data %>% 
  filter(Tee != "Par3")

combo_rounds_used <- combo_rounds %>% 
  slice_min(Differential, n = nrow(combo_rounds) %/% 2.3) %>% 
  arrange(desc(`Registration Data`))

combo_handicap <- round(mean(combo_rounds_used$Differential), 1)

  colors <- list(
  primary = "#000000",
  secondary = "#1B1A1A",
  tertiary = "#484745",
  highlight = "#282928",
  accent = "#246ED0"
)

ui <- fluidPage(
  
  tags$head(
    
    tags$link(
      rel = "preconnect",
      href = "https://fonts.googleapis.com"
    ),
    
    tags$link(
      rel = "preconnect",
      href = "https://fonts.gstatic.com",
      crossorigin = "anonymous"
    ),
    
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Yuyu+Short&display=swap"
    ),
    
    tags$style(HTML(
      glue::glue("
        :root {{
          --primary: {colors$primary};
          --secondary: {colors$secondary};
          --tertiary: {colors$tertiary};
          --highlight: {colors$highlight};
          --accent: {colors$accent};
        }}"
      ),"
        .yuyu-short-regular {
          font-family: 'Yuyu Short', sans-serif;
          font-style: normal;
        }
        
        body {
          background-color: var(--primary) !important;
          letter-spacing: 0.1rem !important;
        }
        
        .card {
          width: 100%;
          transition: all 0.3s ease;
          background-color: var(--secondary);
          border: 1px solid var(--tertiary);
          border-radius: 10px;
          padding: 1rem;
          color: white;
          margin-bottom: 1rem;
        }
        
        .card.expanded {
          width: 90vw;
          min-height: 80vh;
          position: fixed;
          top: 5vh;
          left: 5vw;
          z-index: 1000;
          background-color: car(--secondary);
          overflow-y: auto;
        }
        
        .card-header {
          font-size: 5.3rem;
          margin: 0.6rem;
        }
        
        .card-subheader {
          font-size: 3.1rem;
          margin: 0.6rem;
        }
        
        .card-text {
          font-size: 2.9rem;
          margin: 0;
        }
        
        .card-num {
          font-size: 3.1rem;
          margin: 0;
        }
        
        .table-text {
          font-size: 2.3rem;
          margin: 0;
        }
        
        .card-btn {
          width: 100%;
          background-color: var(--highlight);
          border: none;
          border-radius: 8px;
          padding: 1rem;
          color: white;
          text-align: left;
          margin-bottom: 1rem;
        }
        
        .card-btn:hover,
        .card-btn:focus {
          background-color: var(--tertiary);
          transition: background-color 0.2s ease;
          border: inherit;
          color: inherit;
        }
        
        .card-btn:active {
          background-color: var(--accent) !important;
          border-color: var(--accent) !important;
          outline: none !important;
        }
        
        .card-row {
          display: flex;
          justify-content: space-between;
          align-items: center;
          width: 100%;
        }
        
        .text-box {
          display: flex;
          min-width: 6rem;
          padding: .5rem 1rem;
          border-radius: 8px;
          background-color: var(--tertiary);
          color: white;
          font-size: 1rem;
          align-items: center;
          justify-content: center;
        }
        
        .rounds-table {
          width: 100%;
          border-collapse: separate;
        }
        
        .rounds-table tbody tr:nth-child(even) td {
          background-color: var(--highlight);
        }
        
        .rounds-table tbody td:first-child {
          border-radius: 8px 0 0 8px;
        }
        
        .rounds-table tbody td:last-child {
          border-radius: 0 8px 8px 0;
        }
        
        .rounds-table th {
          text-align: left;
          padding: 10px;
        }
        
        .rounds-table td {
          padding: 10px;
        }
        
        .rounds-table tbody tr:hover td {
          background-color: var(--accent);
        }
    "))
  ),
  
  fluidPage(
    fluidRow(
      column(
        width = 3,
        div(
          class = "card",
          h2("Handicap", class = "yuyu-short-regular card-header"),
          
          actionButton(
            "cmb_hndcp_btn",
            div(
              class = "card-row",
              p("COMBO", class = "yuyu-short-regular card-text"),
              div(
                class = "text-box",
                p(combo_handicap, class = "yuyu-short-regular card-num")
              )),
            class = "card-btn"
          ),
          
          actionButton(
            "pr3_hndcp_btn",
            div(
              class = "card-row",
              p("PAR 3", class = "yuyu-short-regular card-text"),
              div(
                class = "text-box",
                p(par3_handicap, class = "yuyu-short-regular card-num")
              )),
            class = "card-btn"
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        uiOutput("handicap_rounds")
      )
    )
  )
)

server <- function(input, output) {
  
  selected_handicap <- reactiveVal(NULL)
  
  observeEvent(input$cmb_hndcp_btn, {
    if (identical(selected_handicap(), "Combo")) {
      selected_handicap(NULL)
    } else {
      selected_handicap("Combo")
    }
  })
  
  observeEvent(input$pr3_hndcp_btn, {
    if (identical(selected_handicap(), "Par 3")) {
      selected_handicap(NULL)
    } else {
      selected_handicap("Par 3")
    }
  })
  
  output$handicap_rounds <- renderUI({
    req(selected_handicap())
    
    rounds <- if (selected_handicap() == "Combo") {
      combo_rounds_used 
    } else {
      par3_rounds_used
    }
    
    rounds <- rounds %>% 
      select(
        EVENT = Tournament,
        DATE = `Registration Data`,
        ROUND = Round,
        SCORE = `To Par (Gross)`,
        `HCP DIFF` = Differential
      )
    
    div(
      class = "card expanded-card",
      h2("Rounds Used", class = "yuyu-short-regular card-header"),
      h3(
        paste(toupper(selected_handicap()), " HANDICAP"),
        class = "yuyu-short-regular card-subheader"
      ),
      
      tags$table(
        class = "rounds-table yuyu-short-regular table-text",
        tags$thead(
          tags$tr(
            lapply(names(rounds), tags$th)
          )
        ),
        
        tags$tbody(
          lapply(seq_len(nrow(rounds)), function(i) {
            tags$tr(
              lapply(rounds[i, ], function(x) {
                tags$td(toupper(as.character(x)))
              })
            )
          })
        )
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)