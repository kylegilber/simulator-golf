library(shiny)

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
          font-size: 4.2rem;
        }
        
        .card-text {
          font-size: 2.7rem;
        }
        
        .card-num {
        font-size: 3.3rem;
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
        
        .card-btn:hover {
          background-color: var(--tertiary);
          transition: background-color 0.2s ease;
          border: inherit;
          color: inherit;
        }
        
        .card-btn:focus,
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
          display: inline-block;
          padding: .5rem 1rem;
          border-radius: 8px;
          background-color: var(--tertiary);
          color: white;
          font-size: 1rem;
          align-items: center;
          justify-content: center;
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
                p("2.1", class = "yuyu-short-regular card-num")
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
                p("2.1", class = "yuyu-short-regular card-num")
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
    if (identical(selected_handicap(), "combo")) {
      selected_handicap(NULL)
    } else {
      selected_handicap("combo")
    }
  })
  
  observeEvent(input$pr3_hndcp_btn, {
    if (identical(selected_handicap(), "par3")) {
      selected_handicap(NULL)
    } else {
      selected_handicap("par3")
    }
  })
  
  output$handicap_rounds <- renderUI({
    req(selected_handicap())
    
    if (selected_handicap() == "combo") {
      div(
        class = "card expanded-card",
        h2("Combo Handicap Rounds", class = "yuyu-short-regular card-header"),
        tags$hr()
      )
    } else {
      div(
        class = "card expanded-card",
        h2("Par 3 Handicap Rounds", class = "yuyu-short-regular card-header"),
        tags$hr()
      )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)