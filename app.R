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
        
        .panel {
          background-color: var(--secondary);
          border: 1px solid var(--tertiary);
          border-radius: 10px;
          padding: 1rem;
          color: white;
        }
        
        .panel-header {
          font-size: 4.2rem;
        }
        
        .panel-text {
          font-size: 2.7rem;
        }
        
        .panel-num {
        font-size: 3.3rem;
        }
        
        .panel-btn {
          width: 100%;
          background-color: var(--highlight);
          border: none;
          border-radius: 8px;
          padding: 1rem;
          color: white;
          text-align: left;
          margin-bottom: 1rem;
        }
        
        .panel-btn:hover {
          background-color: var(--tertiary);
          transition: background-color 0.2s ease;
          border: inherit;
          color: inherit;
        }
        
        .panel-btn:active {
          background-color: var(--accent) !important;
          border-color: var(--accent) !important;
          outline: none !important;
        }
        
        .panel-row {
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
    
    # Handicap
    column(
      width = 3,
      div(
        class = "panel",
        h2("Handicap", class = "yuyu-short-regular panel-header"),
        
        # Combo
        actionButton(
          "cmb_hndcp_btn",
          div(
            class = "panel-row",
            p("COMBO", class = "yuyu-short-regular panel-text"),
            div(
              class = "text-box",
              p("2.1", class = "yuyu-short-regular panel-num")
            )
          ),
          class = "panel-btn"
        ),
        
        # Par 3
        actionButton(
          "pr3_hndcp_btn",
          div(
            class = "panel-row",
            p("PAR 3", class = "yuyu-short-regular panel-text"),
            div(
              class = "text-box",
              p("2.1", class = "yuyu-short-regular panel-num")
            )
          ),
          class = "panel-btn"
        )
      )
    )
  )
)

server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)