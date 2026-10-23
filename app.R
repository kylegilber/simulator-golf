library(shiny)
library(dplyr)
library(data.table)
library(stringr)

round_data <- fread(file = "tournament_scorecard_data.csv")
shot_data <- fread(file = "shot_data.csv") %>% 
  mutate(`Starting Lie` = factor(tolower(`Starting Lie`)))

lies <- c(
  "Concrete",
  "Deep",
  "Fairway",
  "Green",
  "Pinestraw",
  "Rough",
  "Sand",
  "Semi",
  "Tee"
)

levels(shot_data$`Starting Lie`) <- lies

colors <- list(
  primary = "#000000",
  secondary = "#1B1A1A",
  tertiary = "#484745",
  highlight = "#282928",
  accent = "#246ED0"
)

round_data <- shot_data %>% 
  filter(Hole == 1, `Finishing Lie` == "holeCup") %>% 
  group_by(Tournament) %>% 
  arrange(Timestamp) %>% 
  mutate(
    Round = row_number(),
    Date = format(as.POSIXct(Timestamp), "%m/%d/%Y")
  ) %>% 
  ungroup() %>% 
  select(Date, Tournament, Round) %>% 
  left_join(round_data, by = c("Tournament", "Round")) %>% 
  arrange(desc(`Registration Data`)) %>% 
  mutate(
    Type = str_remove(Tour, " AutoPutt.*"),
    Tournament = str_remove(Tournament, "\\[.*")
  )

par3_rounds <- round_data %>% 
  filter(Tee == "Par3") %>% 
  slice_head(n = 20) %>% 
  slice_min(Differential, n = 8, with_ties = FALSE) %>% 
  arrange(desc(`Registration Data`))

par3_handicap <- round(mean(par3_rounds$Differential), 1)

combo_rounds <- round_data %>% 
  filter(Tee != "Par3")

combo_rounds <- combo_rounds %>% 
  slice_min(Differential, n = nrow(combo_rounds) %/% 2.3) %>% 
  arrange(desc(`Registration Data`))

combo_handicap <- round(mean(combo_rounds$Differential), 1)

holeouts <- shot_data %>% 
  filter(
    `Starting Lie` != "Green",
    `Finishing Lie` == "holeCup",
    !near(`Carry (yd)`, 0),
    !near(Spin, 0),
    !near(`Roll (yd)`, `Total Distance (yd)`)
  )

n <- nrow(holeouts)
side <- max(1, ceiling(sqrt(n)))

make_grid <- function(n) {
  total <- side^2
  cells <- lapply(seq_len(total), function(i) {
    
    cls <- if (i <= n) {
      "grid-cell grid-filled"
    } else {
      "grid-cell grid-empty"
    }
    
    div(class = cls)
  })
  
  div(
    class = "waffle-grid",
    style = paste0("grid-template-columns: repeat(", side, ", 1fr);"),
    cells
  )
}

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
        
        .grid-title {
          font-size: 3.1rem;
          margin: 1rem;
        }
        
        .level-text {
          font-size: 2.2rem;
          margin: 0;
          padding: 0.5rem;
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
        
        .card-corner-btn,
        .card-corner-btn:focus {
          background: transparent;
          border: none;
          color: white;
          font-size: 2rem;
          padding: 0.5rem;
        }
        
        .card-corner-btn:hover {
          background-color: var(--accent);
        }
        
        .card-row {
          display: flex;
          justify-content: space-between;
          align-items: center;
          width: 100%;
        }
        
        .card-title-row {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
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
        
        .waffle-grid {
          display: grid;
          grid-template-columns: repeat(5, 1fr);
          gap: 4px;
          width: fit-content;
          transform: scaleY(-1);
          padding: 2rem;
        }
        
        .grid-cell {
          width: 2rem;
          height: 2rem;
          border-radius: 4px;
          transform: scaleY(-1);
        }
        
        .grid-filled {
          background-color: var(--accent);
        }
        
        .grid-empty {
          background-color: var(--highlight);
        }
        
        .grid-container {
          display: flex;
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
      ),
      column(
        width = 9,
        div(
          class = "card",
          div(
            class = "card-title-row",
            h2("Round History", class = "yuyu-short-regular card-header"),
            actionButton(
              "expand_history",
              NULL,
              icon = icon("expand"),
              class = "card-corner-btn"
            )
          ),
          uiOutput("recent_rounds")
        )
      )
    ),
    fluidRow(
      column(
        width = 2,
        div(
          class = "card",
          div(
            class = "card-title-row",
            h4(
              sprintf(
                "LONG: %.1f YD",
                max(holeouts$`Starting Distance To Pin (yd)`)
              ),
              class = "yuyu-short-regular level-text"
            ),
            actionButton(
              "expand_stats",
              NULL,
              icon = icon("ellipsis"),
              class = "card-corner-btn"
            )
          ),
          div(
            class = "grid-container",
            make_grid(n),
          ),
          div(
            class = "grid-container",
            h3(
              paste(n, ifelse(n == 1, "HOLEOUT", "HOLEOUTS")),
              class = "yuyu-short-regular grid-title"
            )
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
  
  expand_history <- reactiveVal(FALSE)
  
  observeEvent(input$expand_history, {
    expand_history(!expand_history())
  })
  
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
  
  make_table <- function(df) {
    df <- df %>% 
      mutate(
        across(
          where(is.numeric) & !all_of(c("ROUND", "SCORE")),
          ~ ifelse(is.na(.), "", ifelse(. > 0, paste0("+", .), as.character(.)))
        ),
        across(
          everything(),
          ~ replace(.x, is.na(.x), "")
        )
      )
    
    tags$table(
      class = "rounds-table yuyu-short-regular table-text",
      tags$thead(
        tags$tr(
          lapply(names(df), tags$th)
        )
      ),
      
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            lapply(df[i, ], function(x) {
              tags$td(toupper(as.character(x)))
            })
          )
        })
      )
    )
  }
  
  output$recent_rounds <- renderUI({
    rounds <- round_data %>%
      arrange(desc(Date)) %>% 
      select(
        EVENT = Tournament,
        TOUR = Type,
        DATE = Date,
        ROUND = Round,
        SCORE = `Score (Gross)`,
        GROSS = `To Par (Gross)`,
        NET = `To Par (Net)`
      )
    
    if (!expand_history()) {
      rounds <- rounds %>% 
        slice_head(n = 4)
    }
    
    make_table(rounds)
  })
  
  output$handicap_rounds <- renderUI({
    req(selected_handicap())
    
    rounds <- if (selected_handicap() == "Combo") {
      combo_rounds 
    } else {
      par3_rounds
    }
    
    rounds <- rounds %>% 
      select(
        EVENT = Tournament,
        DATE = Date,
        ROUND = Round,
        SCORE = `Score (Gross)`,
        GROSS = `To Par (Gross)`,
        `HCP DIFF` = Differential
      )
    
    div(
      class = "card expanded-card",
      h2("Rounds Used", class = "yuyu-short-regular card-header"),
      h3(
        paste(toupper(selected_handicap()), " HANDICAP"),
        class = "yuyu-short-regular card-subheader"
      ),
      make_table(rounds)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)