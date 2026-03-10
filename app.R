library(shiny)
library(readxl)
library(dplyr)
library(plotly)
library(DT)
library(writexl)
library(grid)
library(gridExtra)

# ── Load data ──────────────────────────────────────────────────────────────────
df <- read_excel("data/bestellingen 7180 2025_2026_latest.xlsx") %>%
  rename(
    Shopping_basket = Winkelwagennummer,
    Basket_name     = `Naam winkelwagen`,
    Product         = Positienaam,
    Price           = Nettowaarde,
    Ordered_by      = `Gecreeerd door`,
    Categories      = Groep,
    Subcategories   = Wie_Wat,
    Bill_to         = Rekening
  ) %>%
  mutate(
    Date          = as.Date(Date),
    Year          = as.integer(format(Date, "%Y")),
    Quarter       = paste0("Q", ceiling(as.integer(format(Date, "%m")) / 3)),
    Ordered_by    = as.factor(Ordered_by),
    Categories    = as.factor(Categories),
    Subcategories = as.factor(Subcategories),
    Bill_to       = as.factor(Bill_to)
  ) %>%
  filter(Status == "Goedgekeurd")

cat_palette <- c(
  "#E63946", "#457B9D", "#2A9D8F", "#E9C46A", "#F4A261",
  "#A8DADC", "#264653", "#6D6875", "#B5838D", "#FFBA08"
)

all_years <- sort(unique(df$Year), decreasing = TRUE)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', sans-serif; background: #f4f6fb; }
    .title-bar {
      background: linear-gradient(135deg, #1d3557 0%, #457b9d 100%);
      color: white; padding: 18px 30px; border-radius: 10px;
      margin-bottom: 22px; box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    }
    .title-bar h2 { margin: 0; font-size: 1.6rem; }
    .title-bar p  { margin: 4px 0 0; opacity: .8; font-size: .9rem; }
    .sidebar-panel {
      background: white; border-radius: 10px; padding: 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
    }
    .filter-label { font-weight: 600; color: #1d3557; margin-bottom: 4px; }
    .panel-box {
      background: white; border-radius: 10px; padding: 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08); margin-bottom: 16px;
    }
    .panel-box h4 {
      color: #1d3557; font-weight: 700; margin-top: 0;
      border-bottom: 2px solid #e63946; padding-bottom: 8px;
    }
    .breadcrumb-bar {
      background: #eaf2ff; border-left: 4px solid #457b9d;
      padding: 10px 16px; border-radius: 6px; margin-bottom: 14px;
      font-size: .92rem; color: #1d3557;
    }
    .back-btn {
      background: #457b9d; color: white; border: none;
      padding: 7px 18px; border-radius: 20px; cursor: pointer;
      font-size: .85rem; margin-bottom: 10px; transition: background .2s;
    }
    .back-btn:hover { background: #1d3557; }
    .hint { color: #888; font-size: .82rem; font-style: italic; }
    .yr-block {
      background: #f8f9fc; border: 1px solid #dde3f0; border-radius: 8px;
      padding: 12px; margin-bottom: 10px;
    }
    .yr-block .filter-label { font-size: .85rem; }
    .add-btn {
      background: #2A9D8F; color: white; border: none;
      padding: 7px 16px; border-radius: 20px; cursor: pointer;
      font-size: .85rem; width: 100%; margin-top: 6px; transition: background .2s;
    }
    .add-btn:hover { background: #21867a; }
    .remove-btn {
      background: none; border: none; color: #aaa; cursor: pointer;
      float: right; font-size: 1rem; line-height: 1;
    }
    .remove-btn:hover { color: #e63946; }
    .nav-tabs .nav-link { color: #1d3557; font-weight: 600; }
    .nav-tabs .nav-link.active { color: #e63946; border-bottom: 2px solid #e63946; }
  "))),
  
  div(class = "title-bar",
      h2("\U0001f4ca Expenses on 7180"),
      p("Drill down: Category \u2192 Subcategory \u2192 Products")
  ),
  
  tabsetPanel(id = "main_tab", type = "tabs",
              
              tabPanel("📊 Dashboard",
                       fluidRow(
                         # ── Sidebar ──────────────────────────────────────────────────────────────
                         column(3,
                                div(class = "sidebar-panel",
                                    tabsetPanel(
                                      id   = "filter_tab",
                                      type = "tabs",
                                      
                                      tabPanel("Date Range",
                                               br(),
                                               div(class = "filter-label", "\U0001f5d3 Date range"),
                                               dateRangeInput("date_range", NULL,
                                                              start     = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                                                              end       = max(df$Date[format(df$Date, "%Y") == format(Sys.Date(), "%Y")], na.rm = TRUE),
                                                              min       = as.Date(paste0(format(min(df$Date, na.rm = TRUE), "%Y"), "-01-01")),
                                                              max       = as.Date(paste0(format(max(df$Date, na.rm = TRUE), "%Y"), "-12-31")),
                                                              format    = "dd-mm-yyyy",
                                                              separator = " to "
                                               )
                                      ),
                                      
                                      tabPanel("Quarters",
                                               br(),
                                               div(class = "filter-label", "\U0001f4c5 Select year + quarters"),
                                               uiOutput("quarter_selectors"),
                                               tags$button(
                                                 class   = "add-btn",
                                                 onclick = "Shiny.setInputValue('add_yr', Math.random())",
                                                 "+ Add year"
                                               )
                                      )
                                    ),
                                    hr(),
                                    div(class = "filter-label", "🚫 Categories"),
                                    checkboxInput("hide_investering", "Hide \"Investering\"", value = FALSE),
                                    hr(),
                                    uiOutput("summary_box")
                                )
                         ),
                         
                         # ── Main panel ────────────────────────────────────────────────────────────
                         column(9,
                                uiOutput("breadcrumb"),
                                uiOutput("back_button"),
                                
                                conditionalPanel("output.drill_level == 'categories'",
                                                 div(class = "panel-box",
                                                     h4(uiOutput("cat_title")),
                                                     p(class = "hint", "Click a bar to drill down"),
                                                     uiOutput("plot_cat_ui")
                                                 )
                                ),
                                
                                conditionalPanel("output.drill_level == 'rekening'",
                                                 div(class = "panel-box",
                                                     h4(uiOutput("rek_title")),
                                                     p(class = "hint", "Click a bar to drill into subcategories"),
                                                     uiOutput("plot_rek_ui")
                                                 )
                                ),
                                
                                conditionalPanel("output.drill_level == 'subcategories'",
                                                 div(class = "panel-box",
                                                     h4(uiOutput("sub_title")),
                                                     p(class = "hint", "Click a bar to see the product table"),
                                                     uiOutput("plot_sub_ui")
                                                 )
                                ),
                                
                                conditionalPanel("output.drill_level == 'products'",
                                                 div(class = "panel-box",
                                                     div(style = "display:flex; justify-content:space-between; align-items:center;",
                                                         h4(style = "margin-bottom:0; border-bottom:none;", uiOutput("prod_title")),
                                                         div(
                                                           downloadButton("dl_prod_csv",   label = "CSV",   style = "margin-right:6px;"),
                                                           downloadButton("dl_prod_excel", label = "Excel", style = "margin-right:6px;"),
                                                           uiOutput("dl_prod_pdf_ui")
                                                         )
                                                     ),
                                                     tags$hr(style = "border-color:#e63946; margin-top:8px;"),
                                                     DTOutput("table_prod")
                                                 )
                                )
                         )
                       )
              ), # end Dashboard tabPanel
              
              tabPanel("🗂 Data",
                       br(),
                       div(class = "panel-box",
                           div(style = "display:flex; justify-content:space-between; align-items:center;",
                               h4(style = "margin-bottom:0; border-bottom:none;", "All Orders"),
                               div(
                                 downloadButton("dl_all_csv",   label = "CSV",   style = "margin-right:6px;"),
                                 downloadButton("dl_all_excel", label = "Excel")
                               )
                           ),
                           tags$hr(style = "border-color:#e63946; margin-top:8px;"),
                           p(class = "hint", "Use the search box or column filters to narrow down, then export all matching rows."),
                           DTOutput("table_all")
                       )
              )
              
  ) # end main tabsetPanel
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Quarter selector state ─────────────────────────────────────────────────
  yr_count <- reactiveVal(1)
  
  observeEvent(input$add_yr, {
    if (yr_count() < 4) yr_count(yr_count() + 1)
  })
  
  observeEvent(input$remove_yr, {
    if (yr_count() > 1) yr_count(yr_count() - 1)
  })
  
  output$quarter_selectors <- renderUI({
    n <- yr_count()
    lapply(seq_len(n), function(i) {
      div(class = "yr-block",
          if (i > 1) {
            tags$button(
              class   = "remove-btn",
              onclick = "Shiny.setInputValue('remove_yr', Math.random())",
              "\u00d7"
            )
          },
          div(class = "filter-label", paste("Selection", i)),
          selectInput(paste0("yr_", i), NULL,
                      choices  = all_years,
                      selected = all_years[min(i, length(all_years))]
          ),
          checkboxGroupInput(paste0("qtr_", i), NULL,
                             choices  = c("Q1", "Q2", "Q3", "Q4"),
                             selected = "Q1",
                             inline   = TRUE
          )
      )
    })
  })
  
  # ── Filtered data ──────────────────────────────────────────────────────────
  filtered <- reactive({
    d <- if (input$filter_tab == "Date Range") {
      req(input$date_range)
      df %>%
        filter(Date >= input$date_range[1], Date <= input$date_range[2]) %>%
        mutate(Label = "All")
    } else {
      n     <- yr_count()
      parts <- lapply(seq_len(n), function(i) {
        yr   <- input[[paste0("yr_", i)]]
        qtrs <- input[[paste0("qtr_", i)]]
        req(yr, qtrs)
        df %>%
          filter(Year == as.integer(yr), Quarter %in% qtrs) %>%
          mutate(Label = paste0(yr, " ", Quarter))
      })
      bind_rows(parts)
    }
    if (isTRUE(input$hide_investering)) {
      d <- d %>% filter(Categories != "Investering")
    }
    d
  })
  
  # ── Drill state ────────────────────────────────────────────────────────────
  sel_cat <- reactiveVal(NULL)
  sel_rek <- reactiveVal(NULL)
  sel_sub <- reactiveVal(NULL)
  
  observeEvent(list(input$filter_tab, input$date_range), {
    sel_cat(NULL)
    sel_rek(NULL)
    sel_sub(NULL)
  })
  
  drill_level <- reactive({
    if (is.null(sel_cat())) {
      "categories"
    } else if (sel_cat() == "Exotisch" && is.null(sel_rek())) {
      "rekening"
    } else if (is.null(sel_sub())) {
      "subcategories"
    } else {
      "products"
    }
  })
  output$drill_level <- renderText(drill_level())
  outputOptions(output, "drill_level", suspendWhenHidden = FALSE)
  
  # ── Back button ────────────────────────────────────────────────────────────
  output$back_button <- renderUI({
    if (drill_level() != "categories") {
      tags$button(
        class   = "back-btn",
        onclick = "Shiny.setInputValue('go_back', Math.random())",
        "\u2190 Back"
      )
    }
  })
  
  observeEvent(input$go_back, {
    lvl <- drill_level()
    if (lvl == "products") {
      sel_sub(NULL)
    } else if (lvl == "subcategories") {
      sel_sub(NULL)
      if (!is.null(sel_cat()) && sel_cat() == "Exotisch") {
        sel_rek(NULL)
      } else {
        sel_cat(NULL)
      }
    } else if (lvl == "rekening") {
      sel_rek(NULL)
      sel_cat(NULL)
    }
  })
  
  # ── Breadcrumb ─────────────────────────────────────────────────────────────
  output$breadcrumb <- renderUI({
    parts <- "All Categories"
    if (!is.null(sel_cat())) parts <- paste0(parts, " \u203a <b>", sel_cat(), "</b>")
    if (!is.null(sel_rek())) parts <- paste0(parts, " \u203a <b>", sel_rek(), "</b>")
    if (!is.null(sel_sub())) parts <- paste0(parts, " \u203a <b>", sel_sub(), "</b>")
    div(class = "breadcrumb-bar", HTML(parts))
  })
  
  # ── Summary ────────────────────────────────────────────────────────────────
  output$summary_box <- renderUI({
    d     <- filtered()
    total <- sum(d$Price, na.rm = TRUE)
    tagList(
      div(class = "filter-label", "\U0001f4e6 Summary"),
      tags$table(
        style = "width:100%;font-size:.88rem;",
        tags$tr(
          tags$td("Orders:"),
          tags$td(
            style = "text-align:right;font-weight:600;",
            n_distinct(d$Shopping_basket)
          )
        ),
        tags$tr(
          tags$td("Products:"),
          tags$td(
            style = "text-align:right;font-weight:600;",
            n_distinct(d$Product)
          )
        ),
        tags$tr(
          tags$td("Total (\u20ac):"),
          tags$td(
            style = "text-align:right;font-weight:600;color:#e63946;",
            formatC(total, format = "f", digits = 2, big.mark = ",")
          )
        )
      )
    )
  })
  
  # ── Timeframe label ───────────────────────────────────────────────────────
  timeframe_label <- reactive({
    if (input$filter_tab == "Date Range") {
      req(input$date_range)
      paste0(format(input$date_range[1], "%d-%m-%Y"), " to ", format(input$date_range[2], "%d-%m-%Y"))
    } else {
      n            <- yr_count()
      period_parts <- sapply(seq_len(n), function(i) {
        yr   <- input[[paste0("yr_", i)]]
        qtrs <- paste(sort(input[[paste0("qtr_", i)]]), collapse = " & ")
        paste0(yr, " ", qtrs)
      })
      paste(period_parts, collapse = " vs ")
    }
  })
  
  # ── Helper: plotly horizontal bar ─────────────────────────────────────────
  make_bar <- function(data, group_col, source_id) {
    is_compare <- input$filter_tab == "Quarters" && n_distinct(data$Label) > 1
    
    if (!is_compare) {
      agg <- data %>%
        group_by(.data[[group_col]]) %>%
        summarise(Total = sum(Price, na.rm = TRUE), .groups = "drop") %>%
        arrange(Total)
      
      fills <- rep(cat_palette, length.out = nrow(agg))
      
      plot_ly(
        data          = agg,
        x             = ~Total,
        y             = as.formula(paste0("~`", group_col, "`")),
        type          = "bar",
        orientation   = "h",
        marker        = list(color = fills),
        text          = ~paste0("\u20ac", formatC(Total, format = "f", digits = 0, big.mark = ",")),
        textposition  = "outside",
        hovertemplate = "%{y}<br><b>\u20ac%{x:,.0f}</b><extra></extra>",
        source        = source_id
      ) %>%
        layout(
          xaxis = list(
            title      = "Total (\u20ac)",
            tickformat = ",.0f",
            showgrid   = TRUE,
            gridcolor  = "#eee"
          ),
          yaxis = list(
            title         = "",
            categoryorder = "array",
            categoryarray = agg[[group_col]]
          ),
          margin        = list(l = 10, r = 90, t = 10, b = 40),
          plot_bgcolor  = "white",
          paper_bgcolor = "white",
          font          = list(family = "Segoe UI", size = 13),
          hoverlabel    = list(bgcolor = "#1d3557", font = list(color = "white"))
        ) %>%
        config(displayModeBar = FALSE)
      
    } else {
      agg <- data %>%
        group_by(.data[[group_col]], Label) %>%
        summarise(Total = sum(Price, na.rm = TRUE), .groups = "drop")
      
      cat_order <- agg %>%
        group_by(.data[[group_col]]) %>%
        summarise(Grand = sum(Total), .groups = "drop") %>%
        arrange(Grand) %>%
        pull(.data[[group_col]])
      
      labels <- sort(unique(agg$Label))
      pal    <- rep(cat_palette, length.out = length(labels))
      
      p <- plot_ly(source = source_id)
      for (i in seq_along(labels)) {
        lbl   <- labels[i]
        d_lbl <- agg %>% filter(Label == lbl)
        p <- add_trace(p,
                       data          = d_lbl,
                       x             = ~Total,
                       y             = as.formula(paste0("~`", group_col, "`")),
                       type          = "bar",
                       orientation   = "h",
                       name          = lbl,
                       marker        = list(color = pal[i]),
                       text          = ~paste0("\u20ac", formatC(Total, format = "f", digits = 0, big.mark = ",")),
                       textposition  = "outside",
                       hovertemplate = paste0(lbl, " | %{y}<br><b>\u20ac%{x:,.0f}</b><extra></extra>")
        )
      }
      
      p %>%
        layout(
          barmode = "group",
          xaxis   = list(
            title      = "Total (\u20ac)",
            tickformat = ",.0f",
            showgrid   = TRUE,
            gridcolor  = "#eee"
          ),
          yaxis = list(
            title         = "",
            categoryorder = "array",
            categoryarray = cat_order
          ),
          margin        = list(l = 10, r = 90, t = 10, b = 40),
          plot_bgcolor  = "white",
          paper_bgcolor = "white",
          font          = list(family = "Segoe UI", size = 13),
          legend        = list(orientation = "h", x = 0, y = 1.08, xanchor = "left", yanchor = "bottom"),
          hoverlabel    = list(bgcolor = "#1d3557", font = list(color = "white"))
        ) %>%
        config(displayModeBar = FALSE)
    }
  }
  
  # ── Dynamic plot heights ───────────────────────────────────────────────────
  calc_height <- function(data, group_col) {
    n_cats   <- n_distinct(data[[group_col]])
    n_labels <- if (input$filter_tab == "Quarters") n_distinct(data$Label) else 1
    px       <- max(300, n_cats * (50 * n_labels + 10) + 80)
    paste0(px, "px")
  }
  
  output$plot_cat_ui <- renderUI({
    plotlyOutput("plot_cat", height = calc_height(filtered(), "Categories"))
  })
  
  output$plot_rek_ui <- renderUI({
    plotlyOutput("plot_rek", height = calc_height(rek_data(), "Bill_to"))
  })
  
  output$plot_sub_ui <- renderUI({
    plotlyOutput("plot_sub", height = calc_height(sub_data(), "Subcategories"))
  })
  
  # ── Level 1 – Categories ───────────────────────────────────────────────────
  output$plot_cat <- renderPlotly({
    make_bar(filtered(), "Categories", "cat_source")
  })
  
  observeEvent(event_data("plotly_click", source = "cat_source"), {
    click <- event_data("plotly_click", source = "cat_source")
    req(click)
    sel_cat(click$y)
    sel_rek(NULL)
    sel_sub(NULL)
  })
  
  # ── Level 2a – Bill_to (Exotisch only) ───────────────────────────────────
  rek_data <- reactive({
    req(sel_cat() == "Exotisch")
    filtered() %>% filter(Categories == "Exotisch")
  })
  
  output$cat_title <- renderUI({
    HTML(paste0("Total spend by Category &nbsp;<small style='color:#888;font-weight:400;'>", timeframe_label(), "</small>"))
  })
  
  output$rek_title <- renderUI({
    HTML(paste0("Bill_to within <b>Exotisch</b> &nbsp;<small style='color:#888;font-weight:400;'>", timeframe_label(), "</small>"))
  })
  
  output$plot_rek <- renderPlotly({
    make_bar(rek_data(), "Bill_to", "rek_source")
  })
  
  observeEvent(event_data("plotly_click", source = "rek_source"), {
    click <- event_data("plotly_click", source = "rek_source")
    req(click)
    sel_rek(click$y)
    sel_sub(NULL)
  })
  
  # ── Level 2b – Subcategories ───────────────────────────────────────────────
  sub_data <- reactive({
    req(sel_cat())
    d <- filtered() %>% filter(Categories == sel_cat())
    if (sel_cat() == "Exotisch") {
      req(sel_rek())
      d <- d %>% filter(Bill_to == sel_rek())
    }
    d
  })
  
  output$sub_title <- renderUI({
    lbl <- paste0(" &nbsp;<small style='color:#888;font-weight:400;'>", timeframe_label(), "</small>")
    if (!is.null(sel_cat()) && sel_cat() == "Exotisch") {
      HTML(paste0("Subcategories in <b>", sel_rek(), "</b>", lbl))
    } else {
      HTML(paste0("Subcategories in <b>", sel_cat(), "</b>", lbl))
    }
  })
  
  output$plot_sub <- renderPlotly({
    make_bar(sub_data(), "Subcategories", "sub_source")
  })
  
  observeEvent(event_data("plotly_click", source = "sub_source"), {
    click <- event_data("plotly_click", source = "sub_source")
    req(click)
    sel_sub(click$y)
  })
  
  # ── Data tab – full table ─────────────────────────────────────────────────
  # Reactive: cleaned full dataset for Data tab
  all_data <- reactive({
    df %>%
      arrange(desc(Date)) %>%
      mutate(
        Date        = format(Date, "%d-%m-%Y"),
        Price = paste0("€ ", formatC(Price, format = "f", digits = 2))
      ) %>%
      select(-`Pos.`, -Status, -Valuta, -Time)
  })
  
  output$table_all <- renderDT({
    datatable(all_data(),
              rownames   = FALSE,
              filter     = "top",
              options    = list(
                pageLength = 25,
                dom        = "frtip",
                order      = list(),
                scrollX    = TRUE,
                columnDefs = list(list(className = "dt-center", targets = "_all")),
                stateSave  = FALSE
              )
    ) %>%
      formatStyle("Price", fontWeight = "bold", color = "#e63946")
  }, server = TRUE)
  
  # Download handlers — export only filtered rows
  fname_all <- function() paste0("export_all_", format(Sys.Date(), "%Y%m%d"))
  
  filtered_all_data <- reactive({
    rows <- input$table_all_rows_all
    d    <- all_data()
    if (is.null(rows) || length(rows) == 0) d else d[rows, , drop = FALSE]
  })
  
  output$dl_all_csv <- downloadHandler(
    filename = function() paste0(fname_all(), ".csv"),
    content  = function(file) write.csv(filtered_all_data(), file, row.names = FALSE)
  )
  
  output$dl_all_excel <- downloadHandler(
    filename = function() paste0(fname_all(), ".xlsx"),
    content  = function(file) writexl::write_xlsx(filtered_all_data(), file)
  )
  
  # ── Level 3 – Product table ────────────────────────────────────────────────
  output$prod_title <- renderUI({
    req(sel_cat(), sel_sub())
    total_fmt <- paste0("€ ", formatC(prod_total_price(), format = "f", digits = 2, big.mark = ","))
    HTML(paste0(
      "Products in <b>", sel_sub(), "</b>",
      " &nbsp;<span style='color:#e63946;font-weight:700;'>", total_fmt, "</span>",
      " &nbsp;<small style='color:#888;font-weight:400;'>", timeframe_label(), "</small>"
    ))
  })
  
  output$table_prod <- renderDT({
    req(sel_cat(), sel_sub())
    
    datatable(prod_data_clean(),
              rownames = FALSE,
              options  = list(
                pageLength = 100,
                dom        = "tip",
                order      = list(),
                scrollX    = TRUE,
                columnDefs = list(list(className = "dt-center", targets = "_all"))
              )
    ) %>%
      formatStyle("Price", fontWeight = "bold", color = "#e63946") %>%
      formatStyle("Product",
                  target     = "row",
                  fontWeight = styleEqual("TOTAL", "bold"),
                  background = styleEqual("TOTAL", "#fff3f3")
      )
  }, server = TRUE)
  
  # Reactive: raw total price (before formatting)
  prod_total_price <- reactive({
    req(sel_cat(), sel_sub())
    sub_data() %>%
      filter(Subcategories == sel_sub()) %>%
      summarise(total = sum(Price, na.rm = TRUE)) %>%
      pull(total)
  })
  
  # Reactive: cleaned product data for export
  prod_data_clean <- reactive({
    req(sel_cat(), sel_sub())
    d <- sub_data() %>%
      filter(Subcategories == sel_sub()) %>%
      arrange(desc(Date)) %>%
      mutate(
        Price = paste0("€ ", formatC(Price, format = "f", digits = 2, big.mark = ",")),
        Date  = format(Date, "%d-%m-%Y")
      ) %>%
      select(-Label, -`Pos.`, -Status, -Valuta, -Time)
    
    # Only append TOTAL row for Exotisch > Bill_to > Subcategory path
    if (!is.null(sel_cat()) && sel_cat() == "Exotisch" && !is.null(sel_rek())) {
      total_row <- d[1, ]
      total_row[1, ] <- NA
      total_row$Product <- "TOTAL"
      total_row$Price   <- paste0("€ ", formatC(prod_total_price(), format = "f", digits = 2, big.mark = ","))
      d <- bind_rows(d, total_row)
    }
    d
  })
  
  prod_fname <- reactive({
    rek_part <- if (!is.null(sel_rek())) paste0("_", sel_rek()) else ""
    fname <- if (input$filter_tab == "Date Range") {
      paste0(
        "export_",
        format(input$date_range[1], "%Y%m%d"),
        "_to_",
        format(input$date_range[2], "%Y%m%d"),
        "_", sel_cat(), rek_part, "_", sel_sub()
      )
    } else {
      n            <- yr_count()
      period_parts <- sapply(seq_len(n), function(i) {
        yr   <- input[[paste0("yr_", i)]]
        qtrs <- paste(sort(input[[paste0("qtr_", i)]]), collapse = "")
        paste0(yr, qtrs)
      })
      paste0(
        "export_",
        paste(period_parts, collapse = "_vs_"),
        "_", sel_cat(), rek_part, "_", sel_sub()
      )
    }
    gsub("[^A-Za-z0-9_.\\-]", "_", fname)
  })
  
  filtered_prod_data <- reactive({
    rows <- input$table_prod_rows_all
    d    <- prod_data_clean()
    if (is.null(rows) || length(rows) == 0) d else d[rows, , drop = FALSE]
  })
  
  # Only show PDF button for Exotisch > Bill_to path
  output$dl_prod_pdf_ui <- renderUI({
    if (!is.null(sel_cat()) && sel_cat() == "Exotisch" && !is.null(sel_rek())) {
      downloadButton("dl_prod_pdf", label = "PDF",
                     style = "background:#e63946; color:white; border:none; margin-left:0;")
    }
  })
  
  output$dl_prod_pdf <- downloadHandler(
    filename = function() paste0(prod_fname(), ".pdf"),
    content  = function(file) {
      req(sel_cat(), sel_sub())
      
      d_raw <- sub_data() %>%
        filter(Subcategories == sel_sub()) %>%
        arrange(desc(Date)) %>%
        mutate(
          Price = paste0("EUR ", formatC(Price, format = "f", digits = 2, big.mark = ",")),
          Date  = format(Date, "%d-%m-%Y")
        ) %>%
        select(Date, Shopping_basket, Basket_name, Product, Price)
      
      total_fmt <- paste0("EUR ", formatC(prod_total_price(), format = "f", digits = 2, big.mark = ","))
      
      # ── Colours ─────────────────────────────────────────────────────────────
      col_dark  <- "#1d3557"
      col_red   <- "#e63946"
      col_light <- "#eaf2ff"
      col_grey  <- "#f4f6fb"
      
      # ── Column layout ────────────────────────────────────────────────────────
      col_labels <- c("Date", "Basket #", "Basket Name", "Product", "Price")
      col_widths <- c(0.11, 0.11, 0.24, 0.40, 0.14)  # fractions of content width
      n_cols     <- length(col_labels)
      n_rows     <- nrow(d_raw)
      
      # ── Page setup ───────────────────────────────────────────────────────────
      pdf(file, width = 11, height = 8.5, paper = "a4r")  # landscape A4
      grid.newpage()
      
      # margins in npc
      ml <- 0.04; mr <- 0.04; mt <- 0.03; mb <- 0.04
      cw <- 1 - ml - mr   # content width
      ch <- 1 - mt - mb   # content height
      
      vp_main <- viewport(x = ml, y = mb, width = cw, height = ch,
                          just = c("left","bottom"))
      pushViewport(vp_main)
      
      y_cursor <- 1.0  # top of content area
      
      # ── Header bar ──────────────────────────────────────────────────────────
      hdr_h <- 0.10
      grid.rect(x = 0, y = y_cursor - hdr_h, width = 1, height = hdr_h,
                just = c("left","bottom"), gp = gpar(fill = col_dark, col = NA))
      grid.text("Expenses on 7180", x = 0.02, y = y_cursor - hdr_h/2 + 0.02,
                just = c("left","center"), gp = gpar(col = "white", fontsize = 16, fontface = "bold"))
      grid.text("Purchase Report", x = 0.02, y = y_cursor - hdr_h/2 - 0.02,
                just = c("left","center"), gp = gpar(col = "#a8dadc", fontsize = 9))
      grid.text(paste("Generated:", format(Sys.Date(), "%d-%m-%Y")),
                x = 0.98, y = y_cursor - hdr_h/2,
                just = c("right","center"), gp = gpar(col = "white", fontsize = 8))
      y_cursor <- y_cursor - hdr_h - 0.02
      
      # ── Meta cards ──────────────────────────────────────────────────────────
      meta_h   <- 0.09
      meta_lbs <- c("BILL TO", "SUBCATEGORY", "PERIOD")
      meta_vls <- c(sel_rek(), sel_sub(), timeframe_label())
      card_w   <- 1 / length(meta_lbs)
      grid.rect(x = 0, y = y_cursor - meta_h, width = 1, height = meta_h,
                just = c("left","bottom"), gp = gpar(fill = col_grey, col = "#dde3f0", lwd = 0.5))
      for (i in seq_along(meta_lbs)) {
        cx <- (i - 1) * card_w + card_w / 2
        grid.text(meta_lbs[i], x = cx, y = y_cursor - 0.025,
                  just = c("center","center"), gp = gpar(col = "#888888", fontsize = 7, fontface = "plain"))
        grid.text(meta_vls[i], x = cx, y = y_cursor - 0.062,
                  just = c("center","center"), gp = gpar(col = col_dark,  fontsize = 10, fontface = "bold"))
        if (i < length(meta_lbs))
          grid.lines(x = c((i * card_w), (i * card_w)), y = c(y_cursor - meta_h, y_cursor),
                     gp = gpar(col = "#dde3f0", lwd = 0.5))
      }
      y_cursor <- y_cursor - meta_h - 0.025
      
      # ── Table: available height ──────────────────────────────────────────────
      avail_h  <- y_cursor - 0.08   # leave room for total box + footer
      row_h    <- min(0.045, avail_h / (n_rows + 1.5))
      hdr_row_h <- row_h * 1.1
      
      # column x positions
      col_x <- cumsum(c(0, col_widths[-n_cols]))
      
      # Header row
      grid.rect(x = 0, y = y_cursor - hdr_row_h, width = 1, height = hdr_row_h,
                just = c("left","bottom"), gp = gpar(fill = col_dark, col = NA))
      for (j in seq_len(n_cols)) {
        grid.text(col_labels[j],
                  x = col_x[j] + 0.008,
                  y = y_cursor - hdr_row_h / 2,
                  just = c("left","center"),
                  gp = gpar(col = "white", fontsize = 7.5, fontface = "bold"))
      }
      y_cursor <- y_cursor - hdr_row_h
      
      # Data rows
      for (i in seq_len(n_rows)) {
        bg <- if (i %% 2 == 0) col_light else "white"
        grid.rect(x = 0, y = y_cursor - row_h, width = 1, height = row_h,
                  just = c("left","bottom"), gp = gpar(fill = bg, col = "#dddddd", lwd = 0.3))
        row_vals <- as.character(unlist(d_raw[i, ]))
        row_vals[is.na(row_vals)] <- ""
        for (j in seq_len(n_cols)) {
          is_price <- j == n_cols
          grid.text(row_vals[j],
                    x = if (is_price) col_x[j] + col_widths[j] - 0.008 else col_x[j] + 0.008,
                    y = y_cursor - row_h / 2,
                    just = c(if (is_price) "right" else "left", "center"),
                    gp = gpar(col = if (is_price) col_red else col_dark,
                              fontsize = 7, fontface = if (is_price) "bold" else "plain"))
        }
        y_cursor <- y_cursor - row_h
      }
      
      # Separator line above total
      grid.lines(x = c(0,1), y = c(y_cursor, y_cursor),
                 gp = gpar(col = col_red, lwd = 1.5))
      y_cursor <- y_cursor - 0.015
      
      # ── Total box ────────────────────────────────────────────────────────────
      tot_h <- 0.055
      grid.rect(x = 0, y = y_cursor - tot_h, width = 1, height = tot_h,
                just = c("left","bottom"), gp = gpar(fill = "#fff3f3", col = col_red, lwd = 1.2))
      grid.text("TOTAL", x = 0.02, y = y_cursor - tot_h / 2,
                just = c("left","center"), gp = gpar(col = col_dark, fontsize = 11, fontface = "bold"))
      grid.text(total_fmt, x = 0.98, y = y_cursor - tot_h / 2,
                just = c("right","center"), gp = gpar(col = col_red, fontsize = 13, fontface = "bold"))
      y_cursor <- y_cursor - tot_h - 0.015
      
      # ── Footer ───────────────────────────────────────────────────────────────
      grid.lines(x = c(0,1), y = c(0.015, 0.015), gp = gpar(col = "#dddddd", lwd = 0.5))
      grid.text(paste0("Period: ", timeframe_label(), "  |  Expenses on 7180"),
                x = 0.5, y = 0.007, just = c("center","bottom"),
                gp = gpar(col = "#aaaaaa", fontsize = 7))
      
      popViewport()
      dev.off()
    }
  )
  
  output$dl_prod_csv <- downloadHandler(
    filename = function() paste0(prod_fname(), ".csv"),
    content  = function(file) write.csv(filtered_prod_data(), file, row.names = FALSE)
  )
  
  output$dl_prod_excel <- downloadHandler(
    filename = function() paste0(prod_fname(), ".xlsx"),
    content  = function(file) writexl::write_xlsx(filtered_prod_data(), file)
  )
}

shinyApp(ui, server)