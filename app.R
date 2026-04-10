library(shiny)
library(readxl)
library(dplyr)
library(plotly)
library(DT)
library(writexl)
library(grid)

# ── Utility functions ──────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a)) a else b

clean_shopping_basket <- function(x) {
  out <- gsub("[^0-9]", "", as.character(x))
  out[out == ""] <- NA_character_
  as.character(as.integer(out))
}

safe_drop <- function(data, cols) {
  data[, setdiff(names(data), cols), drop = FALSE]
}

fmt_eur <- function(x) {
  paste0("\u20ac ", formatC(x, format = "f", digits = 2, big.mark = ","))
}

fmt_eur_pdf <- function(x) {
  paste0("EUR ", formatC(x, format = "f", digits = 2, big.mark = ","))
}

build_dt <- function(data, basket_col, price_col = "Price", page_len = 25,
                     dom = "frtip", extra_defs = list(), filter_top = TRUE,
                     escape = TRUE, selection = "none", server = TRUE,
                     colnames = NULL) {
  defs <- c(
    list(list(className = "dt-center", targets = "_all")),
    list(list(targets = basket_col, searchType = "string")),
    extra_defs
  )
  dt <- datatable(
    data,
    rownames  = FALSE,
    escape    = escape,
    selection = selection,
    filter    = if (filter_top) list(position = "top", clear = FALSE, plain = TRUE) else "none",
    options   = list(
      pageLength      = page_len,
      dom             = dom,
      order           = list(),
      orderCellsTop   = FALSE,
      scrollX         = TRUE,
      autoWidth       = FALSE,
      columnDefs      = defs,
      stateSave       = FALSE
    ),
    colnames = colnames
  )
  if (price_col %in% names(data)) {
    dt <- dt %>%
      formatCurrency(price_col, currency = "\u20ac", interval = 3,
                     mark = ",", digits = 2, before = TRUE) %>%
      formatStyle(price_col, fontWeight = "bold", color = "#e63946")
  }
  dt
}

# ── Load & prepare data ────────────────────────────────────────────────────────

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
    Shopping_basket = clean_shopping_basket(Shopping_basket),
    Date            = as.Date(Date),
    Year            = as.integer(format(Date, "%Y")),
    Quarter         = paste0("Q", ceiling(as.integer(format(Date, "%m")) / 3)),
    Ordered_by      = as.factor(Ordered_by),
    Categories      = as.factor(Categories),
    Subcategories   = as.factor(Subcategories),
    Bill_to         = as.factor(Bill_to),
    Price           = as.numeric(Price)
  ) %>%
  filter(Status == "Goedgekeurd")

cat_palette          <- c("#E63946", "#457B9D", "#2A9D8F", "#E9C46A", "#F4A261",
                          "#A8DADC", "#264653", "#6D6875", "#B5838D", "#FFBA08")
all_years            <- sort(unique(df$Year), decreasing = TRUE)
display_cols_to_drop <- c("Pos.", "Status", "Valuta", "Time", "Label")

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', sans-serif; background: #f4f6fb; }

    /* ── Title bar ── */
    .title-bar {
      background: linear-gradient(135deg, #1d3557 0%, #457b9d 100%);
      color: white; padding: 18px 30px; border-radius: 10px;
      margin-bottom: 22px; box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    }
    .title-bar h2 { margin: 0; font-size: 1.6rem; }
    .title-bar p  { margin: 4px 0 0; opacity: .8; font-size: .9rem; }

    /* ── Panels ── */
    .sidebar-panel {
      background: white; border-radius: 10px; padding: 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
    }
    .panel-box {
      background: white; border-radius: 10px; padding: 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08); margin-bottom: 16px;
    }
    .panel-box h4 {
      color: #1d3557; font-weight: 700; margin-top: 0;
      border-bottom: 2px solid #457b9d; padding-bottom: 8px;
    }

    /* ── Typography helpers ── */
    .filter-label { font-weight: 600; color: #1d3557; margin-bottom: 4px; }
    .hint         { color: #888; font-size: .92rem; font-style: italic; }
    .dt-nowrap    { white-space: nowrap !important; }

    /* ── Navigation ── */
    .nav-tabs .nav-link        { color: #1d3557; font-weight: 600; }
    .nav-tabs .nav-link.active { color: #e63946; border-bottom: 2px solid #e63946; }

    /* ── Breadcrumb & back ── */
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

    /* ── Sidebar year blocks ── */
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
    .add-btn:hover   { background: #21867a; }
    .remove-btn      { background: none; border: none; color: #aaa; cursor: pointer;
                       float: right; font-size: 1rem; line-height: 1; }
    .remove-btn:hover { color: #e63946; }

    /* ── Invoice basket ── */
    .basket-item {
      display: flex; justify-content: space-between; align-items: flex-start;
      padding: 8px 10px; border-radius: 6px; margin-bottom: 6px;
      background: #f4f6fb; border: 1px solid #dde3f0; font-size: .88rem;
    }
    .basket-item .bi-name  { color: #1d3557; font-weight: 600; }
    .basket-item .bi-meta  { color: #888; font-size: .8rem; }
    .basket-item .bi-price { color: #e63946; font-weight: 700;
                             white-space: nowrap; margin-left: 8px; }
    .basket-item .bi-remove {
      background: none; border: none; color: #ccc; cursor: pointer;
      font-size: 1rem; padding: 0 0 0 6px; line-height: 1;
    }
    .basket-item .bi-remove:hover { color: #e63946; }
    .basket-total {
      display: flex; justify-content: space-between; align-items: center;
      padding: 10px 0; border-top: 2px solid #e63946; margin-top: 8px;
      font-weight: 700; color: #1d3557;
    }
    .basket-total .bt-amt { color: #e63946; font-size: 1.1rem; }

    /* ── Invoice buttons ── */
    .inv-btn {
      width: 100%; padding: 10px; border: none; border-radius: 6px;
      font-size: .95rem; font-weight: 600; cursor: pointer; margin-top: 10px;
      transition: background .2s;
    }
    .inv-btn-active          { background: #e63946; color: white; }
    .inv-btn-active:hover    { background: #c1121f; }
    .inv-btn-disabled        { background: #dde3f0; color: #aaa; cursor: not-allowed; }
    .clear-basket-btn {
      background: none; border: 1px solid #e63946; color: #e63946;
      border-radius: 6px; padding: 4px 12px; font-size: .8rem;
      cursor: pointer; transition: all .2s; float: right; margin-top: -2px;
    }
    .clear-basket-btn:hover { background: #e63946; color: white; }

    /* ── Browse table add buttons ── */
    .add-line-btn {
      background: #2A9D8F; color: white; border: none;
      padding: 3px 8px; border-radius: 10px; cursor: pointer;
      font-size: .75rem; white-space: nowrap; margin-right: 3px;
    }
    .add-line-btn:hover   { background: #21867a; }
    .add-basket-btn {
      background: #457b9d; color: white; border: none;
      padding: 3px 8px; border-radius: 10px; cursor: pointer;
      font-size: .75rem; white-space: nowrap;
    }
    .add-basket-btn:hover { background: #1d3557; }

    /* ── DataTables: stable column widths ── */
    .dataTables_wrapper table {
      table-layout: fixed;
      width: 100% !important;
    }
    .dataTables_scrollHeadInner table,
    .dataTables_scrollHeadInner {
      width: 100% !important;
    }

    /* ── Invoice number display ── */
    .inv-number-badge {
      display: inline-block; background: #eaf2ff; border: 1px solid #457b9d;
      border-radius: 6px; padding: 4px 12px; font-size: .85rem;
      color: #1d3557; font-weight: 600; margin-bottom: 10px;
    }
  "))),
  
  div(class = "title-bar",
      h2("\U0001f4ca Expenses on 7180"),
      p("Drill down: Category \u2192 Subcategory \u2192 Products")
  ),
  
  tabsetPanel(
    id   = "main_tab",
    type = "tabs",
    
    # ── Dashboard tab ──────────────────────────────────────────────────────────
    tabPanel(
      "\U0001f4ca Dashboard",
      fluidRow(
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
                                             end       = max(df$Date[format(df$Date, "%Y") == format(Sys.Date(), "%Y")],
                                                             na.rm = TRUE),
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
                   div(class = "filter-label", "\U0001f6ab Categories"),
                   checkboxInput("hide_investering", "Hide \"Investering\"", value = FALSE),
                   hr(),
                   uiOutput("summary_box")
               )
        ),
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
                                        div(uiOutput("dl_prod_buttons_ui"))
                                    ),
                                    tags$hr(style = "border-color:#457b9d; margin-top:8px;"),
                                    DTOutput("table_prod")
                                )
               )
        )
      )
    ), # end Dashboard
    
    # ── Data tab ───────────────────────────────────────────────────────────────
    tabPanel(
      "\U0001f5c2 Data",
      br(),
      div(class = "panel-box",
          div(style = "display:flex; justify-content:space-between; align-items:center;",
              h4(style = "margin-bottom:0; border-bottom:none;", "All Orders"),
              div(
                downloadButton("dl_all_csv",   label = "CSV",   style = "margin-right:6px;"),
                downloadButton("dl_all_excel", label = "Excel")
              )
          ),
          tags$hr(style = "border-color:#457b9d; margin-top:8px;"),
          uiOutput("active_filters_ui"),
          p(class = "hint", "Use the search box or column filters to narrow down, then export all matching rows."),
          DTOutput("table_all")
      )
    ), # end Data
    
    # ── Invoice tab ────────────────────────────────────────────────────────────
    tabPanel(
      "\U0001f9fe Invoice",
      br(),
      fluidRow(
        column(8,
               div(class = "panel-box",
                   h4("Browse & Add to Invoice"),
                   div(style = "display:flex; align-items:center; gap:16px; margin-bottom:10px; flex-wrap:wrap;",
                       div(style = "display:flex; align-items:center; gap:8px;",
                           span(class = "filter-label",
                                style = "white-space:nowrap; margin-bottom:0;",
                                "\U0001f4c5 Date range:"),
                           dateRangeInput("inv_date_range", NULL,
                                          start     = min(df$Date, na.rm = TRUE),
                                          end       = max(df$Date, na.rm = TRUE),
                                          min       = min(df$Date, na.rm = TRUE),
                                          max       = max(df$Date, na.rm = TRUE),
                                          format    = "dd-mm-yyyy",
                                          separator = " to ",
                                          width     = "280px"
                           )
                       ),
                       actionLink("inv_date_reset", "\u21ba Reset",
                                  style = "font-size:.85rem; color:#457b9d;")
                   ),
                   p(class = "hint",
                     "Filter or search, then use \u2795 Line to add one product or \u2795 Basket to add all lines from that basket number."),
                   DTOutput("table_inv_browse")
               )
        ),
        column(4,
               div(class = "panel-box",
                   div(style = "display:flex; justify-content:space-between; align-items:center;",
                       h4(style = "margin-bottom:0; border-bottom:none;", "\U0001f9fe Invoice Basket"),
                       uiOutput("clear_basket_btn_ui")
                   ),
                   tags$hr(style = "border-color:#457b9d; margin-top:8px;"),
                   uiOutput("inv_number_display"),
                   uiOutput("inv_basket_ui"),
                   tags$hr(style = "border-color:#dde3f0; margin:12px 0;"),
                   div(class = "filter-label", "Invoice #"),
                   textInput("inv_number", NULL, placeholder = "e.g. INV-20260410-001..."),
                   div(class = "filter-label", "Bill To"),
                   textInput("inv_bill_to", NULL, placeholder = "e.g. Department / Project..."),
                   div(class = "filter-label", "Description"),
                   textInput("inv_description", NULL, placeholder = "e.g. Q1 Supplies..."),
                   uiOutput("inv_create_btn_ui")
               )
        )
      )
    ) # end Invoice
    
  ) # end tabsetPanel
)

# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ── Quarter selector state ───────────────────────────────────────────────────
  yr_count <- reactiveVal(1)
  
  observeEvent(input$add_yr,    { if (yr_count() < 4) yr_count(yr_count() + 1) })
  observeEvent(input$remove_yr, { if (yr_count() > 1) yr_count(yr_count() - 1) })
  
  output$quarter_selectors <- renderUI({
    lapply(seq_len(yr_count()), function(i) {
      div(class = "yr-block",
          if (i > 1)
            tags$button(class = "remove-btn",
                        onclick = "Shiny.setInputValue('remove_yr', Math.random())",
                        "\u00d7"),
          div(class = "filter-label", paste("Selection", i)),
          selectInput(paste0("yr_", i), NULL,
                      choices  = all_years,
                      selected = all_years[min(i, length(all_years))]),
          checkboxGroupInput(paste0("qtr_", i), NULL,
                             choices  = c("Q1","Q2","Q3","Q4"),
                             selected = "Q1",
                             inline   = TRUE)
      )
    })
  })
  
  # ── Filtered data ────────────────────────────────────────────────────────────
  filtered <- reactive({
    d <- if (input$filter_tab == "Date Range") {
      req(input$date_range)
      df %>%
        filter(Date >= input$date_range[1], Date <= input$date_range[2]) %>%
        mutate(Label = "All")
    } else {
      bind_rows(lapply(seq_len(yr_count()), function(i) {
        yr   <- input[[paste0("yr_", i)]]
        qtrs <- input[[paste0("qtr_", i)]]
        req(yr, qtrs)
        df %>%
          filter(Year == as.integer(yr), Quarter %in% qtrs) %>%
          mutate(Label = paste0(yr, " ", Quarter))
      }))
    }
    if (isTRUE(input$hide_investering)) d <- d %>% filter(Categories != "Investering")
    d
  })
  
  # ── Drill-down state ─────────────────────────────────────────────────────────
  sel_cat <- reactiveVal(NULL)
  sel_rek <- reactiveVal(NULL)
  sel_sub <- reactiveVal(NULL)
  
  observeEvent(
    list(input$filter_tab, input$date_range,
         input$hide_investering, input$add_yr, input$remove_yr),
    { sel_cat(NULL); sel_rek(NULL); sel_sub(NULL) }
  )
  
  drill_level <- reactive({
    if      (is.null(sel_cat()))                                   "categories"
    else if (sel_cat() == "Exotisch" && is.null(sel_rek()))        "rekening"
    else if (is.null(sel_sub()))                                   "subcategories"
    else                                                           "products"
  })
  output$drill_level <- renderText(drill_level())
  outputOptions(output, "drill_level", suspendWhenHidden = FALSE)
  
  # ── Back button ──────────────────────────────────────────────────────────────
  output$back_button <- renderUI({
    if (drill_level() != "categories")
      tags$button(class   = "back-btn",
                  onclick = "Shiny.setInputValue('go_back', Math.random())",
                  "\u2190 Back")
  })
  
  observeEvent(input$go_back, {
    lvl <- drill_level()
    if (lvl == "products") {
      sel_sub(NULL)
    } else if (lvl == "subcategories") {
      sel_sub(NULL)
      if (!is.null(sel_cat()) && sel_cat() == "Exotisch") sel_rek(NULL) else sel_cat(NULL)
    } else if (lvl == "rekening") {
      sel_rek(NULL); sel_cat(NULL)
    }
  })
  
  # ── Breadcrumb ───────────────────────────────────────────────────────────────
  output$breadcrumb <- renderUI({
    parts <- "All Categories"
    if (!is.null(sel_cat())) parts <- paste0(parts, " \u203a <b>", sel_cat(), "</b>")
    if (!is.null(sel_rek())) parts <- paste0(parts, " \u203a <b>", sel_rek(), "</b>")
    if (!is.null(sel_sub())) parts <- paste0(parts, " \u203a <b>", sel_sub(), "</b>")
    div(class = "breadcrumb-bar", HTML(parts))
  })
  
  # ── Summary box ──────────────────────────────────────────────────────────────
  output$summary_box <- renderUI({
    d     <- filtered()
    total <- sum(d$Price, na.rm = TRUE)
    tagList(
      div(class = "filter-label", "\U0001f4e6 Summary"),
      tags$table(
        style = "width:100%;font-size:.88rem;",
        tags$tr(
          tags$td("Orders:"),
          tags$td(style = "text-align:right;font-weight:600;",
                  n_distinct(dplyr::na_if(d$Shopping_basket, "NA")))
        ),
        tags$tr(
          tags$td("Products:"),
          tags$td(style = "text-align:right;font-weight:600;",
                  n_distinct(d$Product))
        ),
        tags$tr(
          tags$td("Total (\u20ac):"),
          tags$td(style = "text-align:right;font-weight:600;color:#e63946;",
                  formatC(total, format = "f", digits = 2, big.mark = ","))
        )
      )
    )
  })
  
  # ── Timeframe label ──────────────────────────────────────────────────────────
  timeframe_label <- reactive({
    if (input$filter_tab == "Date Range") {
      req(input$date_range)
      paste0(format(input$date_range[1], "%d-%m-%Y"),
             " to ",
             format(input$date_range[2], "%d-%m-%Y"))
    } else {
      paste(
        sapply(seq_len(yr_count()), function(i)
          paste0(input[[paste0("yr_", i)]], " ",
                 paste(sort(input[[paste0("qtr_", i)]]), collapse = " & "))
        ),
        collapse = " vs "
      )
    }
  })
  
  # ── Plotly bar helper ─────────────────────────────────────────────────────────
  make_bar <- function(data, group_col, source_id) {
    is_compare <- input$filter_tab == "Quarters" && n_distinct(data$Label) > 1
    
    if (!is_compare) {
      agg   <- data %>%
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
        marker        = list(color = fills, line = list(color = "white", width = 1.5)),
        text          = ~paste0("  \u20ac", formatC(Total, format = "f",
                                                    digits = 0, big.mark = ",")),
        textposition  = "outside",
        textfont      = list(size = 12, color = "#1d3557", family = "Segoe UI"),
        hovertemplate = "<b>%{y}</b><br>\u20ac %{x:,.0f}<extra></extra>",
        source        = source_id
      ) %>%
        layout(
          xaxis = list(title = "", tickformat = ",.0f", showgrid = TRUE,
                       gridcolor = "#f0f0f0", gridwidth = 1, zeroline = FALSE,
                       tickfont = list(size = 11, color = "#888"),
                       showticklabels = TRUE),
          yaxis = list(title = "", categoryorder = "array",
                       categoryarray = agg[[group_col]],
                       tickfont = list(size = 12, color = "#1d3557", family = "Segoe UI"),
                       ticklen = 6, tickcolor = "white"),
          margin        = list(l = 8, r = 110, t = 10, b = 30),
          plot_bgcolor  = "white",
          paper_bgcolor = "white",
          font          = list(family = "Segoe UI", size = 12),
          hoverlabel    = list(bgcolor = "#1d3557",
                               font = list(color = "white", size = 12),
                               bordercolor = "#1d3557")
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
                       marker        = list(color = pal[i], line = list(color = "white", width = 1)),
                       text          = ~paste0("  \u20ac", formatC(Total, format = "f",
                                                                   digits = 0, big.mark = ",")),
                       textposition  = "outside",
                       textfont      = list(size = 11, color = "#1d3557", family = "Segoe UI"),
                       hovertemplate = paste0("<b>", lbl, "</b><br>%{y}<br>\u20ac %{x:,.0f}<extra></extra>")
        )
      }
      p %>%
        layout(
          barmode = "group",
          xaxis   = list(title = "", tickformat = ",.0f", showgrid = TRUE,
                         gridcolor = "#f0f0f0", gridwidth = 1, zeroline = FALSE,
                         tickfont = list(size = 11, color = "#888")),
          yaxis   = list(title = "", categoryorder = "array",
                         categoryarray = cat_order,
                         tickfont = list(size = 12, color = "#1d3557", family = "Segoe UI"),
                         ticklen = 6, tickcolor = "white"),
          margin        = list(l = 8, r = 110, t = 10, b = 30),
          plot_bgcolor  = "white",
          paper_bgcolor = "white",
          font          = list(family = "Segoe UI", size = 12),
          legend        = list(orientation = "h", x = 0, y = 1.08,
                               xanchor = "left", yanchor = "bottom"),
          hoverlabel    = list(bgcolor = "#1d3557",
                               font = list(color = "white", size = 12),
                               bordercolor = "#1d3557")
        ) %>%
        config(displayModeBar = FALSE)
    }
  }
  
  # ── Dynamic plot heights ──────────────────────────────────────────────────────
  calc_height <- function(data, group_col) {
    n_cats   <- n_distinct(data[[group_col]])
    n_labels <- if (input$filter_tab == "Quarters") n_distinct(data$Label) else 1
    paste0(max(300, n_cats * (50 * n_labels + 10) + 80), "px")
  }
  
  output$plot_cat_ui <- renderUI(plotlyOutput("plot_cat", height = calc_height(filtered(),  "Categories")))
  output$plot_rek_ui <- renderUI(plotlyOutput("plot_rek", height = calc_height(rek_data(),  "Bill_to")))
  output$plot_sub_ui <- renderUI(plotlyOutput("plot_sub", height = calc_height(sub_data(),  "Subcategories")))
  
  # ── Level 1 – Categories ──────────────────────────────────────────────────────
  output$plot_cat <- renderPlotly(make_bar(filtered(), "Categories", "cat_source"))
  
  observeEvent(event_data("plotly_click", source = "cat_source"), {
    click <- event_data("plotly_click", source = "cat_source")
    req(click)
    sel_cat(click$y); sel_rek(NULL); sel_sub(NULL)
  })
  
  # ── Level 2a – Bill_to (Exotisch only) ───────────────────────────────────────
  rek_data <- reactive({
    req(sel_cat() == "Exotisch")
    filtered() %>% filter(Categories == "Exotisch")
  })
  
  output$cat_title <- renderUI(
    HTML(paste0("Total spend by Category &nbsp;<small style='color:#888;font-weight:400;'>",
                timeframe_label(), "</small>"))
  )
  output$rek_title <- renderUI(
    HTML(paste0("Bill_to within <b>Exotisch</b> &nbsp;<small style='color:#888;font-weight:400;'>",
                timeframe_label(), "</small>"))
  )
  
  output$plot_rek <- renderPlotly(make_bar(rek_data(), "Bill_to", "rek_source"))
  
  observeEvent(event_data("plotly_click", source = "rek_source"), {
    click <- event_data("plotly_click", source = "rek_source")
    req(click)
    sel_rek(click$y); sel_sub(NULL)
  })
  
  # ── Level 2b – Subcategories ──────────────────────────────────────────────────
  sub_data <- reactive({
    req(sel_cat())
    d <- filtered() %>% filter(Categories == sel_cat())
    if (sel_cat() == "Exotisch") { req(sel_rek()); d <- d %>% filter(Bill_to == sel_rek()) }
    d
  })
  
  output$sub_title <- renderUI({
    lbl <- paste0(" &nbsp;<small style='color:#888;font-weight:400;'>",
                  timeframe_label(), "</small>")
    if (!is.null(sel_cat()) && sel_cat() == "Exotisch")
      HTML(paste0("Subcategories in <b>", sel_rek(), "</b>", lbl))
    else
      HTML(paste0("Subcategories in <b>", sel_cat(), "</b>", lbl))
  })
  
  output$plot_sub <- renderPlotly(make_bar(sub_data(), "Subcategories", "sub_source"))
  
  observeEvent(event_data("plotly_click", source = "sub_source"), {
    click <- event_data("plotly_click", source = "sub_source")
    req(click)
    sel_sub(click$y)
  })
  
  # ── Level 3 – Product table ───────────────────────────────────────────────────
  prod_total_price <- reactive({
    req(sel_cat(), sel_sub())
    sub_data() %>%
      filter(Subcategories == sel_sub()) %>%
      summarise(total = sum(Price, na.rm = TRUE), .groups = "drop") %>%
      pull(total)
  })
  
  prod_data_clean <- reactive({
    req(sel_cat(), sel_sub())
    d <- sub_data() %>%
      filter(Subcategories == sel_sub()) %>%
      arrange(desc(Date)) %>%
      mutate(
        Shopping_basket = as.character(Shopping_basket),
        Price           = as.numeric(Price),
        Date            = format(Date, "%d-%m-%Y")
      ) %>%
      safe_drop(display_cols_to_drop)
    
    if (!is.null(sel_cat()) && sel_cat() == "Exotisch" && !is.null(sel_rek())) {
      total_row          <- d[1, , drop = FALSE]
      total_row[1, ]     <- NA
      total_row$Product  <- "TOTAL"
      total_row$Price    <- prod_total_price()
      d <- bind_rows(d, total_row)
    }
    d
  })
  
  output$prod_title <- renderUI({
    req(sel_cat(), sel_sub())
    HTML(paste0(
      "Products in <b>", sel_sub(), "</b>",
      " &nbsp;<span style='color:#e63946;font-weight:700;'>",
      fmt_eur(prod_total_price()), "</span>",
      " &nbsp;<small style='color:#888;font-weight:400;'>",
      timeframe_label(), "</small>"
    ))
  })
  
  output$table_prod <- renderDT({
    req(sel_cat(), sel_sub())
    
    is_bill_to_table <- !is.null(sel_cat()) &&
      sel_cat() == "Exotisch" &&
      !is.null(sel_rek())
    
    extra_defs <- list(
      list(targets = 3, width = "110px", className = "dt-right dt-nowrap")
    )
    
    if (is_bill_to_table) {
      extra_defs <- c(
        list(list(targets = "_all", orderable = FALSE)),
        extra_defs
      )
    }
    
    build_dt(
      prod_data_clean(),
      basket_col = 1,
      price_col = "Price",
      page_len = 100,
      dom = "tip",
      extra_defs = extra_defs,
      selection = "none"
    ) %>%
      formatStyle(
        "Product",
        target = "row",
        fontWeight = styleEqual("TOTAL", "bold"),
        background = styleEqual("TOTAL", "#fff3f3")
      )
  }, server = TRUE)
  
  prod_fname <- reactive({
    rek_part <- if (!is.null(sel_rek())) paste0("_", sel_rek()) else ""
    fname    <- paste0("export_", timeframe_label(), "_",
                       sel_cat(), rek_part, "_", sel_sub())
    gsub("[^A-Za-z0-9_.\\-]", "_", fname)
  })
  
  filtered_prod_data <- reactive({
    rows <- input$table_prod_rows_all
    d    <- prod_data_clean()
    if (is.null(rows) || length(rows) == 0) d else d[rows, , drop = FALSE]
  })
  
  output$dl_prod_buttons_ui <- renderUI({
    is_exotisch <- !is.null(sel_cat()) && sel_cat() == "Exotisch" && !is.null(sel_rek())
    if (is_exotisch) {
      tagList(
        downloadButton("dl_prod_excel", label = "Excel", style = "margin-right:6px;"),
        downloadButton("dl_prod_pdf",   label = "Create Invoice",
                       style = "background:#e63946; color:white; border:none;
                                border-radius:4px; padding:7px 14px;")
      )
    } else {
      tagList(
        downloadButton("dl_prod_csv",   label = "CSV",   style = "margin-right:6px;"),
        downloadButton("dl_prod_excel", label = "Excel")
      )
    }
  })
  
  # ── PDF helper (shared) ───────────────────────────────────────────────────────
  draw_invoice_pdf <- function(file, d_raw, total_fmt, meta_lbs, meta_vls) {
    col_dark  <- "#1d3557"
    col_red   <- "#e63946"
    col_light <- "#eaf2ff"
    col_grey  <- "#f4f6fb"
    
    col_labels <- c("Date", "Basket #", "Basket Name", "Product", "Price")
    col_widths <- c(0.11, 0.11, 0.24, 0.40, 0.14)
    n_cols     <- length(col_labels)
    n_rows     <- nrow(d_raw)
    
    pdf(file, width = 11, height = 8.5, paper = "a4r")
    grid.newpage()
    
    ml <- 0.04; mr <- 0.04; mt <- 0.03; mb <- 0.04
    pushViewport(viewport(x = ml, y = mb,
                          width  = 1 - ml - mr,
                          height = 1 - mt - mb,
                          just   = c("left", "bottom")))
    y_cursor <- 1.0
    
    # Header bar
    hdr_h <- 0.10
    grid.rect(x = 0, y = y_cursor - hdr_h, width = 1, height = hdr_h,
              just = c("left","bottom"), gp = gpar(fill = col_dark, col = NA))
    grid.text("Expenses on 7180",
              x = 0.02, y = y_cursor - hdr_h/2 + 0.02,
              just = c("left","center"),
              gp = gpar(col = "white", fontsize = 16, fontface = "bold"))
    grid.text("Purchase Report",
              x = 0.02, y = y_cursor - hdr_h/2 - 0.02,
              just = c("left","center"),
              gp = gpar(col = "#a8dadc", fontsize = 9))
    grid.text(paste("Generated:", format(Sys.Date(), "%d-%m-%Y")),
              x = 0.98, y = y_cursor - hdr_h/2,
              just = c("right","center"),
              gp = gpar(col = "white", fontsize = 8))
    y_cursor <- y_cursor - hdr_h - 0.02
    
    # Meta cards
    meta_h <- 0.09
    card_w <- 1 / length(meta_lbs)
    grid.rect(x = 0, y = y_cursor - meta_h, width = 1, height = meta_h,
              just = c("left","bottom"),
              gp = gpar(fill = col_grey, col = "#dde3f0", lwd = 0.5))
    for (i in seq_along(meta_lbs)) {
      cx <- (i - 1) * card_w + card_w / 2
      grid.text(meta_lbs[i],
                x = cx, y = y_cursor - 0.025,
                just = c("center","center"),
                gp = gpar(col = "#888888", fontsize = 7))
      grid.text(meta_vls[i],
                x = cx, y = y_cursor - 0.062,
                just = c("center","center"),
                gp = gpar(col = col_dark, fontsize = 10, fontface = "bold"))
      if (i < length(meta_lbs))
        grid.lines(x = c(i * card_w, i * card_w),
                   y = c(y_cursor - meta_h, y_cursor),
                   gp = gpar(col = "#dde3f0", lwd = 0.5))
    }
    y_cursor <- y_cursor - meta_h - 0.025
    
    # Table
    avail_h   <- y_cursor - 0.08
    row_h     <- min(0.045, avail_h / (n_rows + 1.5))
    hdr_row_h <- row_h * 1.1
    col_x     <- cumsum(c(0, col_widths[-n_cols]))
    
    grid.rect(x = 0, y = y_cursor - hdr_row_h, width = 1, height = hdr_row_h,
              just = c("left","bottom"), gp = gpar(fill = col_dark, col = NA))
    for (j in seq_len(n_cols))
      grid.text(col_labels[j],
                x    = col_x[j] + 0.008,
                y    = y_cursor - hdr_row_h / 2,
                just = c("left","center"),
                gp   = gpar(col = "white", fontsize = 7.5, fontface = "bold"))
    y_cursor <- y_cursor - hdr_row_h
    
    for (i in seq_len(n_rows)) {
      bg <- if (i %% 2 == 0) col_light else "white"
      grid.rect(x = 0, y = y_cursor - row_h, width = 1, height = row_h,
                just = c("left","bottom"),
                gp = gpar(fill = bg, col = "#dddddd", lwd = 0.3))
      row_vals <- as.character(unlist(d_raw[i, ]))
      row_vals[is.na(row_vals)] <- ""
      for (j in seq_len(n_cols)) {
        is_price <- j == n_cols
        grid.text(row_vals[j],
                  x    = if (is_price) col_x[j] + col_widths[j] - 0.008
                  else          col_x[j] + 0.008,
                  y    = y_cursor - row_h / 2,
                  just = c(if (is_price) "right" else "left", "center"),
                  gp   = gpar(col       = if (is_price) col_red else col_dark,
                              fontsize  = 7,
                              fontface  = if (is_price) "bold" else "plain"))
      }
      y_cursor <- y_cursor - row_h
    }
    
    # Total box
    grid.lines(x = c(0,1), y = c(y_cursor, y_cursor),
               gp = gpar(col = col_red, lwd = 1.5))
    y_cursor <- y_cursor - 0.015
    tot_h <- 0.055
    grid.rect(x = 0, y = y_cursor - tot_h, width = 1, height = tot_h,
              just = c("left","bottom"),
              gp = gpar(fill = "#fff3f3", col = col_red, lwd = 1.2))
    grid.text("TOTAL",
              x = 0.02, y = y_cursor - tot_h / 2,
              just = c("left","center"),
              gp = gpar(col = col_dark, fontsize = 11, fontface = "bold"))
    grid.text(total_fmt,
              x = 0.98, y = y_cursor - tot_h / 2,
              just = c("right","center"),
              gp = gpar(col = col_red, fontsize = 13, fontface = "bold"))
    
    # Footer
    grid.lines(x = c(0,1), y = c(0.015, 0.015),
               gp = gpar(col = "#dddddd", lwd = 0.5))
    grid.text(paste0(meta_lbs[length(meta_lbs)], ": ",
                     meta_vls[length(meta_vls)], "  |  Expenses on 7180"),
              x = 0.5, y = 0.007,
              just = c("center","bottom"),
              gp = gpar(col = "#aaaaaa", fontsize = 7))
    
    popViewport()
    dev.off()
  }
  
  # ── Dashboard PDF download ────────────────────────────────────────────────────
  output$dl_prod_pdf <- downloadHandler(
    filename = function() paste0(prod_fname(), ".pdf"),
    content  = function(file) {
      req(sel_cat(), sel_sub())
      d_raw <- sub_data() %>%
        filter(Subcategories == sel_sub()) %>%
        arrange(desc(Date)) %>%
        mutate(
          Shopping_basket = as.character(Shopping_basket),
          Price           = fmt_eur_pdf(Price),
          Date            = format(Date, "%d-%m-%Y")
        ) %>%
        select(Date, Shopping_basket, Basket_name, Product, Price)
      draw_invoice_pdf(
        file,
        d_raw,
        total_fmt = fmt_eur_pdf(prod_total_price()),
        meta_lbs  = c("BILL TO", "SUBCATEGORY", "PERIOD"),
        meta_vls  = c(sel_rek(), sel_sub(), timeframe_label())
      )
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
  
  # ── Data tab ──────────────────────────────────────────────────────────────────
  all_data <- reactive({
    df %>%
      arrange(desc(Date)) %>%
      mutate(Shopping_basket = as.character(Shopping_basket),
             Price           = as.numeric(Price)) %>%
      safe_drop(display_cols_to_drop)
  })
  
  output$table_all <- renderDT({
    build_dt(all_data(), basket_col = 1, price_col = "Price",
             page_len = 25, dom = "frtip",
             extra_defs = list(list(targets = 3, width = "110px",
                                    className = "dt-right dt-nowrap")),
             selection = "none",
             colnames = c("Basket #", "Basket Name", "Product", "Price", "Ordered By",
                          "Date", "Category", "Subcategory", "Bill To", "Year", "Quarter")) %>%
      formatDate("Date", method = "toLocaleDateString",
                 params = list("nl-NL",
                               list(day = "2-digit", month = "2-digit", year = "numeric")))
  }, server = TRUE)
  
  output$active_filters_ui <- renderUI({
    col_names   <- c("Shopping_basket","Basket_name","Product","Price","Ordered_by",
                     "Date","Categories","Subcategories","Bill_to","Year","Quarter")
    search_cols <- input$table_all_search_columns
    search_glob <- input$table_all_search
    tags_list   <- list()
    
    pill <- function(label, value)
      tags$span(
        style = "display:inline-block; background:#eaf2ff; border:1px solid #457b9d;
                 border-radius:14px; padding:5px 14px; margin:3px;
                 font-size:1.05rem; color:#1d3557;",
        tags$b(paste0(label, ": ")), value
      )
    
    if (!is.null(search_glob) && nchar(trimws(search_glob)) > 0)
      tags_list <- c(tags_list, list(pill("Search", search_glob)))
    
    if (!is.null(search_cols)) {
      for (i in seq_along(search_cols)) {
        val <- trimws(search_cols[i])
        if (nchar(val) > 0 && i <= length(col_names))
          tags_list <- c(tags_list,
                         list(pill(col_names[i], gsub('^\\["?|"?\\]$', "", val))))
      }
    }
    if (length(tags_list) == 0) return(NULL)
    div(style = "margin-bottom:8px;",
        tags$span(style = "font-size:.9rem; color:#888; margin-right:8px;",
                  "\U0001f50d Active filters:"),
        tags_list)
  })
  
  filtered_all_data <- reactive({
    rows <- input$table_all_rows_all
    d    <- all_data()
    if (is.null(rows) || length(rows) == 0) d else d[rows, , drop = FALSE]
  })
  
  output$dl_all_csv <- downloadHandler(
    filename = function() paste0("export_all_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) write.csv(filtered_all_data(), file, row.names = FALSE)
  )
  output$dl_all_excel <- downloadHandler(
    filename = function() paste0("export_all_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) writexl::write_xlsx(filtered_all_data(), file)
  )
  
  # ── Invoice tab ───────────────────────────────────────────────────────────────
  
  # Auto-generate invoice number once per session, allow editing
  inv_number_default <- paste0("INV-", format(Sys.Date(), "%Y%m%d"), "-001")
  observe({
    updateTextInput(session, "inv_number", value = inv_number_default)
  })
  
  # Basket state: named list, key -> raw df row
  inv_basket <- reactiveVal(list())
  
  # Reset date range to full extent
  observeEvent(input$inv_date_reset, {
    updateDateRangeInput(session, "inv_date_range",
                         start = min(df$Date, na.rm = TRUE),
                         end   = max(df$Date, na.rm = TRUE))
  })
  
  # Browse table data - filtered by selected date range
  inv_browse_data <- reactive({
    req(input$inv_date_range)
    df %>%
      filter(Date >= input$inv_date_range[1],
             Date <= input$inv_date_range[2]) %>%
      arrange(desc(Date)) %>%
      mutate(
        Shopping_basket = as.character(Shopping_basket),
        Price           = as.numeric(Price),
        Date_fmt        = format(Date, "%d-%m-%Y")
      ) %>%
      select(Date_fmt, Shopping_basket, Basket_name, Product,
             Ordered_by, Categories, Subcategories, Price)
  })
  
  output$table_inv_browse <- renderDT({
    d <- inv_browse_data()
    # Two action buttons per row: + Line and + Basket
    d$Actions <- paste0(
      '<button class="add-line-btn" ',
      'onclick="Shiny.setInputValue(\'inv_add_line\', ', seq_len(nrow(d)),
      ', {priority: \'event\'})">&#8853; Line</button>',
      '<button class="add-basket-btn" ',
      'onclick="Shiny.setInputValue(\'inv_add_basket\', \'',
      d$Shopping_basket,
      '\', {priority: \'event\'})">&#8853; Basket</button>'
    )
    build_dt(d, basket_col = 1, price_col = "Price",
             page_len = 15, dom = "frtip",
             extra_defs = list(list(targets = 8, orderable = FALSE,
                                    searchable = FALSE, width = "120px")),
             escape = FALSE, selection = "none",
             colnames = c("Date","Basket #","Basket Name","Product",
                          "Ordered By","Category","Subcategory","Price",""))
  }, server = TRUE)
  
  # Deduplication key: Shopping_basket + Product + Date
  # Same combination from either + Line or + Basket is treated as identical
  inv_key <- function(row) {
    paste0(row$Shopping_basket, "__",
           row$Product,         "__",
           format(row$Date, "%Y%m%d"))
  }
  
  # Add a single product line
  observeEvent(input$inv_add_line, {
    idx    <- input$inv_add_line
    d_full <- df %>% arrange(desc(Date))
    req(idx >= 1, idx <= nrow(d_full))
    row    <- d_full[idx, ]
    key    <- inv_key(row)
    basket <- inv_basket()
    if (!key %in% names(basket)) {
      basket[[key]] <- row
      inv_basket(basket)
    }
  })
  
  # Add all lines belonging to a shopping basket number
  observeEvent(input$inv_add_basket, {
    basket_no <- input$inv_add_basket
    req(nchar(trimws(basket_no)) > 0)
    rows   <- df %>% filter(Shopping_basket == basket_no)
    basket <- inv_basket()
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, ]
      key <- inv_key(row)
      if (!key %in% names(basket)) basket[[key]] <- row
    }
    inv_basket(basket)
  })
  
  # Remove a single item
  observeEvent(input$inv_remove_item, {
    basket <- inv_basket()
    basket[[input$inv_remove_item]] <- NULL
    inv_basket(basket)
  })
  
  # Clear entire basket
  observeEvent(input$inv_clear_basket, {
    inv_basket(list())
  })
  
  # Clear basket button (only shown when basket has items)
  output$clear_basket_btn_ui <- renderUI({
    if (length(inv_basket()) > 0)
      tags$button(class   = "clear-basket-btn",
                  onclick = "Shiny.setInputValue('inv_clear_basket', Math.random())",
                  "\u2715 Clear all")
  })
  
  # Invoice number display in basket panel
  output$inv_number_display <- renderUI({
    num <- trimws(input$inv_number %||% inv_number_default)
    if (nchar(num) > 0)
      div(class = "inv-number-badge", "\U0001f4cb ", num)
  })
  
  # Basket item list UI
  output$inv_basket_ui <- renderUI({
    basket <- inv_basket()
    if (length(basket) == 0)
      return(p(class = "hint",
               style = "text-align:center; padding:20px 0;",
               "\u2205 Your invoice basket is empty"))
    
    total <- sum(sapply(basket, function(r) as.numeric(r$Price)), na.rm = TRUE)
    
    items <- lapply(names(basket), function(key) {
      r <- basket[[key]]
      div(class = "basket-item",
          div(style = "flex:1;",
              div(class = "bi-name", r$Product),
              div(class = "bi-meta",
                  r$Basket_name, " \u00b7 ", format(r$Date, "%d-%m-%Y"))
          ),
          div(class = "bi-price", fmt_eur(as.numeric(r$Price))),
          tags$button(
            class   = "bi-remove",
            onclick = paste0("Shiny.setInputValue('inv_remove_item', '",
                             key, "', {priority: 'event'})"),
            "\u00d7"
          )
      )
    })
    
    tagList(
      items,
      div(class = "basket-total",
          span(paste(length(basket), "item(s)")),
          span(class = "bt-amt", fmt_eur(total)))
    )
  })
  
  # Create Invoice button — enabled when basket non-empty AND Bill To filled
  inv_ready <- reactive({
    length(inv_basket()) > 0 &&
      nchar(trimws(input$inv_bill_to %||% "")) > 0
  })
  
  output$inv_create_btn_ui <- renderUI({
    if (inv_ready()) {
      downloadButton("dl_inv_pdf", label = "Create Invoice",
                     class = "inv-btn inv-btn-active",
                     style = "display:block; width:100%; margin-top:10px;")
    } else {
      tags$button(class    = "inv-btn inv-btn-disabled",
                  disabled = NA,
                  "Create Invoice (add items + fill Bill To)")
    }
  })
  
  # Custom invoice PDF download
  output$dl_inv_pdf <- downloadHandler(
    filename = function() {
      num   <- trimws(input$inv_number %||% inv_number_default)
      bill  <- trimws(input$inv_bill_to %||% "")
      fname <- paste0(num, "_", bill)
      paste0(gsub("[^A-Za-z0-9_.\\-]", "_", fname), ".pdf")
    },
    content = function(file) {
      basket <- inv_basket()
      req(length(basket) > 0)
      
      d_raw <- bind_rows(lapply(basket, function(r) r)) %>%
        arrange(desc(Date)) %>%
        mutate(
          Shopping_basket = as.character(Shopping_basket),
          Price           = fmt_eur_pdf(as.numeric(Price)),
          Date            = format(Date, "%d-%m-%Y")
        ) %>%
        select(Date, Shopping_basket, Basket_name, Product, Price)
      
      total_val <- sum(sapply(basket, function(r) as.numeric(r$Price)), na.rm = TRUE)
      all_dates <- as.Date(sapply(basket, function(r) as.character(r$Date)))
      period    <- paste0(format(min(all_dates), "%d-%m-%Y"),
                          " to ",
                          format(max(all_dates), "%d-%m-%Y"))
      
      inv_num  <- trimws(input$inv_number %||% inv_number_default)
      bill_to  <- trimws(input$inv_bill_to %||% "")
      desc_val <- trimws(input$inv_description %||% "")
      if (nchar(desc_val) == 0) desc_val <- "-"
      
      draw_invoice_pdf(
        file, d_raw,
        total_fmt = fmt_eur_pdf(total_val),
        meta_lbs  = c("INVOICE #", "BILL TO", "DESCRIPTION", "PERIOD"),
        meta_vls  = c(inv_num, bill_to, desc_val, period)
      )
    }
  )
  
} # end server

shinyApp(ui, server)