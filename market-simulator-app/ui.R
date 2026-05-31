ui <- page_sidebar(
  # General page settings ----
  title = tags$span(
    bsicons::bs_icon("car-front"),
    HTML("&ensp;"),
    "Minivan market simulator",
    tags$small(
      class = "ms-4",
      style = "font-size: 0.8em; font-style: italic;",
      "Conjoint analysis workshop, Day 3"
    )
  ),
  theme = bs_theme(bootswatch = "cosmo", primary = "#802417") |>
    bs_add_rules(
      "
    .bslib-page-sidebar {
      --bslib-page-sidebar-title-bg: #802417;
      --bslib-page-sidebar-title-color: #ffffff;
    }

    #wtp_section .accordion-button {
      background-color: #e8b96033 !important;
    }
  "
    ),

  # Sidebar stuff ----
  sidebar = sidebar(
    width = "20%",
    card(
      card_header("Respondent details"),
      checkboxInput(
        "carpool",
        "Regular carpool commuter",
        value = FALSE
      )
    ),

    card(
      card_header("Save the market"),
      downloadButton(
        "download_market",
        "Download market CSV",
        class = "w-100 mt-2"
      )
    )
  ),

  # Main content ----

  ## Value boxes in the first row ----
  layout_columns(
    col_widths = c(3, 3, 3, 3),
    uiOutput("vbox1"),
    uiOutput("vbox2"),
    uiOutput("vbox3"),
    uiOutput("vbox4")
  ),

  ## Market shares and utility ----
  layout_columns(
    col_widths = c(8, 4),
    card(
      card_header(bsicons::bs_icon("pie-chart"), " Market shares"),
      plotOutput("shares_plot", height = "270px")
    ),
    card(
      card_header(bsicons::bs_icon("emoji-smile"), "Total utility"),
      plotOutput("utility_plot", height = "270px")
    )
  ),

  ## Willingness to pay ----
  accordion(
    id = "wtp_section",
    open = FALSE,
    accordion_panel(
      "Willingness to pay",
      icon = bsicons::bs_icon("currency-dollar"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("WTP estimates"),
          tableOutput("wtp_table")
        ),
        card(
          card_header("WTP plot"),
          plotOutput("wtp_plot", height = "220px")
        )
      )
    )
  ),

  ## Market settings ----
  card(
    card_header(bsicons::bs_icon("shop-window"), "Products in the market"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      product_card(1, "Eco Basic", "hyb", 30, "6", "2ft"),
      product_card(2, "Family Max", "gas", 35, "8", "3ft"),
      product_card(3, "Green Star", "elec", 40, "7", "3ft"),
      product_card(4, "Value Pack", "gas", 30, "6", "2ft")
    )
  )
)
