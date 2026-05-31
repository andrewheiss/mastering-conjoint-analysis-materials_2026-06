server <- function(input, output, session) {

  # Market data frame ----
  # Simulated market with one row per product, just like in the static version
  market_df <- reactive({
    data.frame(
      product = c(input$name1, input$name2, input$name3, input$name4),
      seat = factor(
        c(input$seat1, input$seat2, input$seat3, input$seat4),
        levels = c("6", "7", "8")
      ),
      cargo = factor(
        c(input$cargo1, input$cargo2, input$cargo3, input$cargo4),
        levels = c("2ft", "3ft")
      ),
      eng = factor(
        c(input$eng1, input$eng2, input$eng3, input$eng4),
        levels = c("gas", "elec", "hyb")
      ),
      price_num = c(input$price1, input$price2, input$price3, input$price4),
      carpool = isTRUE(input$carpool),
      choice_id = 1
    )
  })

  # Predicted market share ----
  # This is also just like the static version
  sim <- reactive({
    mkt   <- market_df()
    preds <- predictions(model_wtp_carpool, newdata = mkt, type = "link")
    utils  <- preds$estimate
    shares <- exp(utils) / sum(exp(utils))
    list(utilities = utils, shares = shares, products = mkt$product)
  })

  # WTP ----
  # WTP = -(β_feature / β_price_num)
  # Once again, just like the static version
  wtp_data <- reactive({
    if (!isTRUE(input$carpool)) {
      hypotheses(
        model_wtp_carpool,
        hypothesis = c(
          "7 seats vs. 6" = "-seat7 / price_num = 0",
          "8 seats vs. 6" = "-seat8 / price_num = 0",
          "3ft cargo vs. 2ft" = "-cargo3ft / price_num = 0",
          "Electric vs. Gas" = "-engelec / price_num = 0",
          "Hybrid vs. Gas" = "-enghyb / price_num = 0"
        )
      )
    } else {
      hypotheses(
        model_wtp_carpool,
        hypothesis = c(
          "7 seats vs. 6" = "-(seat7 + `seat7:carpoolTRUE`) / (price_num + `price_num:carpoolTRUE`) = 0",
          "8 seats vs. 6" = "-(seat8 + `seat8:carpoolTRUE`) / (price_num + `price_num:carpoolTRUE`) = 0",
          "3ft cargo vs. 2ft" = "-(cargo3ft + `cargo3ft:carpoolTRUE`) / (price_num + `price_num:carpoolTRUE`) = 0",
          "Electric vs. Gas" = "-(engelec + `engelec:carpoolTRUE`) / (price_num + `price_num:carpoolTRUE`) = 0",
          "Hybrid vs. Gas" = "-(enghyb + `enghyb:carpoolTRUE`) / (price_num + `price_num:carpoolTRUE`) = 0"
        )
      )
    }
  })

  # Value boxes ----
  for (i in 1:4) {
    local({
      idx <- i
      output[[paste0("vbox", idx)]] <- renderUI({
        res <- sim()
        share <- res$shares[idx]
        util <- res$utilities[idx]
        nm <- res$products[idx]
        rank <- rank(-res$shares, ties.method = "first")[idx]
        col <- VBOX_COLORS[idx]
        value_box(
          title = nm,
          value = label_percent(accuracy = 0.1)(share),
          theme = value_box_theme(
            bg = if (rank == 1) col else "#f0f4f8",
            fg = if (rank == 1) "white" else "#333333"
          ),
          p(paste0(
            "Utility: ",
            label_number(accuracy = 0.1, style_negative = "minus")(util)
          )),
          p(paste0("Rank: #", rank))
        )
      })
    })
  }

  # Market shares plot ----
  output$shares_plot <- renderPlot(
    {
      res <- sim()
      color_map <- setNames(VBOX_COLORS, res$products)
      df <- data.frame(
        product = reorder(res$products, res$shares),
        share = res$shares
      )
      ggplot(df, aes(x = share, y = product, fill = product)) +
        geom_col(width = 0.75) +
        geom_text(
          aes(label = label_percent(accuracy = 0.1)(share)),
          hjust = -0.15,
          size = 9,
          size.unit = "pt",
          fontface = "bold"
        ) +
        scale_x_continuous(
          labels = label_percent(accuracy = 1),
          expand = expansion(mult = c(0, 0.1))
        ) +
        scale_fill_manual(values = color_map, guide = "none") +
        labs(x = "Market share", y = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.y = element_text(face = "bold"),
          panel.grid.major.y = element_blank()
        )
    },
    res = 96
  )

  # Utility plot ----
  output$utility_plot <- renderPlot(
    {
      res <- sim()
      color_map <- setNames(VBOX_COLORS, res$products)
      df <- data.frame(
        product = reorder(res$products, res$utilities),
        utility = res$utilities
      )

      ggplot(df, aes(x = utility, y = product, fill = product)) +
        geom_col(width = 0.75) +
        geom_text(
          aes(label = label_comma(accuracy = 0.1)(utility)),
          hjust = 1.15,
          size = 9,
          size.unit = "pt",
          fontface = "bold"
        ) +
        scale_x_continuous(
          labels = label_comma(accuracy = 0.1),
          expand = expansion(mult = c(0.1, 0))
        ) +
        scale_fill_manual(values = color_map, guide = "none") +
        labs(x = "Total utility (log-odds)", y = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold")
        )
    },
    res = 96
  )

  # WTP table ----
  output$wtp_table <- renderTable(
    {
      df <- as.data.frame(wtp_data())
      df <- df[order(-df$estimate), ]
      data.frame(
        Comparison = df$hypothesis,
        WTP = nice_wtp(df$estimate),
        `95% CI` = paste0("[", nice_wtp(df$estimate), ", ", nice_wtp(df$estimate), "]"),
        check.names = FALSE
      )
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE
  )

  # WTP plot ----
  output$wtp_plot <- renderPlot(
    {
      df <- as.data.frame(wtp_data())
      df$hypothesis <- factor(
        df$hypothesis,
        levels = df$hypothesis[order(df$estimate)]
      )

      ggplot(df, aes(x = estimate, y = hypothesis, color = estimate >= 0)) +
        geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
        scale_color_manual(
          values = c("FALSE" = clrs[1], "TRUE" = clrs[8]),
          guide = "none"
        ) +
        scale_x_continuous(labels = nice_wtp) +
        labs(x = "WTP", y = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold")
        )
    },
    res = 96
  )

  # Download button ----
  output$download_market <- downloadHandler(
    filename = function() paste0("market-", format(Sys.time(), "%Y-%m-%dT%H-%M-%S"), ".csv"),
    content = function(file) write.csv(market_df(), file, row.names = FALSE)
  )
}
