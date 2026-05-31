library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(scales)
library(mclogit)
library(marginaleffects)

# mclogit(choice | choice_id ~ (seat + cargo + eng + price_num) * carpool)
# Make sure you run _shrink_model.R to shrink this down
model_wtp_carpool <- readRDS("model.rds")

# From MetBrewer::met.brewer("Tiepolo")
clrs <- c(
  "#802417",
  "#c06636",
  "#ce9344",
  "#e8b960",
  "#646e3b",
  "#2b5851",
  "#508ea2",
  "#17486f"
)
VBOX_COLORS <- clrs[c(1, 3, 5, 7)]

nice_wtp <- label_dollar(scale = 1000, accuracy = 1, style_negative = "minus")

product_card <- function(id, nm, eng, price_val, seat, cargo) {
  card(
    card_header(strong(paste0("Product ", id))),
    card_body(
      class = "p-2",
      textInput(paste0("name", id), "Name", value = nm, width = "100%"),
      selectInput(
        paste0("eng", id), "Engine",
        choices  = c("Gas" = "gas", "Hybrid" = "hyb", "Electric" = "elec"),
        selected = eng, width = "100%"
      ),
      sliderInput(
        paste0("price", id), "Price ($K)",
        min = 25, max = 50, value = price_val, step = 1, width = "100%"
      ),
      selectInput(
        paste0("seat", id), "Seating",
        choices  = c("6 seats" = "6", "7 seats" = "7", "8 seats" = "8"),
        selected = seat, width = "100%"
      ),
      selectInput(
        paste0("cargo", id), "Cargo",
        choices  = c("2 ft" = "2ft", "3 ft" = "3ft"),
        selected = cargo, width = "100%"
      )
    )
  )
}
