# Legacy code

```{r}
#| label: ui

page_navbar(
  title="Icarus Calculators",
  nav_panel(title="Electricity Calculator", p("First page content")),
  nav_panel(title="Combat Calcultor", p("Coming soon")),
  nav_spacer(),
  nav_menu(title="Links",
           align="right",
           nav_item(link_github))
)
```


```{r}
#| context: server

tab <- "   "

# Parse user text inputs -> numeric (with basic clamping)
vals <- reactive({
  list(
    n_freezer = input$n_freezer,
    n_fridge = input$n_fridge,
    daylight = input$daylight
  )
})

# Run the calculations to get the values
req_reactive <- reactive({
  v <- vals()
  calc_requirements(v$n_freezer, v$n_fridge, v$daylight)
})

# Plot variations in output, holding constant number of freezers
output$vary_freezer <- renderPlot({
  v <- vals()
  
  freezer.range <- c(0, 1, 2, 3)
  
  if(v$n_freezer >= 1) {
    freezer.range <- c(-1, freezer.range)
  }
  if(v$n_freezer >= 2) {
    freezer.range <- c(-2, freezer.range)
  }
  if(v$n_freezer >= 3) {
    freezer.range <- c(-3, freezer.range)
  }
  
  plot.data <- data.frame(
    freezers = numeric(),
    solar.panels = numeric(),
    batteries = numeric())
  
  for(freezer.offset in freezer.range) {
    r <- calc_requirements(v$n_freezer + freezer.offset,
                           v$n_fridge,
                           v$daylight)
    
    plot.data <- rbind(plot.data, 
                       data.frame(freezers = v$n_freezer + freezer.offset,
                                  solar.panels = r$panels_needed,
                                  batteries = r$batteries_needed))
  }
  
  plot.data.labels <- plot.data %>%
    group_by(solar.panels, batteries) %>%
    summarise(freezers = paste(freezers, collapse = ", "), .groups = "drop")
  
  x.lims <- range(plot.data$solar.panels) + c(-1, 1)
  y.lims <- range(plot.data$batteries) + c(-1, 1)
  
  label <- paste0("Number of solar panels and batteries requied\n",
                  "   - holding fixed refrigerators at: ",
                  v$n_fridge,
                  "\n",
                  "   - holding fixed daylight hours at: ",
                  v$daylight)
  
  ggplot(plot.data, aes(x=solar.panels, y=batteries)) + 
    geom_point(aes(x=solar.panels, y=batteries)) +
    geom_text_repel(data = plot.data.labels, 
                    aes(label = paste0(freezers, " freezers")),
                    color = "red",
                    size = 5) + 
    scale_x_continuous(limits = x.lims, 
                       breaks = seq(x.lims[1], x.lims[2], 1)) +
    scale_y_continuous(limits = y.lims, 
                       breaks = seq(y.lims[1], y.lims[2], 1)) + 
    labs(x="Number of solar panels required",
         y="Number of batteries required",
         subtitle = label) +
    theme(panel.border = element_blank())  
})

# Plot variations in output, holding constant number of refrigerators
output$vary_fridge <- renderPlot({
  
  v <- vals()
  
  fridge.range <- c(0, 1, 2, 3)
  
  if(v$n_fridge >= 1) {
    fridge.range <- c(-1, fridge.range)
  }
  if(v$n_fridge >= 2) {
    fridge.range <- c(-2, fridge.range)
  }
  if(v$n_fridge >= 3) {
    fridge.range <- c(-3, fridge.range)
  }
  
  plot.data <- data.frame(
    fridges = numeric(),
    solar.panels = numeric(),
    batteries = numeric())
  
  for(fridge.offset in fridge.range) {
    r <- calc_requirements(v$n_freezer,
                           v$n_fridge + fridge.offset,
                           v$daylight)
    
    plot.data <- rbind(plot.data, 
                       data.frame(fridges = v$n_fridge + fridge.offset,
                                  solar.panels = r$panels_needed,
                                  batteries = r$batteries_needed))
  }
  
  plot.data.labels <- plot.data %>%
    group_by(solar.panels, batteries) %>%
    summarise(fridges = paste(fridges, collapse = ", "), .groups = "drop")
  
  x.lims <- range(plot.data$solar.panels) + c(-1, 1)
  y.lims <- range(plot.data$batteries) + c(-1, 1)
  
  label <- paste0("Number of solar panels and batteries requied\n",
                  "   - holding fixed freezers at: ",
                  v$n_freezer,
                  "\n",
                  "   - holding fixed daylight hours at: ",
                  v$daylight)
  
  ggplot(plot.data, aes(x=solar.panels, y=batteries)) + 
    geom_point(aes(x=solar.panels, y=batteries)) +
    geom_text_repel(data = plot.data.labels, 
                    aes(label = paste0(fridges, " refrigerators")),
                    color = "red",
                    size = 5) + 
    scale_x_continuous(limits = x.lims, 
                       breaks = seq(x.lims[1], x.lims[2], 1)) +
    scale_y_continuous(limits = y.lims, 
                       breaks = seq(y.lims[1], y.lims[2], 1)) + 
    labs(x="Number of solar panels required",
         y="Number of batteries required",
         subtitle = label) +
    theme(panel.border = element_blank())
})

# Plot variations in output, holding constant number of freezers
output$vary_daylight <- renderPlot({
  v <- vals()
  
  daylight.range <- c(0)
  
  if(v$daylight >= 1) {
    daylight.range <- c(-1, daylight.range)
  }
  if(v$daylight >= 2) {
    daylight.range <- c(-2, daylight.range)
  }
  if(v$daylight >= 3) {
    daylight.range <- c(-3, daylight.range)
  }
  if(v$daylight <= 23) {
    daylight.range <- c(daylight.range, 1)
  }
  if(v$daylight <= 22) {
    daylight.range <- c(daylight.range, 2)
  }
  if(v$daylight <= 21) {
    daylight.range <- c(daylight.range, 3)
  }
  
  plot.data <- data.frame(
    daylight = numeric(),
    solar.panels = numeric(),
    batteries = numeric())
  
  for(daylight.offset in daylight.range) {
    r <- calc_requirements(v$n_freezer,
                           v$n_fridge,
                           v$daylight + daylight.offset)
    
    plot.data <- rbind(plot.data, 
                       data.frame(daylight = v$daylight + daylight.offset,
                                  solar.panels = r$panels_needed,
                                  batteries = r$batteries_needed))
  }
  
  
  plot.data.labels <- plot.data %>%
    group_by(solar.panels, batteries) %>%
    summarise(daylight = paste(daylight, collapse = ", "), .groups = "drop")
  
  x.lims <- range(plot.data$solar.panels) + c(-1, 1)
  y.lims <- range(plot.data$batteries) + c(-1, 1)
  
  label <- paste0("Number of solar panels and batteries requied\n",
                  "   - holding fixed freezers at: ",
                  v$n_freezer,
                  "\n",
                  "   - holding fixed refrigerators at: ",
                  v$n_fridge)
  
  ggplot(plot.data, aes(x=solar.panels, y=batteries)) + 
    geom_point(aes(x=solar.panels, y=batteries)) +
    geom_text_repel(data = plot.data.labels, 
                    aes(label = paste0(daylight, " daylight hours")),
                    color = "red",
                    size = 5) + 
    scale_x_continuous(limits = x.lims, 
                       breaks = seq(x.lims[1], x.lims[2], 1)) +
    scale_y_continuous(limits = y.lims, 
                       breaks = seq(y.lims[1], y.lims[2], 1)) + 
    labs(x="Number of solar panels required",
         y="Number of batteries required",
         subtitle = label) +
    theme(panel.border = element_blank())  
})


# Display the calculation steps
output$calc_steps <- renderText({
  
  v <- vals()
  r <- req_reactive()
  
  paste0(
    "Assumptions:\n",
    tab, "Freezers:         ", FREEZER_P, " J/s draw\n", 
    tab, "Refrigerators:    ", FRIDGE_P, " J/s draw\n",
    tab, "Solar panels:     ", PANEL_P, " J/s generation\n",
    tab, "Battery:          ", BAT_CAP, " J capacity\n",
    tab, "Battery:          ", BAT_RATE, " J/s discharge\n",
    tab, "24h cycle length: ", IG_24H_MIN, " real minutes",
    "\n",
    "Given inputs:\n",
    tab, "Freezers:         ", v$n_freezer, "\n",
    tab, "Refrigerators:    ", v$n_fridge, "\n",
    tab, "Daylight:         ", v$daylight, " in-game hours (",
    round(input$daylight * SEC_PER_IG_HR, 0),  " real seconds)\n",
    "\n",
    "1) Load P (J/s):\n",
    tab, "P = ", FREEZER_P, " * freezers + ", FRIDGE_P, " * fridges\n",
    tab, r$P, " J/s", " = ", round(FREEZER_P, 0) * v$n_freezer, " + ", round(FRIDGE_P * v$n_fridge, 0), "\n",
    "\n",
    "2) Time mapping:\n",
    tab, "Real seconds per in-game hour = day_cycle_length (", IG_24H_MIN, " min) * 60 seconds / 24 hours ≈ ", round(SEC_PER_IG_HR, 0), "s\n",
    tab, "Real seconds per in-game day  = daylight_hours * ", round(SEC_PER_IG_HR, 0), " = ", round(r$day_s, 0), " s\n",
    tab, "Real seconds per in-game night = nightime_hours = (24 - daylight_hours) * ", round(SEC_PER_IG_HR, 0), " = ", round(r$night_s, 0), " s\n",
    "\n",
    "3) Solar panels needed:\n",
    tab, "Daytime power need: ceiling(P / 6000) = ", r$panels_instant, "\n",
    tab, "Energy balance over 24h:\n",
    tab, tab, "Need: num_panels * panel_power * day_s ≥ P * (day_s + night_s) = P * 24 * SEC_PER_IG_HR\n",
    tab, tab, "Rearranged: P * 24 / (panel_power * day_s) = ", r$panels_energy, "\n",
    tab, "Panels needed = max(daytime_use, full_day_use) = ", r$panels_needed, "\n",
    "\n",
    "4) Batteries (advanced racks):\n",
    tab, "Night energy = P * night_s = ", format(round(r$night_energy), big.mark=","), " J\n",
    tab, "Capacity: ceiling of(night_energy / 6,000,000) = ", r$bat_capacity_needed, "\n",
    tab, "Discharge: ceiling of(P / 10,000) = ", r$bat_rate_needed, "\n",
    tab, "Batteries needed = max(capacity, discharge) = ", r$batteries_needed, "\n")
})
```
```{r}
#| context: setup
#| label: setup

library(shiny)
library(bslib)
library(tidyverse)
library(readr)
library(ggrepel)

options("scipen"=10) 
theme_set(theme_minimal())

# ---------------- Constants (ICARUS) ----------------
FREEZER_P  <- 2500     # J/s per freezer
FRIDGE_P   <- 500      # J/s per refrigerator
PANEL_P    <- 6000     # J/s per solar panel
BAT_CAP    <- 6000000  # J per advanced battery rack
BAT_RATE   <- 10000    # J/s per advanced battery rack

# Time mapping: 24 in-game hours = 70 real minutes
IG_24H_MIN <- 70
IG_24H_SEC <- 70 * 60
SEC_PER_IG_HR <- IG_24H_SEC / 24

calc_requirements <- function(n_freezer, n_fridge, daylight_hours) {
  # clamp daylight to (0,24) to avoid divide-by-zero
  dh <- max(0.01, min(23.99, daylight_hours))
  
  # total continuous load
  P <- n_freezer * FREEZER_P + n_fridge * FRIDGE_P
  
  # durations (real seconds)
  day_s   <- dh * SEC_PER_IG_HR
  night_s <- (24 - dh) * SEC_PER_IG_HR
  
  # Solar: must satisfy instantaneous AND energy balance over 24h
  panels_instant <- ceiling(P / PANEL_P)
  panels_energy  <- ceiling(P * 24 / (PANEL_P * dh))
  panels_needed  <- max(panels_instant, panels_energy)
  
  # Batteries: capacity for night + discharge rate at night load
  night_energy       <- P * night_s
  bat_capacity_need  <- ceiling(night_energy / BAT_CAP)
  bat_rate_need      <- ceiling(P / BAT_RATE)
  batteries_needed   <- max(bat_capacity_need, bat_rate_need)
  
  list(
    P = P,
    day_s = day_s,
    night_s = night_s,
    night_energy = night_energy,
    panels_instant = panels_instant,
    panels_energy = panels_energy,
    panels_needed = panels_needed,
    bat_capacity_needed = bat_capacity_need,
    bat_rate_needed = bat_rate_need,
    batteries_needed = batteries_needed,
    daylight = dh
  )
}

link_github <- tags$a(
  shiny::icon("github"), "Github Page",
  href = "https://github.com/andrewmacd/icaruscalculator",
  target = "_blank"
)

```