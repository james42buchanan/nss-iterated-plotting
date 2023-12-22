# Setup -------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(conflicted); conflicts_prefer(dplyr::filter)
library(janitor)
library(extrafontdb)
library(extrafont)

# Defining functions ------------------------------------------------------
clean_titles <- function(x){
  '''
  Converts strings to title-case and replaces underscores with spaces.
  '''
  x <- gsub("_", " ", x)
  x <- gsub("\\b([a-z])", "\\U\\1", x, perl = TRUE)
  x
}

# Setting chart styling ---------------------------------------------------
theme_set(
  theme_minimal() +
    theme(
      # Text and title
      text = element_text(family = "Roboto Condensed"),
      title = element_text(face = "bold", 
                         size = 20, 
                         colour = "#1F4E79"),
      # Axis text
      axis.text.x = element_text(size = 18, 
                                 colour="#1F4E79"),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      # Legend
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(size = 16, 
                                 colour = "#1F4E79"),
      # Panel
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#D9D9D9", 
                                  fill = NA, 
                                  linewidth = 1),
    )
)
# Group styling
group.colors = c(`Group 1` = "#1F4E79", 
                 `Group 2`= "#8EA9DB")

# Plotting by column ------------------------------------------------------
'''
Iterates through columns of dataset and plots column against year by group.
(e.g. Subject score (held in columns) by year by groups (University of 
Aberdeen and sector)).
'''
lapply(names(df[-c(1:2)]), function(col){
  df %>%
    # Plot variables
    ggplot(aes(year, as.numeric(.data[[col]]), 
           fill = `Group`)) +
    # Bar formatting
    geom_bar(stat = "identity", 
             width = 0.8, 
             position = position_dodge()) +
    # Text formatting
    geom_text(
      aes(x = year, 
          y = as.numeric(.data[[col]]), 
          label = as.numeric(.data[[col]]), 
          group = `Group`,
          colour = `Group`),
      family = "Roboto Condensed",
      vjust = -0.5,
      fontface = "bold", 
      size = 6,
      position = position_dodge(width = 0.8),
      inherit.aes = TRUE) +
    # Colouration
    scale_fill_manual(values = group.colors) +
    scale_colour_manual(values = group.colors) +
    # Scaling
    scale_y_continuous(limits = c(0, 105), 
                       expand = expansion(mult = c(0,0))) +
    scale_x_discrete(limits = c("Year 1", "Year 2")) +
    # Titles
    labs(title = paste('Title: ', clean_titles(col)),
    )
}) -> list_plots

# Exporting plots ---------------------------------------------------------
'''
Iterates through list of saved plots and exports to path.
'''
for (i in 1:length(list_plots)) {
  ggsave(filename = sprintf("plot%d.jpg", i), 
         path = 'Output\\Charts',
         plot = list_plots[[i]],
         scale = 1, 
         width = 250, 
         height = 250, 
         units = "mm",
         dpi = 300, 
         limitsize = TRUE
  )
}