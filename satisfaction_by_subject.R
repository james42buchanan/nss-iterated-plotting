# Setup -------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(conflicted); conflicts_prefer(dplyr::filter)
library(janitor)
library(extrafontdb)
library(extrafont)

# Importing data ----------------------------------------------------------
df_nss2022 <- read_excel('Data\\NSS22_External_T_CAH3_AllSubjects.xlsx',
  sheet = 'NSS2022_CAH3_T_All_Subjects')

df_nss2021 <- read_excel('Data\\NSS21_External_T_CAH3_AllSubjects.xlsx',
  sheet = 'NSS21_External_T_CAH3_All')

df_nss2020 <- read_excel('Data\\NSS20_External_T_CAH3_AllSubjects.xlsx',
  sheet = 'NSS20_External_T_CAH3_All')

df_nss2019 <- read_excel('Data\\NSS19_External_T_CAH3_All.xlsx',
  sheet = 'NSS19_External_T_CAH3_All')

# Selecting columns and filtering rows ------------------------------------
df_nss2022 <- df_nss2022 %>% 
  select(c(Year, Institution, Subject, `\nMeasure`, `Overall satisfaction`)) %>% 
  filter(grepl(('Sector|Aberdeen'), Institution)) %>% 
  filter(grepl(('% Agree'), `\nMeasure`))

df_nss2021 <- df_nss2021 %>% 
  select(c(Year, Institution, Subject, `\nMeasure`, `Overall satisfaction`)) %>% 
  filter(grepl(('Sector|Aberdeen'), Institution)) %>% 
  filter(grepl(('% Agree'), `\nMeasure`))

df_nss2020 <- df_nss2020 %>% 
  select(c(Year, Institution, Subject, `\nMeasure`, `Overall satisfaction`)) %>% 
  filter(grepl(('Sector|Aberdeen'), Institution)) %>% 
  filter(grepl(('% Agree'), `\nMeasure`))

df_nss2019 <- df_nss2019 %>% 
  select(c(Year, Institution, Subject, `\nMeasure`, `Overall satisfaction`)) %>% 
  filter(grepl(('Sector|Aberdeen'), Institution)) %>% 
  filter(grepl(('% Agree'), `\nMeasure`))

# Calculating subject means -----------------------------------------------
df_nss2022 <- df_nss2022 %>% 
  group_by(Subject, Institution, Year) %>% 
  summarise_at(vars(`Overall satisfaction`), list(name = mean))

df_nss2021 <- df_nss2021 %>% 
  group_by(Subject, Institution, Year) %>% 
  summarise_at(vars(`Overall satisfaction`), list(name = mean))

df_nss2020 <- df_nss2020 %>% 
  group_by(Subject, Institution, Year) %>% 
  summarise_at(vars(`Overall satisfaction`), list(name = mean))

df_nss2019 <- df_nss2019 %>% 
  group_by(Subject, Institution, Year) %>% 
  summarise_at(vars(`Overall satisfaction`), list(name = mean))

# Pivoting dataframes -----------------------------------------------------
df_nss2022 <- df_nss2022 %>% 
  pivot_wider(names_from = Subject,
              values_from = name)

df_nss2021 <- df_nss2021 %>% 
  pivot_wider(names_from = Subject,
              values_from = name)

df_nss2020 <- df_nss2020 %>% 
  pivot_wider(names_from = Subject,
              values_from = name)

df_nss2019 <- df_nss2019 %>% 
  pivot_wider(names_from = Subject,
              values_from = name)

# Combining and tidying dataframes ----------------------------------------
df_nss_comb <- rbind(df_nss2022,
                     df_nss2021,
                     df_nss2020,
                     df_nss2019) %>% 
  arrange(Institution)

# Replace NA values with 0
df_nss_comb <- df_nss_comb %>% 
  mutate(across(everything(), ~replace_na(.x, 0)))

# Round to 1 decimal place
df_nss_comb <- df_nss_comb %>% 
  mutate(across(where(is.numeric), round, 1), na.rm = FALSE)

# Renaming all column names to lowercase
names(df_nss_comb) <- tolower(names(df_nss_comb))

# Rename values
df_nss_comb <- df_nss_comb %>%
  mutate(institution = recode(
    institution, 
    'University of Aberdeen (10007783)' = 'University of Aberdeen',
    'Sector-wide' = 'Sector'))

# Remove columns where UoA has no current score ---------------------------
df_nss_sector <- df_nss_comb %>% 
  filter(institution == 'Sector')

df_nss_uoa <- df_nss_comb %>% 
  filter(institution == 'University of Aberdeen')

# Select columns where 2022 value is NOT 0
df_nss_uoa <- df_nss_uoa %>% 
  select_if(df_nss_uoa[1,] != 0)

# Full join dataframes
df_nss_comb2 <- merge(df_nss_uoa, df_nss_sector, all = TRUE)

# Remove columns where ANY NA is present
df_nss_comb2 <- df_nss_comb2[ , colSums(is.na(df_nss_comb2)) == 0]

# Setting global theme ----------------------------------------------------
theme_set(
  theme_minimal() +
    theme(
      text = element_text(family = "Roboto Condensed"),
      title = element_text(face = "bold", size = 20, colour = "#1F4E79"),
      axis.text.x = element_text(size = 18, colour = "#1F4E79"),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "top",
      legend.justification = 'left',
      legend.title = element_blank(),
      legend.text = element_text(size = 16, colour = "#1F4E79"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#D9D9D9", fill = NA, linewidth = 1)
      )
  )

# Set colours of institutions
group.colors = c('University of Aberdeen' = "#1F4E79", 'Sector'= "#8EA9DB")

# 'Clean titles' function -------------------------------------------------
clean_titles <- function(x){
  x <- gsub("_", " ", x)
  x <- gsub("\\b([a-z])", "\\U\\1", x, perl = TRUE)
  x
}

# Plotting charts ---------------------------------------------------------
lapply(names(df_nss_comb2[-c(1:2)]), function(col){
  df_nss_comb2 %>%
    ggplot(aes(year, as.numeric(.data[[col]]), fill = institution)) +
    
    geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
    
    geom_text(
      aes(x = year, 
          y = as.numeric(.data[[col]]), 
          label = as.numeric(.data[[col]]), 
          group = institution,
          colour = institution),
          family = "Roboto Condensed",
          vjust = -0.5,
          fontface = "bold", 
          size = 6,
          position = position_dodge(width = 0.8),
          inherit.aes = TRUE) +
    
    scale_fill_manual(values = group.colors) +
    
    scale_colour_manual(values = group.colors) +
    
    scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0,0))) +
    
    scale_x_discrete(limits = c("2019", "2020", "2021", "2022")) +
    
    labs(title = paste('Average positive score: ', clean_titles(col)))
}) -> list_plots

# Exporting plots ---------------------------------------------------------
for (i in 1:length(list_plots)) {
  ggsave(filename = sprintf("plot_%d.jpg", i), 
         path = 'Output\\Charts',
         plot = list_plots[[i]],
         scale = 1, 
         width = 250, 
         height = 250, 
         units = "mm",
         dpi = 300, 
         limitsize = TRUE)
}