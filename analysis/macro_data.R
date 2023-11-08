library(ecb)
library(tidyverse)
library(lubridate)
library(patchwork)

keys <- tribble(
  ~ type, ~ key,
  "inflation", "ICP.M.U2.N.000000.4.ANR",
  "growth", "MNA.Q.Y.I8.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.GY",
  "interest_rates", "FM.B.U2.EUR.4F.KR.MRR_FR.LEV",
  "interest_rates_bis", "FM.B.U2.EUR.4F.KR.MRR_MBR.LEV",
  "balance_sheet", "ILM.W.U2.C.T000000.Z5.Z01" 
)

keys <- keys %>% 
  mutate(data = map(key, ~get_data(.) %>% select(obstime, obsvalue)))

pull(filter(keys, type == "balance_sheet"), data)

inflation <- filter(keys, type == "inflation") %>%  
  select(type, data) %>% 
  unnest(data) %>%  
  mutate(obstime = ym(obstime))
growth <- filter(keys, type == "growth") %>%  
  select(type, data) %>% 
  unnest(data) %>% 
  mutate(obstime = str_replace(obstime, "Q", "0"),
         obstime = ym(obstime))
rate <- filter(keys, str_detect(type, "rate")) %>% 
  select(type, data) %>% 
  unnest(data) %>% 
  mutate(obstime = ymd(obstime),
         type = "interest_rates") %>% 
  distinct(obstime, .keep_all = TRUE)
balance_sheet <- filter(keys, type == "balance_sheet") %>% 
  select(type, data) %>% 
  unnest(data) %>% 
  mutate(obstime = str_remove(obstime, "W(0)?"))  %>% 
  separate(obstime, c("year", "week"), "-") %>% 
  filter(week != 53) %>% 
  mutate(obstime = parse_date_time(paste(year, week, 1, sep = "/"), "Y/W/w") %>% 
           ymd()) %>% 
  select(type, obstime, obsvalue)

data <- bind_rows(inflation,
                  rate,
                  growth,
                  balance_sheet)

<<<<<<< HEAD

macro_plot <- data %>% 
  filter(type != "balance_sheet") %>% 
  ggplot(aes(obstime, obsvalue, color = type)) + 
  geom_step(data = . %>% filter(type == "interest_rates"), linewidth = 0.9) +
  geom_line(data = . %>% filter(type %in% c("balance_sheet", "inflation", "growth")), linewidth = 0.9) +
#  geom_smooth(data = . %>% filter(type == "growth"), 
 #             se = FALSE, span = 0.2, method = "loess", linewidth = 1) +
  geom_vline(xintercept = date(c("2011-11-08", "2021-09-01"))) + 
  scale_color_grey(labels = c("GDP Yearly Growth",
                              "HICP Inflation",
                              "Main Refinancing Operation\n(Key Interest Rate)"),
                   start = 0, end = 0.7) +
  labs(y = NULL,
       x = NULL,
       color = "Macroeconomic Data") +
  scale_y_continuous(labels = scales::label_percent(scale = 1), n.breaks = 10) +
  scale_x_date(expand = c(0, 0), 
               date_breaks = "2 years", 
               date_labels = "%Y", 
               limits = as_date(c("1999-01-01", "2023-01-31"))) +
  theme_light(base_size = 7) +
  theme(strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.26, 0.22))

balance_sheet_plot <- data %>% 
  filter(type == "balance_sheet") %>% 
  ggplot(aes(obstime, obsvalue/100, color = type)) + 
  geom_step(data = . %>% filter(type == "interest_rates"), linewidth = 0.9) +
  geom_line(data = . %>% filter(type %in% c("balance_sheet", "inflation")), linewidth = 0.9) +
  geom_smooth(data = . %>% filter(type == "growth"), 
              se = FALSE, span = 0.2, method = "loess", linewidth = 0.9) +
  geom_vline(xintercept = date(c("2011-11-08", "2021-09-01"))) + 
  labs(y = "Billions of Euro",
       x = NULL,
       color = "Balance Sheet",
       caption = "Source: ECB Statistical Data Warehouse") +
  scale_color_grey(labels = c("Eurosystem Total Assets"),
                   start = 0, end = 0.7) +
  scale_x_date(expand = c(0, 0), 
               date_breaks = "2 years", 
               date_labels = "%Y", 
               limits = as_date(c("1999-01-01", "2023-01-31"))) +
  theme_light(base_size = 7) +
  theme(strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(0.25, 0.7))

data_plot <- macro_plot/balance_sheet_plot + plot_layout(heights = c(10,6))

picture_path <- "C:/Users/goutsmedt/Dropbox/GreenCBreput/article_pictures"
ggsave(here::here(picture_path, "Figure_1_macro_data_non_smoothed.tiff"), 
       data_plot,
       device = ragg::agg_tiff,
       width = 16, height = 11, units = "cm", dpi = 300)


#' Measuring wages and profits occurrences
#' 

eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds")) %>% 
  arrange(file_name) %>% 
  filter(central_bank %in% c("european central bank", "bundesbank"))
eurosystem_text <- readRDS(here(data_path, "eurosystem_text_cleaned.rds")) %>% 
  filter(file %in% eurosystem_metadata$file_name)

data <- eurosystem_text %>% 
  left_join(select(eurosystem_metadata, file = file_name, date, central_bank)) %>% 
  unnest_tokens(word, paragraphs) %>% 
  select(file, date, central_bank, word) %>% 
  add_count(file, name = "nb_words") %>%
  group_by(file, central_bank) %>% 
  mutate(wages = sum(str_detect(word, "^wage(s)?$"))/nb_words,
         profits = sum(str_detect(word, "^profit(s)?$"))/nb_words) %>% 
  distinct(file, date, central_bank, wages, profits) %>% 
  pivot_longer(c(wages, profits), names_to = "word", values_to = "frequency") %>% 
  filter(central_bank != "bundesbank")


plot_1 <- ggplot(data, aes(date, frequency, color = word)) +
  geom_smooth(method = "loess", span = 0.1) +
  scico::scale_color_scico_d(palette = "roma",
                             labels = c("profit(s)", "wage(s)")) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_cartesian(ylim = c(-0.0001, 0.0014),
                  expand = FALSE) +
  theme_bw(base_size = 22) +
  labs(x = NULL,
       y = "Number of occurrences on total words",
       color = NULL)


ggsave(here("pictures", glue::glue("wage_profit.png")), plot_1,
       device = ragg::agg_jpeg,
       width = 20, height = 15, units = "cm", res = 300)
  

plot_2 <- data %>% 
  filter(date > as_date("2018-01-01")) %>% 
  ggplot(aes(date, frequency, color = word)) +
  geom_smooth(method = "loess", span = 0.2) +
  scico::scale_color_scico_d(palette = "roma") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_cartesian(ylim = c(-0.0001, 0.0013),
                  xlim = c(as_date("2018-01-01"), as_date("2022-09-01")),
                  expand = FALSE) +
  theme_bw(base_size = 22) +
  labs(x = NULL,
       y = "Number of occurrences on total words",
       color = NULL)

ggsave(here("pictures", glue::glue("wage_profit_reduce.png")), plot_2,
       device = ragg::agg_jpeg,
       width = 20, height = 15, units = "cm", res = 600)
=======
macro_plot <- data %>% 
  mutate(type_2 = ifelse(type == "balance_sheet", "ECB Balance Sheet", "Macroeconomic data")) %>% 
  ggplot(aes(obstime, obsvalue, color = type)) + 
  geom_step(data = . %>% filter(type == "interest_rates"), linewidth = 1) +
  geom_line(data = . %>% filter(type %in% c("balance_sheet", "inflation")), linewidth = 1) +
  geom_smooth(data = . %>% filter(type == "growth"), 
              se = FALSE, span = 0.2, method = "loess", linewidth = 1) +
  geom_vline(xintercept = date(c("2011-11-08", "2021-09-01"))) +
  facet_wrap(~ fct_inorder(type_2), ncol = 1, scales = "free_y") + 
  scale_color_discrete(name = NULL,
                       labels = c("Eurosystem Total Assets",
                                  "GDP Yearly Growth\n(Smoothed)",
                                  "HICP Inflation",
                                  "Main Refinancing Operation\n(Key Interest Rate)")) +
  labs(y = NULL,
       x = NULL) +
  xlim(date("1999-01-01"), date("2023-02-01"))

ggsave(here::here("pictures", glue::glue("macro_data.png")), macro_plot,
       device = ragg::agg_png,
       width = 30, height = 20, units = "cm", res = 300)



>>>>>>> 94e4769367dc1858bff47b054eabd826cd2681e7
