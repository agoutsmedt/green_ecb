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



