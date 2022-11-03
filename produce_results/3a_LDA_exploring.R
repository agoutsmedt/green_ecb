source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
K = 50
lda_data <- readRDS(here(data_path, 
             "topic_modelling",
             paste0("LDA_", K, "_data.rds"))) %>% 
  mutate(date = lubridate::dmy(date))


#' We first explore the data per year. 


topic_per_date <- lda_data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 30))) %>% 
  ggplot(aes(date, gamma, color = topic_name)) +
  geom_smooth(show.legend = FALSE) +
  facet_wrap(~ topic_name) +
  theme(strip.text = element_text (size = 9))

ggsave(here("pictures", glue::glue("TM_LDA_topic_per_date.png")), topic_per_date,
       device = ragg::agg_png,
       width = 50, height = 45, units = "cm", res = 300)

topic_per_date_ecb <- lda_data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 30))) %>%
  filter(central_bank == "european central bank") %>% 
  ggplot(aes(date, gamma, color = topic_name)) +
  geom_smooth(show.legend = FALSE) +
  facet_wrap(~ topic_name) +
  theme(strip.text = element_text (size = 9))

ggsave(here("pictures", glue::glue("TM_LDA_topic_per_date_ecb.png")), topic_per_date_ecb,
       device = ragg::agg_png,
       width = 50, height = 45, units = "cm", res = 300)


#' We create two subsamples of years: 2006-2008, 2021-2022
#' 
data_year_subset <- lda_data %>% 
  mutate(period = case_when(between(date, "2006-01-01", "2008-10-01") ~ "Period_1",
                            between(date, "2021-10-01", "2022-06-26") ~ "Period_2")) %>% 
  filter(! is.na(period)) 

data <- data_year_subset %>% 
  group_by(topic, period) %>% 
  mutate(mean = mean(gamma),
         st_err = sd(gamma)/sqrt(length(gamma))) %>% 
  select(topic, topic_name, period, mean, st_err) %>% 
  unique()

data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 50)),
         period = ifelse(period == "Period_1", "Period 1 (2006-2008)", "Period 2 (2021-2022)")) %>% 
  ggplot(aes(mean, fct_reorder(topic_name, mean), color = period)) +
  geom_errorbar(aes(xmin = mean - st_err, xmax = mean + st_err), show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ period) +
  labs(x = "Prevalence of the topic")

ggsave(here("pictures", glue::glue("TM_mean_LDA_topic_period.png")),
       device = ragg::agg_png,
       width = 40, height = 35, units = "cm", res = 300)

data %>% 
  pivot_wider(names_from = "period", values_from = c("mean", "st_err"),) %>% 
  mutate(differential = mean_Period_2 - mean_Period_1,
         st_err = (st_err_Period_1 + st_err_Period_2)/2,
         topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 50))) %>% 
  ggplot(aes(differential, fct_reorder(topic_name, differential))) +
  geom_errorbar(aes(xmin = differential - st_err, xmax = differential + st_err)) +
  geom_point()

ggsave(here("pictures", glue::glue("TM_differential_LDA_topic_period.png")),
       device = ragg::agg_png,
       width = 40, height = 35, units = "cm", res = 300)

data_ecb <- data_year_subset %>% 
  group_by(topic, period) %>% 
  mutate(mean = mean(gamma),
         st_err = sd(gamma)/sqrt(length(gamma))) %>% 
  select(topic, topic_name, period, mean, st_err) %>% 
  unique()
