optimised_lda <- ldatuning::FindTopicsNumber(
  dtm_filtered,
  topics = seq(from = 20, to = 90, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 3,
  verbose = TRUE
)
saveRDS(optimised_lda, here(data_path, 
                            "topic_modelling",
                            paste0("LDA_optimized.rds")))