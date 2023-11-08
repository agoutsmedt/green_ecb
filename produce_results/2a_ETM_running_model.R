source("packages_and_data.R")
#eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds"))
#eurosystem_text <- readRDS(here(data_path, "eurosystem_text.rds"))
term_list <- readRDS(here(data_path, "topic_modelling","TM_term_list_inflation.rds"))
cb_word_embeddings <- readRDS(here(path.expand("~"),
                                   "data",
                                   "whatever_it_takes_embeddings",
                                   "word_embeddings.rds")) %>% 
  pivot_longer(cols = starts_with("V"), names_to = "dimensions", values_to = "values") %>% 
  as.data.table()

cb_word_embeddings <- readRDS(here(path.expand("~"),
                                   "data",
                                   "whatever_it_takes_embeddings",
                                   "word_embeddings.rds")) 
matrix_embeddings <- cb_word_embeddings %>% 
  filter(! is.na(word)) %>% 
  mutate(across(starts_with("V"), ~ as.double(.))) %>% 
  column_to_rownames(var = "word") %>% 
  as.matrix()

embedding_corpus <- "manual_tidytext"
if(embedding_corpus == "manual_tidytext"){
  embedding <- readRDS(here(data_path, "word_embedding", "word_vectors_tidytext_8.rds"))
} else {
  embedding <- word2vec::read.word2vec(here(data_path, "word_embedding", "word_vectors_word2vec.bin")) %>% 
    as.matrix()
}

################################# ETM topic model ###############################

#dtm   <- strsplit.data.frame(term_list, group = "document", term = "term", split = " ")
#dtm   <- document_term_frequencies(dtm)

#doc_id <- data.table(doc_id = 1:length(term_list_filtered$document %>% unique()) %>% as.integer, 
#                    document = term_list_filtered$document %>% unique())
dtm   <- term_list %>%
  filter(ngram != "trigram") %>% 
  .[, freq := .N, by = c("document_id", "term")] %>%  
  select(doc_id = document_id, term, freq) %>% 
  document_term_matrix()
dtm_filtered   <- dtm_remove_tfidf(dtm, prob = 0.70)

embedding_test <- embedding %>% 
  pivot_wider(names_from = "dimension", values_from = "value") %>% 
  column_to_rownames(var = "item1") %>% 
  as.matrix
colnames(matrix_embeddings) <- NULL

vocab <- intersect(rownames(matrix_embeddings), colnames(dtm_filtered))
embeddings <- dtm_conform(matrix_embeddings, rows = vocab)
dtm_filtered <- dtm_conform(dtm_filtered, columns = vocab)

set.seed(1234)
torch_manual_seed(4321)
model     <- topicmodels.etm::ETM(k = 20, dim = 300, embeddings = embeddings)
optimizer <- optim_adam(params = model$parameters, lr = 0.005, weight_decay = 0.0000012)
loss      <- model$fit(data = dtm_filtered, optimizer = optimizer, epoch = 40, batch_size = 1000)
predict(model, type = "terms")

topic.centers <- as.matrix(model, type = "embedding", which = "topics")
word.embeddings <- as.matrix(model, type = "embedding", which = "words")
topic.terminology <- as.matrix(model, type = "beta")

beta_dt <- bind_cols(tibble(term = rownames(topic.terminology)), as_tibble(topic.terminology)) %>% 
  pivot_longer(contains("V"),
               names_to = "topic",
               values_to = "beta") %>% 
  mutate(topic = str_remove(topic, "V") %>% as.integer) %>% 
  arrange(topic, -beta)

test <- beta_dt %>% group_by(topic) %>%  slice_max(order_by = beta, n = 15) %>% View()

