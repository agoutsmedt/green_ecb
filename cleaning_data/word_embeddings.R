source("packages_and_data.R")

########## Loading BIS data ###################

bis_data_path <- here(path.expand("~"),
                      "data",
                      "scrap_bis")

data_files <- list.files(bis_data_path)
bis_text <- readRDS(here(bis_data_path, 
                         data_files[str_which(data_files, "text")])) %>% 
  mutate(file = str_remove(file, "\\.pdf$"))

#eurosystem_metadata <- readRDS(here(data_path, "eurosystem_metadata.rds"))
#eurosystem_text <- readRDS(here(data_path, "eurosystem_text.rds"))

####################### trying word embedding with the Supervised Machine Learning Textbook ####################

term_list <- eurosystem_text %>% 
  unnest_tokens(word, text, token = "words") %>% 
  as.data.table %>% 
  anti_join(stop_words) %>% 
  mutate(term = lemmatize_words(word),
         document = paste0(file, "_page", page)) %>% 
  add_count(term) %>% 
  filter(n >= 10) %>% 
  select(document, term)

nested_words <- term_list %>% 
  as_tibble %>% 
  nest(terms = c(term))

slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(
    tbl, 
    ~.x, 
    .after = window_size - 1, 
    .step = 1, 
    .complete = TRUE
  )
  
  safe_mutate <- safely(mutate)
  
  out <- map2(skipgrams,
              1:length(skipgrams),
              ~ safe_mutate(.x, window_id = .y))
  
  out %>%
    purrr::transpose() %>%
    pluck("result") %>%
    compact() %>%
    bind_rows()
}

plan(multisession)  ## for parallel processing

tidy_pmi <- nested_words %>%
  mutate(terms = map(terms, slide_windows, 4L)) %>%
  unnest(terms) %>%
  unite(window_id, document, window_id) %>%
  widyr::pairwise_pmi(term, window_id)

tidy_word_vectors <- tidy_pmi %>%
  widyr::widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )

saveRDS(tidy_word_vectors, here(data_path,
             "word_embedding", 
             "word_vectors_tidytext.rds"))

nearest_neighbors <- function(df, token) {
  df %>%
    widyr::widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) / 
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, value) %>%
    select(-item2)
}

tidy_word_vectors %>%
  nearest_neighbors("research")

tidy_word_vectors %>%
  filter(dimension <= 24) %>%
  group_by(dimension) %>%
  top_n(12, abs(value)) %>%
  ungroup() %>%
  mutate(item1 = reorder_within(item1, value, dimension)) %>%
  ggplot(aes(item1, value, fill = dimension)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 4) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Value",
    title = "First 24 principal components for text of CFPB complaints",
    subtitle = paste("Top words contributing to the components that explain",
                     "the most variation")
  )

word_matrix <- term_list %>% 
  count(document, term) %>%
  cast_sparse(document, term, n)

embedding_matrix <- tidy_word_vectors %>%
  cast_sparse(item1, dimension, value)

doc_matrix <- word_matrix %*% embedding_matrix

dim(doc_matrix)

####################### trying word embedding with word2vec ####################
corpus <- term_list %>% 
  mutate(document = str_remove(document, "(?<=_).*")) %>% 
  .[, text := paste0(term, collapse = " "), by = "document"] %>% 
  select(doc_id = document, text) %>% 
  unique
model <- word2vec::word2vec(corpus$text, type = "cbow", window = 10, dim = 100, iter = 100, min_count = 10)
predict(model, "research", type = "nearest", top_n = 20)
word2vec::write.word2vec(model, here(data_path,
                           "word_embedding", 
                           "word_vectors_word2vec.bin"))

embedding <- as.matrix(model)
library(uwot)
viz <- uwot::umap(embedding, n_neighbors = 15, n_threads = 2)
rownames(viz) <- rownames(embedding)
head(viz, n = 10)

library(ggplot2)
df <- data.frame(word = rownames(viz),
                 x = viz[, 1], y = viz[, 2],
                 stringsAsFactors = FALSE)
p <- ggplot(df, aes(x = x, y = y, label = word)) +
  ggrepel::geom_text_repel(size = 1, max.overlaps = 100) + 
  theme_void()
ggsave("test.png", p, device = ragg::agg_png(), width = 50, height = 50, units = "cm")


corpus$text <- word2vec::txt_clean_word2vec(corpus$text, ascii = TRUE, alpha = TRUE, tolower = TRUE, trim = TRUE)
model <- doc2vec::paragraph2vec(x = corpus, type = "PV-DBOW",
                       dim = 10, iter = 10, min_count = 10, lr = 0.05, threads = 2)
predict(model,
        newdata = "keynesian",
        type = "nearest", which = "word2word", top_n = 10)

sentences <- c("climate change", "new keynesian") 
sentences <- setNames(sentences, sentences) 
sentences <- strsplit(sentences, split = " ")
predict(model, newdata = sentences, type = "nearest", which = "sent2doc", top_n = 7)

text <- c("r220317b_")
predict(model,
        newdata = text,
        type = "nearest", which = "doc2doc", top_n = 30)

####################### trying word embedding with golgotha ####################

library(golgotha)
transformer_download_model("distilbert-base-multilingual-cased", architecture = "DistilBERT")
model <- transformer("bert-base-multilingual-uncased")
bert <- hf_load_model("bert-base-multilingual-uncased")
distilBERT <- hf_load_model("distilbert-base-uncased-finetuned-sst-2-english")

model <- golgotha::transformer("bert-base-multilingual-uncased")
x <- data.frame(doc_id = c("doc_1", "doc_2"),
                text = c("give me back my money or i'll call the police.",
                         "talk to the hand because the face don't want to hear it any more."),
                stringsAsFactors = FALSE)
embedding <- predict(model, x, type = "embed-sentence")
embedding <- predict(model, x, type = "embed-token")
tokens    <- predict(model, x, type = "tokenise")


reticulate::use_condaenv("r-reticulate")

library(keras)
texts <- iconv(eurosystem_text$text, to = "UTF-8")
tokenizer <- text_tokenizer(num_words = 20000)
tokenizer %>% fit_text_tokenizer(texts)


skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}



embedding_size <- 50  # Dimension of the embedding vector.
skip_window <- 4       # How many words to consider left and right.
num_sampled <- 1       # Number of negative examples to sample for each word.


input_target <- keras::layer_input(shape = 1)
input_context <- layer_input(shape = 1)

embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1, 
  output_dim = embedding_size, 
  input_length = 1, 
  name = "embedding"
)

target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten()

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()

dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)
output <- layer_dense(dot_product, units = 1, activation = "sigmoid")

model <- keras_model(list(input_target, input_context), output)
model %>% compile(loss = "binary_crossentropy", optimizer = "adam")

model %>%
  fit_generator(
    skipgrams_generator(texts, tokenizer, skip_window, negative_samples), 
    steps_per_epoch = 10000, epochs = 1
  )

embedding_matrix <- get_weights(model)[[1]]
words <- data_frame(
  word = names(tokenizer$word_index), 
  id = as.integer(unlist(tokenizer$word_index))
)

words <- words %>%
  filter(id <= tokenizer$num_words) %>%
  arrange(id)

row.names(embedding_matrix) <- c("UNK", words$word)

find_similar_words <- function(word, embedding_matrix, n = 20) {
  similarities <- embedding_matrix[word, , drop = FALSE] %>%
    text2vec::sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}


find_similar_words("climate", embedding_matrix)
tsne <- Rtsne::Rtsne(embedding_matrix[2:2000,], perplexity = 50, pca = TRUE)
tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(embedding_matrix)[2:2000]) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 1.8)
plotly::ggplotly(tsne_plot)
