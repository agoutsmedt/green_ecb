source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
library(rollinglda)
library(GGally)

list_rolling_model <- readRDS(here(data_path,"rollingLDA", "roll_full.rds"))
rolling_model <- pluck(list_rolling_model, "30")

K <- getK(getLDA(rolling_model))
eta <- getEta(getLDA(rolling_model))
docs <- getDocs(rolling_model)
vocab <- getVocab(rolling_model)
assignments <- getAssignments(getLDA(rolling_model))
topics_chunks <- lapply(rolling_model$chunks$chunk.id, function(x){
  limits = rolling_model$chunks[chunk.id == x]
  tmp = table(factor(unlist(assignments[rolling_model$dates >= limits$start.date & rolling_model$dates <= limits$end.date])+1, levels = 1:K), 
              factor(unlist(lapply(docs[rolling_model$dates >= limits$start.date & rolling_model$dates <= limits$end.date], function(y) y[1,]))+1, levels = seq_len(length(vocab))))
  tmp = matrix(as.integer(tmp), nrow = K)
  colnames(tmp) = vocab
  tmp
})

phi_chunks <- lapply(topics_chunks, function(topics)
  (topics + eta)/(rowSums(topics) + ncol(topics) * eta))

topwords <- apply(topWords(getTopics(getLDA(rolling_model)), 4), 2, paste, collapse = "; ")
topwords_chunks <- lapply(topics_chunks, topWords, numWords = 50)
topwords_chunks <- lapply(1:K, function(k) sapply(seq_along(topwords_chunks), function(t) topwords_chunks[[t]][,k]))
dir.create(here(data_path,"rollingLDA", "topWordsPerChunk"))
for(i in 1:K){
  out <- topwords_chunks[[i]]
  colnames(out) <- as.character(rolling_model$chunks$end.date)
  write.csv(out,
            file = here(data_path,"rollingLDA", "topWordsPerChunk", paste0(i, "_", topwords[i], ".csv")),
            fileEncoding = "UTF-8")
}

topics <- lapply(1:K, function(k){
  tmp <- sapply(topics_chunks, function(x) x[k,])
  colnames(tmp) = paste0("Chunk", rolling_model$chunks$chunk.id)
  tmp
})

cosine <- function(a, b) sum(a*b) / sqrt(sum(a^2)) / sqrt(sum(b^2))
nchunks <- nrow(rolling_model$chunks)

z = 4
rel = 0.85

#for(rel in c(0.8, 0.85)){
q = rel
sim1 = quantiles1 = matrix(NA_real_, ncol = K, nrow = nchunks)
run_length1 = integer(K)
for(i in seq_len(nchunks)[-1]){
  run_length1 = run_length1 + 1
  z1 = pmin(run_length1, z)
  
  limits = rolling_model$chunks[i]
  tab = table(factor(unlist(assignments[rolling_model$dates >= limits$start.date &
                                          rolling_model$dates <= limits$end.date])+1, levels = 1:K))
  for(k in seq_len(K)){
    topics_run = rowSums(topics[[k]][,max(1,i-z1[k]):(i-1), drop = FALSE])
    sim1[i, k] = cosine(topics[[k]][,i], topics_run)
    topics_tmp = Reduce("+", topics_chunks[max(1,i-z1[k]):(i-1)]) + eta
    phi = topics_tmp / rowSums(topics_tmp)
    topics_tmp = topics_chunks[[i]] + eta
    phi_tmp = phi[k,] 
    phi = topics_tmp / rowSums(topics_tmp)
    phi_tmp = (1-rel)*phi_tmp + rel*phi[k,]
    quantiles = replicate(500, {
      topics_resampled = tabulate(
        sample(length(vocab),
               size = tab[k],
               replace = TRUE,
               prob = phi_tmp), nbins = length(vocab))
      cosine(topics_resampled, topics_run)
    })
    quantiles1[i, k] = quantile(quantiles, 0.01, na.rm = TRUE)
  }
  run_length1[sim1[i,] < quantiles1[i,]] = 0L
}
saveRDS(quantiles1, here(data_path,"rollingLDA", paste0("quantiles", rel, ".rds")))
saveRDS(sim1, here(data_path,"rollingLDA", paste0("sim", rel, ".rds")))

# wordimpact
quantiles <- quantiles1
sim <- sim1
events_end <- apply(sim < quantiles, 2, function(d) rolling_model$chunks$end.date[which(d)])
events_start <- apply(sim < quantiles, 2, function(d) rolling_model$chunks$start.date[which(d)])
events_ind <- apply(sim < quantiles, 2, function(d) which(d))

loo <- lapply(seq_along(events_ind), function(k){
  if(length(events_ind[[k]]) > 0){
    loo <- sapply(events_ind[[k]], function(i){
      tmp = rowSums(topics[[k]][,max(1,i-8):(i-1), drop = FALSE])
      loo = cosine(topics[[k]][,i], tmp) -
        sapply(seq_len(length(vocab)), function(j) cosine(topics[[k]][,i][-j], tmp[-j]))
    })
    rownames(loo) = vocab
    colnames(loo) = as.character(events_ind[[k]])
  }else loo = NULL
  loo
})

pdf(here(data_path,"rollingLDA", paste0("wordimpact2", rel, ".pdf")), height = 8, width = 10)
for(k in seq_len(K)){
  zaehler = 0
  for(i in colnames(loo[[k]])){
    zaehler = zaehler + 1
    tmp = c(head(sort(loo[[k]][,i]), 10), tail(sort(loo[[k]][,i]), 5))
    print(ggplot() +
            geom_bar(aes(x = reorder(names(tmp), tmp), y = tmp), stat = "identity") +
            xlab("") + ylab("Impact on Cosine Similarity") +
            ggtitle(paste0(events_start[[k]][zaehler], " - ", events_end[[k]][zaehler], ", Topic ", k, ": ", topwords[k])))
  }
}
dev.off()

## changes
nr <- 5
nc <- 5
chunk_dates <- rolling_model$chunks$end.date
events <- data.table(k = rep(seq_along(events_end), lengths(events_end)),
                    start = as.Date(unlist(lapply(events_start, as.character))),
                    end = as.Date(unlist(lapply(events_end, as.character))))
fwrite(events, file = here(data_path, "rollingLDA", paste0("changes", rel, ".csv")))

pdf(here(data_path,"rollingLDA",  paste0("changes", rel, ".pdf")), height = 8, width = 8)
print(ggmatrix(lapply(1:K, function(i){
  ggplot() + ylim(c(-0.1,1)) +
    geom_vline(xintercept = events_end[[i]], col = "darkgrey") +
    geom_line(aes(x = chunk_dates, y = quantiles[,i], col = "1")) +
    geom_line(aes(x = chunk_dates, y = sim[,i], col = "3")) +
    ggplot2::annotate("text", x = min(chunk_dates), y = -0.08, 
                      label = paste0(i, "_", topwords[i]), hjust = 0, vjust = 0, cex = 1)
}), nrow = nr, ncol = nc, ylab = "Cosine Similarity", title = paste0("Backward looking reference period of length ", z, " | mix = ", q)))
dev.off()
#}