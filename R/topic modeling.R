set.seed(12345)


k_list <- seq(1, 25, by = 1)
model_dir <- paste0("models_", digest::digest(colnames(nih_sample_dtm), algo = "sha1"))

# Fit a bunch of LDA models
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  
  m <- FitLdaModel(dtm = dtm, 
                   k = k, 
                   iterations = 200, 
                   burnin = 180,
                   alpha = 0.1,
                   beta = colSums(dtm) / sum(dtm) * 100,
                   optimize_alpha = TRUE,
                   calc_likelihood = FALSE,
                   calc_coherence = TRUE,
                   calc_r2 = FALSE,
                   cpus = 1)
  m$k <- k
  
  m
}, export= ls(), # c("nih_sample_dtm"), # export only needed for Windows machines
cpus = 2) 



####evaluasi model

#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)


ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + 
  ylab("Coherence")

###best model
model$summary <- SummarizeTopics(model)

top20_terms <- as.data.frame(model$top_terms)


# predictions with gibbs
assignments <- predict(model, dtm,
                       method = "gibbs", 
                       iterations = 200,
                       burnin = 180,
                       cpus = 2)

hasil_pred <- data.frame(assignments, id = rownames(dtm),
                         text = tokens$text) %>% as_tibble

hasil_pred %>% 
  transmute(id, max = max(c_across(t_1:t_14)))

hasil_pred %>% gather("topics", "values", -id, -text) %>% 
  arrange(id %>% desc, values %>% desc) %>% 
  filter(values>=0.5) %>% 
  group_by(id) %>% 
  top_n(3)
  
  slice(which.max(values))
