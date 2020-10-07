set.seed(12345)


k_list <- seq(1, 25, by = 1)
model_dir <- paste0("models_", digest::digest(colnames(nih_sample_dtm), algo = "sha1"))

# Fit a bunch of LDA models
# even on this trivial corpus, it will a bit of time to fit all of these models
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

#export only needed for Windows machines
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

model <- model_list[which.max(coherence_mat$coherence)][[1]]
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 1)


model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)


model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]

top20_wide <- as.data.frame(model$top_terms)


# predictions with gibbs
assignments <- predict(model, dtm,
                       method = "gibbs", 
                       iterations = 200,
                       burnin = 180,
                       cpus = 2)

# predictions with dot
assignments_dot <- predict(model, dtm,
                           method = "dot")


# compare
barplot(rbind(assignments[10,], assignments_dot[10,]),
        col = c("red", "blue"), las = 2, beside = TRUE)
legend("topright", legend = c("gibbs", "dot"), col = c("red", "blue"), 
       fill = c("red", "blue"))
