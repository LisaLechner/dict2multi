




create_pivot_dictionary <- function(dfmat = dfmat,
                                    coding_id = NULL,
                                    sentiment_name = NULL,
                                    oversampling_max = 0.2,
                                    training_size = 0.9,
                                    seed = 1111,
                                    alpha_elastic_net = 0.9) {
  # check overlapping names
  dfmat0 <- dfm_subset(dfmat, !is.na(docvars(dfmat)[[sentiment_name]]))
  set.seed(seed)
  dfmat_training <- dfm_sample(dfmat0, round(ndoc(dfmat0) * 0.9))
  # oversampling
  if (sum(docvars(dfmat_training)[sentiment_name] != 0) / ndoc(dfmat_training) < oversampling_max) {
    l <- round((ndoc(dfmat_training) * oversampling_max) - sum(docvars(dfmat_training)[sentiment_name] > 0)) # how many documents, we need to add

    dfmat_add <- dfm_sample(dfmat_training, size = l, replace = TRUE)

    dfmat2 <- rbind(dfmat_training, dfmat_add)
  } else {
    (dfmat2 <- dfmat_training)
  }
  # elastic net
  # first find best lambda
  cv_output <- cv.glmnet(as.matrix(dfmat2), docvars(dfmat2)[[sentiment_name]])
  best_lam <- cv_output$lambda.min

  # run elastic net with best lambda
  elastic_net <- glmnet(as.matrix(dfmat2),
    docvars(dfmat2)[[sentiment_name]],
    family = "gaussian",
    alpha = alpha_elastic_net, # the elastic net mixing parameter
    intercept = F,
    lambda = best_lam
  )
  out <- as.matrix(coef(elastic_net))
  out <- out %>%
    as.data.frame() %>%
    mutate(word = (row.names(out))) %>%
    arrange(desc(s0))

  dfmat_test <- dfmat0[!docnames(dfmat0) %in% docnames(dfmat2), featnames(dfmat0) %in% out[out$s0 != 0, ]$word]
  mat_test <- convert(dfmat_test, to = "matrix")
  out <- out[match(colnames(mat_test), out$word), ]
  mat_test <- mat_test * out[out$s0 != 0, ]$s0

  dfmat_training <- dfmat_training[, featnames(dfmat_training) %in% out[out$s0 != 0, ]$word]
  mat_training <- convert(dfmat_training, to = "matrix")
  out <- out[match(colnames(mat_training), out$word), ]
  mat_training <- mat_training * out[out$s0 != 0, ]$s0


  out_sample_prediction <- data.frame(doc_id = row.names(mat_test), prediction = rowSums(mat_test), value = docvars(dfmat_test)[[sentiment_name]])
  out_sample_accuracy <- cor.test(out_sample_prediction$prediction, out_sample_prediction$value, use = "pairwise")

  in_sample_prediction <- data.frame(doc_id = row.names(mat_training), prediction = rowSums(mat_training), value = docvars(dfmat_training)[[sentiment_name]])
  in_sample_accuracy <- cor.test(in_sample_prediction$prediction, in_sample_prediction$value, use = "pairwise")


  out <- list(
    dictionary_words = out,
    sentiment_name = sentiment_name,
    lambda = best_lam,
    training_docs = docnames(dfmat2),
    test_docs = docnames(dfmat0)[!docnames(dfmat0) %in% docnames(dfmat2)],
    out_sample_prediction = out_sample_prediction,
    out_sample_accuracy = out_sample_accuracy,
    in_sample_prediction = in_sample_prediction,
    in_sample_accuracy = in_sample_accuracy
  )
  class(out) <- "pivot_dictonary"
  return(out)
}
