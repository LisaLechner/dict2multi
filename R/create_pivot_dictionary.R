#' Create pivot language dictionary from manually annotated documents
#'
#' @param dfmat a document-feature matrix of class `dfm`.
#' @param sentiment_name a character value representing the variable name of the requested dimension,
#' which must be a variable in the `dfm` indicating the manual coding.
#' @param oversampling_max a numeric value between 0 and 1 representing the number of positivly labelled documents.
#' The default is 0.2 meaning that until the sentiment appears in less than 20 percent of the documents oversampling is conducted.
#' @param training_size a numeric value between 0 and 1 indicating the training sample size.
#' The default is 0.9 standing for 90 percent of documents being used as training set and 10 percent of
#' documents being used as test set.
#' @param seed an integer value specifying the seeds.
#' @param alpha_elastic_net a numeric value between 0 and 1 indicating the mixing parameter between
#' a LASSO and a RIDGE estimation. The default is 0.9 favoring the LASSO estimation.
#' @return An object of class `pivot_dictonary` including the `dictionary_words`, the `sentiment_name`,
#' the optimal lambda (`lambda`) from the elastic net estimation, the names of the `training_docs`,
#' the names of the `test_docs`, the prediction values for the test documents (`out_sample_prediction`),
#' the prediction values for the training documents (`in_sample_prediction`),
#' the accuracy of the predictions for the test sample (`out_sample_accuracy`),
#' the accuracy of the predictions for the training sample (`in_sample_accuracy`)
#'  @importFrom quanteda is.dfm dfm_subset dfm_sample docvars ndoc convert
#'  @importFrom glmnet cv.glmnet glmnet
#'  @importFrom dplyr mutate arrange
#' @examples
#' ?quanteda.corpora::data_corpus_amicus
#' corp <- quanteda.corpora::data_corpus_amicus
#' dfmat <- dfm(corp)
#' table(dfmat$trainclass)
#' dfmat$pro_respondent <- ifelse(dfmat$testclass == "AR" | dfmat$trainclass == "R", 1, 0)
#' dfmat$pro_respondent <- ifelse(is.na(dfmat$pro_respondent), 0, dfmat$pro_respondent)
#' dfmat$pro_petitioner <- ifelse(dfmat$testclass == "AP" | dfmat$trainclass == "AP", 1, 0)
#' dfmat$pro_petitioner <- ifelse(is.na(dfmat$pro_petitioner), 0, dfmat$pro_petitioner)
#' out_respondent <- create_pivot_dictionary(
#'   dfmat = dfmat,
#'   sentiment_name = "pro_respondent",
#'   alpha_elastic_net = 0.1
#' )
#' head(out_respondent$dictionary_words)
#' tail(out_respondent$dictionary_words)

#' out_petitioner <- create_pivot_dictionary(dfmat=dfmat,
#'                                           sentiment_name = 'pro_petitioner',
#'                                           alpha_elastic_net = 0.1)
#' head(out_petitioner$dictionary_words)
#' tail(out_petitioner$dictionary_words)



create_pivot_dictionary <- function(dfmat = NULL,
                                    sentiment_name = NULL,
                                    oversampling_max = 0.2,
                                    training_size = 0.9,
                                    seed = 1111,
                                    alpha_elastic_net = 0.9) {
  if (!is.dfm(dfmat)) {
    stop("Input matrix must be of class quanteda::dfm.")
  }

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
  out <- out %>% arrange(desc(s0))
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
