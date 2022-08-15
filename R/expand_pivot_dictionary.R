#' Expand pivot dictionary using word embeddings
#'
#' @param dfmat a document-feature matrix of class `dfm`.
#' @param pivot_dictionary an object of class `pivot_dictionary`
#' produced by the function `create_pivot_dictionary` on the same dfmat as this function is called upon.
#' @param min_cosine a numeric value indicating the threshold for the similarity values below
#' which similarity values will not be returned. Default is 0.40.
#' @param sentiment_name a character value representing the variable name of the requested dimension,
#' which must be a variable in the `dfm` indicating the manual coding.
#' @return An object of class `expanded_pivot_dictionary` including the `dictionary_words`, the `sentiment_name`,
#' the minimum cosine value (`min_cosine`), the names of the `training_docs`,
#' the names of the `test_docs`, the prediction values for the test documents (`out_sample_prediction`),
#' the prediction values for the training documents (`in_sample_prediction`),
#' the accuracy of the predictions for the test sample (`out_sample_accuracy`),
#' the accuracy of the predictions for the training sample (`in_sample_accuracy`),
#' the accuracy of the predictions for the test sample
#' for the unexpanded/original dictionary (`original_out_sample_accuracy`),
#' the accuracy of the predictions for the training sample (`original_in_sample_accuracy`).
#' @importFrom quanteda.textstats textstat_simil
#' @importFrom dplyr left_join filter mutate select group_by summarise ungroup
#' @importFrom quanteda featnames convert
#' @example
#' ?quanteda.corpora::data_corpus_amicus
#' corp <- quanteda.corpora::data_corpus_amicus
#' dfmat <- dfm(corp)
#' table(dfmat$trainclass)
#' dfmat$pro_respondent <- ifelse(dfmat$testclass=="AR"|dfmat$trainclass=="R",1,0)
#' dfmat$pro_respondent <- ifelse(is.na(dfmat$pro_respondent),0,dfmat$pro_respondent)
#' dfmat$pro_petitioner <- ifelse(dfmat$testclass=="AP"|dfmat$trainclass=="AP",1,0)
#' dfmat$pro_petitioner <- ifelse(is.na(dfmat$pro_petitioner),0,dfmat$pro_petitioner)
#' out_respondent <- create_pivot_dictionary(dfmat=dfmat,
#'                                           sentiment_name = 'pro_respondent',
#'                                           alpha_elastic_net = 0.1)
#' out_respondent_expanded <- expand_pivot_dictionary(dfmat=dfmat,
#'                                                    pivot_dictionary = out_respondent,
#'                                                    sentiment_name = 'pro_respondent',
#'                                                    min_cosine = 0.3)

head(out_respondent$dictionary_words, 10)
head(out_respondent_expanded$dictionary_words, 10)

expand_pivot_dictionary <- function(dfmat = NULL,
                                    pivot_dictionary = NULL,
                                    sentiment_name = NULL,
                                    min_cosine = 0.40) {
  if (!is.dfm(dfmat)) {
    stop("Input matrix must be of class quanteda::dfm.")
  }
  # calculate embedding
  out <- quanteda.textstats::textstat_simil(x = dfmat, margin = "features", method = "cosine", min_simil = min_cosine)
  out <- as.data.frame(out)
  names(out)[1] <- "word"

  # adopt to sentiment
  out <- left_join(pivot_dictionary$dictionary_words %>% filter(s0 != 0), out) %>% mutate(s02 = s0 * cosine)
  out1 <- out %>% dplyr::select(word, s0)
  out2 <- out %>% dplyr::select(feature2, s02)
  names(out2) <- c("word", "s0")
  out <- unique(rbind(out1, out2)) %>%
    dplyr::group_by(word) %>%
    dplyr::summarise(s0 = mean(s0, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(word), !is.na(s0))

  rm(out1, out2)

  dfmat_test <- dfmat[!docnames(dfmat) %in% pivot_dictionary$test_docs, featnames(dfmat) %in% out$word]
  mat_test <- convert(dfmat_test, to = "matrix")
  out <- out[match(colnames(mat_test), out$word), ]
  mat_test <- mat_test * out$s0

  dfmat_training <- dfmat[!docnames(dfmat) %in% pivot_dictionary$training_docs, featnames(dfmat) %in% out$word]
  mat_training <- convert(dfmat_training, to = "matrix")
  out <- out[match(colnames(mat_training), out$word), ]
  mat_training <- mat_training * out$s0

  out_sample_prediction <- data.frame(doc_id = row.names(mat_test), prediction = rowSums(mat_test), value = docvars(dfmat_test)[[sentiment_name]])
  out_sample_accuracy <- cor.test(out_sample_prediction$prediction, out_sample_prediction$value, use = "pairwise")

  in_sample_prediction <- data.frame(doc_id = row.names(mat_training), prediction = rowSums(mat_training), value = docvars(dfmat_training)[[sentiment_name]])
  in_sample_accuracy <- cor.test(in_sample_prediction$prediction, in_sample_prediction$value, use = "pairwise")

  out <- list(
    dictionary_words = out,
    sentiment_name = sentiment_name,
    min_cosine = min_cosine,
    training_docs = pivot_dictionary$training_docs,
    test_docs = pivot_dictionary$test_docs,
    out_sample_prediction = out_sample_prediction,
    out_sample_accuracy = out_sample_accuracy,
    in_sample_prediction = in_sample_prediction,
    in_sample_accuracy = in_sample_accuracy,
    original_in_sample_accuracy = pivot_dictionary$in_sample_accuracy,
    original_out_sample_accuracy = pivot_dictionary$out_sample_accuracy
  )
  class(out) <- "expanded_pivot_dictionary"
  return(out)
}
