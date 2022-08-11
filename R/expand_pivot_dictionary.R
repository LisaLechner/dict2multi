expand_pivot_dictionary <- function(dfmat = dfmat,
                                    pivot_dictionary = pivot_dictionary,
                                    sentiment_name='doubt',
                                    min_cosine = 0.40){

  # calculate embedding
  out <- textstat_simil(x=dfmat,margin="features",method="cosine",min_simil = min_cosine)
  out <- as.data.frame(out)
  names(out)[1] <- 'word'

  # adopt to sentiment
  out <- left_join(pivot_dictionary$dictionary_words %>% filter(s0!=0),out) %>% mutate(s02 = s0 * cosine)
  out1 <- out %>% dplyr::select(word,s0)
  out2 <- out %>% dplyr::select(feature2,s02)
  names(out2) <- c('word','s0')
  out <- unique(rbind(out1,out2)) %>%
    dplyr::group_by(word) %>%
    dplyr::summarise(s0=mean(s0,na.rm=TRUE)) %>% ungroup %>% filter(!is.na(word),!is.na(s0))

  rm(out1,out2)

  dfmat_test <- dfmat[!docnames(dfmat) %in% pivot_dictionary$test_docs,featnames(dfmat) %in% out$word]
  mat_test <- convert(dfmat_test,to='matrix')
  out <- out[match(colnames(mat_test),out$word),]
  mat_test <- mat_test * out$s0

  dfmat_training <- dfmat[!docnames(dfmat) %in% pivot_dictionary$training_docs,featnames(dfmat) %in% out$word]
  mat_training <- convert(dfmat_training,to='matrix')
  out <- out[match(colnames(mat_training),out$word),]
  mat_training <- mat_training * out$s0

  out_sample_prediction <- data.frame(doc_id=row.names(mat_test), prediction=rowSums(mat_test),value=docvars(dfmat_test)[[sentiment_name]])
  out_sample_accuracy <- cor.test(out_sample_prediction$prediction, out_sample_prediction$value,use='pairwise')

  in_sample_prediction <- data.frame(doc_id=row.names(mat_training), prediction=rowSums(mat_training),value=docvars(dfmat_training)[[sentiment_name]])
  in_sample_accuracy <- cor.test(in_sample_prediction$prediction, in_sample_prediction$value,use='pairwise')

  out <- list(dictionary_words = out,
              sentiment_name = sentiment_name,
              min_cosine = min_cosine,
              training_docs = pivot_dictionary$training_docs,
              test_docs = pivot_dictionary$test_docs,
              out_sample_prediction = out_sample_prediction,
              out_sample_accuracy = out_sample_accuracy,
              in_sample_prediction = in_sample_prediction,
              in_sample_accuracy = in_sample_accuracy,
              original_in_sample_accuracy = pivot_dictionary$in_sample_accuracy,
              original_out_sample_accuracy = pivot_dictionary$out_sample_accuracy)
  class(out) <- 'expanded_pivot_dictionary'
  return(out)

}
