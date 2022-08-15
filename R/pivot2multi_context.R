#' Translate pivot dictionary to multilingual dictionary using the context translations
#' (fulltext translations and word-embeddings)
#'
#' @param corpus represents an object of class `corpus` from the quanteda package
#' collecting documents of the pivot language corpus.
#' @param api_deepl the API of the DeepL API Pro Account. This is required for the translations.
#' @param pivot_language a character value indicating the pivot language. Default is EN.
#' @param target_language a character vector indicating the target languages. Default is FR and DE.
#' @param min_count_collocations a numeric value indicating the minimum number of times
#' a collocation needs to appear in the corpus to be counted as collocation. Default is 10.
#' @param target_folder the folder where the output should be stored. Default is  the working directory.
#' @param suffix_filename a suffix being added to the filename as suffix.
#' Default is pivot2multi_context_translation.
#' @return a dataframe including a column with the `pivot_language`-words,
#' the corresponding translated word in the `target_language`, and the
#' `cosine`-similarity of the word-pair.


pivot2multi_context <- function(corpus=NULL,
                                api_deepl=NULL,
                                pivot_language='EN',
                                target_language=c('FR','DE'),
                                min_count_collocations=10,
                                target_folder=getwd(),
                                suffix_filename='pivot2multi_context_translation'){

  if (!is.corpus(corpus)) {
    stop("Input must be an object of class quanteda::corpus.")
  }

  pivot_language <- toupper(pivot_language)
  target_language <- toupper(target_language)

  lang <- deeplr::available_languages(auth_key = api_deepl)
  lang <- toupper(names(txt_all))[toupper(names(txt_all)) %in% target_language]
  message(cat("The following languages are available for translations:",paste0(lang,collapse = ", ")))
  lang <- lang[lang != pivot_language]

# translate fulltexts
dir.create(paste0(tempdir(),'/fulltext_translations/'))

for(i in seq_along(lang)){
  dir.create(paste0(tempdir(),'/fulltext_translations/',lang[i],"/"))
}

  for (i in seq_len(ndoc(corpus))) {
    x <- corpus[[i]]

    if ((deeplr::usage(api_deepl)$character_limit - deeplr::usage(api_deepl)$character_count) > sum(nchar(x))) {

       for (l in seq_along(lang)) {
        if (file.exists(paste0(tempdir(),'/fulltext_translations/', lang[l], "/", names(corpus)[[i]], ".corpus"))) {
          print("file exists already")
        } else {
          temp0 <- deeplr::translate(x, target_lang = lang[l], auth_key = api_deepl)
          write_lines(temp0, paste0(tempdir(), '/fulltext_translations/',lang[l], "/", names(corpus)[[i]], ".corpus"))
        }
      }
    }
  }

  # create embeddings
  lang <- list.files(paste0(tempdir(), '/fulltext_translations/'))

  #----
  # select same english texts
  #----
  f <- list.files(paste0(tempdir(), '/fulltext_translations/', lang[1], "/"))
  toks <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE) %>%
    tokens_remove(stopwords(pivot_lang_news))
  toks <- tokens_tolower(toks)
  col <- tokens_select(toks,
                       pattern = "^[A-Z]",
                       valuetype = "regex",
                       case_insensitive = FALSE,
                       padding = TRUE
  ) %>%
    textstat_collocations(min_count = min_count_collocations)

  # collocations
  if (exists(noquote(paste0("col_", pivot_lang_news)))) {
    col2 <- unique(c(col$collocation, noquote(paste0("col_", pivot_lang_news))))
  } else {
    (col2 <- col$collocation)
  }

  if (length(col2) > 0) {
    toks <- tokens_compound(toks,
                            pattern = col2,
                            concatenator = " "
    )
  }

  toks <- toks %>%
    tokens_tolower(., keep_acronyms = TRUE)



  # generate dfm
  dfmat <- dfm(toks)

  dfmat <- dfm_trim(dfmat, min_docfreq = 0.01, max_docfreq = 0.99, docfreq_type = "prop")
  dfmat <- dfm_tfidf(dfmat)
  dfmat0 <- dfmat
  dfmat0 <- dfm(tokens(corpus))



  for (i in seq_along(lang)) {
    txt1 <- readtext(paste0(tempdir(), '/fulltext_translations/', lang[i], "/*"), docvarsfrom = "filenames")
    txt1$text <- tolower(txt1$text)
    txt1$doc_id <- gsub(".txt", "", txt1$doc_id)
    txt1 <- txt1[match(docnames(dfmat0), txt1$doc_id), ]
    txt1$text <- tolower(txt1$text)
    txt1$doc_id <- gsub(".txt", "", txt1$doc_id)
    txt1 <- txt1[match(docnames(txt), txt1$doc_id), ]
    txt1 <- corpus(txt1)

    toks <- tokens(txt1, remove_punct = TRUE, remove_numbers = TRUE) %>%
      tokens_remove(stopwords(pivot_lang_news))

    col <- tokens_select(toks,
                         pattern = "^[A-Z]",
                         valuetype = "regex",
                         case_insensitive = FALSE,
                         padding = TRUE
    ) %>%
      textstat_collocations(min_count = 10)

    # collocations
    if (exists(noquote(paste0("col_", pivot_lang_news)))) {
      col2 <- unique(c(col$collocation, noquote(paste0("col_", pivot_lang_news))))
    } else {
      (col2 <- col$collocation)
    }

    if (length(col2) > 0) {
      toks <- tokens_compound(toks,
                              pattern = col2,
                              concatenator = " "
      )
    }

    toks <- toks %>%
      tokens_tolower(., keep_acronyms = TRUE)



    # generate dfm
    dfmat <- dfm(toks)

    dfmat <- dfm_trim(dfmat, min_docfreq = 0.01, max_docfreq = 0.95, docfreq_type = "prop")
    dfmat <- dfm_tfidf(dfmat)
    dfmat1 <- dfmat


    sim12 <- textstat_simil(x = dfmat0, y = dfmat1, margin = "features", method = "cosine")
    sim12 <- sim12 %>% data.frame()
    sim12b <- sim12 %>%
      group_by(feature1) %>%
      dplyr::summarise(cosine_max = max(cosine))

    sim12 <- left_join(sim12, sim12b) %>% filter(cosine == cosine_max)
    sim12 <- sim12 %>%
      dplyr::select(-cosine_max) %>%
      filter(cosine >= 0.2)
    names(sim12)[1] <- pivot_language
    names(sim12)[2] <- lang[i]
    sim12 <- sim12 %>% arrange(desc(cosine))

    saveRDS(sim12, paste0(target_folder,suffix_filename, "_",pivot_language, "_to_", tolower(lang[i]),'.rds'))
  }

}


