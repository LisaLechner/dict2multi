#' Translate pivot dictionary to multilingual dictionary using individual translations
#' (word by word)
#'
#' @param pivot_dictionary of class `pivot_dictionary` or `expanded_pivot_dictionary`
#' @param api_deepl the API of the DeepL API Pro Account. This is required for the translations.
#' @param pivot_language a character value indicating the pivot language. Default is EN.
#' @param target_language a character vector indicating the target languages. Default is FR and DE.
#' @param target_folder the folder where the output should be stored. Default is  the working directory.
#' @param suffix_filename a suffix being added to the filename as suffix.
#' Default is pivot2multi_context_translation.
#' @return a dataframe including a column with the `pivot_language`-words,
#' the corresponding translated word in the `target_language`, and the
#' `cosine`-similarity of the word-pair.
#' @importFrom plyr ldply
#' @importFrom tidyr pivot_wider


pivot2multi_individual <- function(pivot_dictionary = NULL,
                                   api_deepl = NULL,
                                   pivot_language = "EN",
                                   target_language = c("FR", "DE"),
                                   min_count_collocations = 10,
                                   target_folder = getwd(),
                                   suffix_filename = "pivot2multi_context_translation") {
  pivot_language <- toupper(pivot_language)
  target_language <- toupper(target_language)

  lang <- deeplr::available_languages(auth_key = api_deepl)
  lang <- toupper(names(txt_all))[toupper(names(txt_all)) %in% target_language]
  message(cat("The following languages are available for translations:", paste0(lang, collapse = ", ")))
  lang <- lang[lang != pivot_language]

  # translate fulltexts

  temp0 <- lapply(seq_along(pivot_dictionary$dictionary_words$words), function(w) {
    if ((deeplr::usage(api_deepl)$character_limit - deeplr::usage(api_deepl)$character_count) > sum(nchar(x))) {
      x <- pivot_dictionary$dictionary_words$words[w]


      temp0 <- lapply(seq_along(lang), function(l) {
        temp1 <- data.frame(
          original = x,
          target = deeplr::translate(x, target_lang = lang[l], auth_key = api_deepl),
          lang = lang[l]
        )
        return(temp1)
      }) %>% ldply()
      return(temp0)
    }
  }) %>% ldply()

  temp <- temp %>% pivot_wider(
    id_cols = original,
    names_from = lang,
    values_from = target
  )
  names(temp)[1] <- pivot_language
  return(temp)
}
