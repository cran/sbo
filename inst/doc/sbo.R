## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(sbo)

## -----------------------------------------------------------------------------
train <- twitter_train
test <- twitter_test

## -----------------------------------------------------------------------------
head(train, 3)

## -----------------------------------------------------------------------------
# N.B.: get_word_freqs(train) returns a tibble with a 'word' column 
# and a 'counts' column, sorted by decreasing counts.
dict <- get_word_freqs(train) %>% names %>% .[1:1000]
head(dict)

## -----------------------------------------------------------------------------
(freqs <- get_kgram_freqs(train, N = 3, dict)) # 'N' is the order of n-grams

## -----------------------------------------------------------------------------
( sbo <- build_sbo_preds(freqs) )

## -----------------------------------------------------------------------------
predict(sbo, "i love") # a character vector
predict(sbo, c("Colorless green ideas sleep", "See you")) # a char matrix

## -----------------------------------------------------------------------------
set.seed(840)
babble(sbo)
babble(sbo)
babble(sbo)

## ---- eval=FALSE--------------------------------------------------------------
#  save(sbo)
#  load("sbo.rda")

## -----------------------------------------------------------------------------
set.seed(840)
(eval <- eval_sbo_preds(sbo, test))

## -----------------------------------------------------------------------------
eval %>% summarise(accuracy = sum(correct)/n(), 
                   uncertainty = sqrt( accuracy*(1-accuracy) / n() )
                   )

## -----------------------------------------------------------------------------
eval %>% # Accuracy for in-sentence predictions
        filter(true != ".") %>%
        summarise(accuracy = sum(correct)/n(),
                  uncertainty = sqrt( accuracy*(1-accuracy) / n() )
                  )

## -----------------------------------------------------------------------------
if (require(ggplot2)) {
        eval %>%
                filter(correct, true != ".") %>%
                transmute(rank = match(true, table = sbo$dict)) %>%
                ggplot(aes(x = rank)) + geom_histogram(binwidth = 25)
}

