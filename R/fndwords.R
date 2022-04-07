#' Odgadnij słowo w krzyżówce
#'
#' @description Funckja pozwala na wydzielenie słów z danego zbioru wyrazów.
#'
#' @param write_a_word - Jako argument należy wpisać słowo, które chcemy odgadnąć. W miejsce liter, które nie są znane, należy wpisać kropki.
#'
#' @param dictionary - Jako argument należy podać obiekt, który jest wektorem, w którym znajdują się słowa.
#'
#' @param report - Wartość TRUE oznacza, że do pliku txt. zostanie zapisany wynik funkcji. Ponadto utworzony zostanie folder results, w którym umieszczane będą wszystkie wyniki.
#'
#' @param to_low - Wartość TRUE sprawia, że wszystkie wielkie litery w wektorze dictionary zostaną zamienione na małe. Ten argument przydaje się m.in. w przypadku słów niemieckich, ponieważ rzeczowniki w tym języku są rozpoczęte wielkimi literami. Niewykorzystanie tego argumentu może doprowadzić do otrzymania niepełnych wyników.
#'
#' @return wektor wydzielonych słów.
#' @export
#'
#' @examples find_words("v..y", c("nice", "very", "small", "package", "bro"), report = TRUE)
#' find_words("....", c("nice", "very", "small", "package", "bro"), report = FALSE)


  find_words = function(write_a_word, dictionary, report = FALSE, to_low = TRUE){
    if(is.vector(write_a_word) == FALSE){
      stop("It must be a vector!")
    }
  write_a_word = paste("^", write_a_word, "$", sep = "")
  if(to_low == TRUE){
    dictionary = tolower(dictionary)
  }
  extracted_words = stringr::str_subset(dictionary, pattern = write_a_word)

  if(report == TRUE){
    first_word = extracted_words[1]
    if(dir.exists("results") == FALSE){
      dir.create("results")
    }
    write.table(c(extracted_words), file = paste("results/", first_word, ".txt"), sep = "")
    cat("Report created successfully.")
    dir_return = paste(" Directory: results/", first_word, ".txt", sep = "")
    cat(dir_return)
  }
  cat("\n")
  return(extracted_words)
}
