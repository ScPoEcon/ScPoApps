
get_lm <- function(a,b,n=20,xscale = 1, escale = 1, seed = 1){
    set.seed(seed)
    x = runif(n,min = -1*xscale, max = xscale)
    y <- a + b * x + rnorm(n,sd = escale)
    list(x=x,y=y, a = a, b = b, n = n, xscale = xscale, escale = escale)
}



#' runTutorial: Run a Tutorial!
#'
#' @param tutoname string of which tutorial you want to run
#' This function runs a given `tutoname` for you. run without an argument `runTutorial()` to see a list of available tutorial.
#' @export
runTutorial <- function(tutoname) {
  # locate all the examples that exist
  validExamples <- list.files(system.file("tutorial", package = "ScPoApps"))

  validExamplesMsg <-
    paste0(
      "Valid tutorial are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(tutoname) || !nzchar(tutoname) ||
      !tutoname %in% validExamples) {
    stop(
      'Please run `runTutorial()` with a valid tutorial as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch
  appDir <- system.file("tutorial", tutoname, package = "ScPoApps")
  rmds = list.files(path=appDir,pattern="\\.Rmd$",full.names=TRUE)
  rmarkdown::run(file = rmds[1])
}


#' launchApp: Launch an App!
#'
#' @param appname string of which app you want to run
#' This function runs the shiny app for a given `appname`. run without an argument `launchApp()` to see a list of available apps.
#' @export
launchApp <- function(appname) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shinys", package = "ScPoApps"))

  validExamplesMsg <-
    paste0(
      "Valid apps are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(appname) || !nzchar(appname) ||
      !appname %in% validExamples) {
    stop(
      'Please run `launchApp()` with a valid app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shinys", appname, package = "ScPoApps")
  shiny::runApp(appDir, display.mode = "normal")
}

#' aboutApp : get more information about an App !
#'
#' @param appname string of which app you want to get info about
#' This function opens the description of any given app
#'
#' @export
aboutApp <- function(appname){

  # locate all the shiny app examples that exist
  validExamples <- character(0)
  v <- list.files(system.file("shinys", package = "ScPoApps"),full.names=TRUE)
  for (i in v){
    # if i has an about.Rmd
    if (file.exists(file.path(i,"about.Rmd"))){
      validExamples <- c(validExamples,basename(i))
    }
  }

  validExamplesMsg <-
    paste0(
      "Valid apps are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(appname) || !nzchar(appname) ||
      !appname %in% validExamples) {
    stop(
      'Please run `aboutApp()` with a valid app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # render and launch the about document
  appDir = system.file("shinys", appname, package = "ScPoApps")
  rmarkdown::run(file = file.path(appDir, "about.Rmd"))
}


pasta_maker <- function(){
  pasta_jar <- tibble::tibble(id = 1:1980,color = sample(c(rep("Red",488),rep("Green",492),rep("White",1000)), size = 1980 ))
  usethis::use_data(pasta_jar,overwrite = TRUE)
  pasta_jar
}

getprompt <- function(){
  return("#OUT>")
}
