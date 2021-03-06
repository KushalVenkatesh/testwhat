#' Set up state for local experimentation.
#'
#' It runs both the solution and the student submission, and populates the state
#' with parse data, output, etc. After running this function, the state is
#' available thorugh \code{\link{ex}}, from which you can start your SCT chains.
#' In a way, this function is a very light weight version of DataCamp's R Backend.
#' 
#' @param sol_code Solution script as a string. If it is not specified, the
#'   student code will be used.
#' @param stu_code Student submission as a string. If it is not specified, the
#'   solution code will be used.
#' @param pec Pre-exercise-code as a string
#' @param ex_type Type of exercise as a string. Defaults to NormalExercise.
#' @return The exercise state, from which you can start chaining.
#' @note This function is only supposed to be used locally when experimenting.
#'   It should never be used in the eventual SCT script of an exercise.
#' @examples
#' \dontrun{
#' setup_state(
#'  sol_code = "a <- 1",
#'  stu_code = "a <- 2"
#' )
#'
#' ex() %>% check_object('a') %>% check_equal()
#' }
#' 
#' @importFrom evaluate evaluate
#' @export
setup_state <- function(sol_code, stu_code, pec = character(), ex_type = "NormalExercise") {
  
  if (base::missing(stu_code) && base::missing(sol_code)) {
    stop("Either stu_code or sol_code have to be specified.")
  } else {
    if (base::missing(stu_code)) {
      stu_code <- sol_code
    }
    if (base::missing(sol_code)) {
      sol_code <- stu_code
    }
  }
  
  if (ex_type == "MarkdownExercise") {
    capture_code <- function(lst) {
      withr::with_tempfile("file", pattern = "doc", fileext = ".Rmd", {
        write(lst[names(lst)[grepl(".rmd|.Rmd", names(lst))]], file = file)
        res <- knitr::purl(file, documentation = 0, quiet = TRUE)
        r_code <- paste(readLines(res), collapse = "\n")
        unlink(res)
        return(r_code)
      })
    }
    
    sol_code_to_run <- capture_code(sol_code)
    stu_code_to_run <- capture_code(stu_code)
  } else if (ex_type == "FileExercise") {
    sol_code_to_run <- ""
    stu_code_to_run <- ""
  } else if (ex_type == "RCppExercise") {
    populate_tmpfile <- function(code) {
      file <- tempfile(fileext = ".cpp")
      write(code, file)
      return(file)
    }
    sol_file <- populate_tmpfile(sol_code)
    stu_file <- populate_tmpfile(stu_code)
    sol_code_to_run <- sprintf("sourceCpp('%s', env = tw$get('sol_env'))", sol_file)
    stu_code_to_run <- sprintf("sourceCpp('%s', env = tw$get('stu_env'))", stu_file)
    on.exit(unlink(sol_file))
    on.exit(unlink(stu_file))
  } else {
    sol_code_to_run <- sol_code
    stu_code_to_run <- stu_code
  }
  
  # Parts of r-backend
  convert <- function(item, ...) UseMethod("convert")
  
  convert.default <- function(item, ...) {
    list(type = "output", payload = gsub("\n+$", "", item))
  }
  
  convert.source <- function(item, ...) {
    list(type = "code", payload = gsub("\n+$", "", item$src))  
  }
  
  convert.message <- function(item, ...) {
    list(type = "r-message", payload = gsub("\n+$", "", item$message))  
  }
  
  convert.warning <- function(item, ...) {
    list(type = "r-warning", payload = paste0("Warning message: ", item$message))
  }
  
  convert.error <- function(item, ...) {
    list(type = "r-error", payload = paste0("Error: ", item$message))  
  }
  
  sol_env <- new_env()
  stu_env <- new_env()
  tw$set(sol_env = sol_env)
  tw$set(stu_env = stu_env)
  
  withr::with_seed(123, {
    evaluate::evaluate(pec, envir = sol_env, stop_on_error = 2)
    evaluate::evaluate(sol_code_to_run, envir = sol_env, stop_on_error = 2)
  })
  withr::with_seed(123, {
    evaluate::evaluate(pec, envir = stu_env, stop_on_error = 2)
    output <- evaluate::evaluate(stu_code_to_run, envir = stu_env)  
  })
  
  formatted_output <- sapply(output, function(x) list(convert(x)))
  
  tw$clear()
  tw$set(success_msg = "good job")
  
  state <- RootState$new(
    pec = pec,
    student_code = stu_code,
    student_pd = build_pd(stu_code),
    student_env = stu_env,
    solution_code = sol_code,
    solution_pd = build_pd(sol_code),
    solution_env = sol_env,
    output_list = formatted_output,
    test_env = new.env(parent = environment())
  )
  
  # testwhat will access the reporter and state from the tw object
  tw$set(state = state, stack = TRUE, seed = 42)
  return(invisible(tw$get("state")))
}

new_env <- function() {
  new.env(parent = globalenv())
}
