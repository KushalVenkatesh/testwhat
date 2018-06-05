#' @export
grade_testwhat <- function(label = NULL,
                           solution_code = NULL,
                           user_code = NULL,
                           check_code = NULL,
                           envir_result = NULL,
                           evaluate_result = NULL,
                           ...) {
  
  print(solution_code)
  print(user_code)
  print(check_code)
  
  ######### START COPY FROM grade_learnr ##################
  # Sometimes no user code is provided, but
  # that means there is nothing to check. Also,
  # you do not want to parse NULL
  if (is.null(user_code)) {
    return(list(
      message = "I didn't receive your code. Did you write any?",
      correct = FALSE,
      type = "error",
      location = "append"
    ))
  }
  
  # Sometimes no solution is provided, but that
  # means there is nothing to check against. Also,
  # you do not want to parse NULL
  if (is.null(solution_code)) {
    return(list(
      message = "No solution is provided for this exercise.",
      correct = TRUE,
      type = "info",
      location = "append"
    ))
  }
  ######### END COPY FROM grade_learnr ##################
  
  setup_state_v2(stu_code = user_code,
                 stu_env = envir_result,
                 stu_out = evaluate_result,
                 sol_code = solution_code)
  
  if (is.null(check_code)) {
    check_code <- ""
  }
  
  res <- run_until_fail(parse(text = check_code))
  
  return(list(message = res$message,
              correct = res$correct,
              location = "append",
              type = if(res$correct) "success" else "error"))
}

#' @export
setup_state_v2 <- function(stu_code, stu_env, stu_out, sol_code) {
  
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
  
  sol_env <- new.env(parent = globalenv())
  evaluate::evaluate(sol_code, envir = sol_env, stop_on_error = 2)
  
  # reformat result of evaluate (passed by learnr)
  formatted_output <- sapply(stu_out, function(x) list(convert(x)))
  
  tw$clear()
  tw$set(success_msg = "Great work!")
  
  state <- RootState$new(
    pec = "",
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
