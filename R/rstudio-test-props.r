#' Check whether the student used the correct properties (ggvis exercises)
#'
#' Check whether the student used at least as many and the correct properties as the solution inside a specific
#' command and inside a specific function. By default, this function will compare the ggvis functions of both
#' student and solution. However, the teacher can also state that the definition of data can be done in other
#' functions.
#'
#' @param index  number of ggvis caommdn to be checked
#' @param funs  the function in which to look for the x and y data. If the same info is found in one function, the test passes.
#' All the functions that the teacher specifies, must be present in the students' solution! The function only looks for
#' properties inside the first mentioned function by the teacher.
#' @param props  set of properties to be checked. If not specified, all properties found in the solution or checked on. If
#' specified as an empty charactor vector (c()), only the calling of the functions will be checked on.
#' @param allow_extra  whether or not the definition of additional properties is accepted (default TRUE)
#' @param not_called_msg feedback message in case the specified function(s) was/were not found.
#' @param incorrect_msg  feedback message in case the student specified properties do not correspond with the ones in the solution.
#' @param error_msg feedback maessage in case the student submitted a faulty ggvis call
#' @keywords internal
test_props <- function(index = 1,
                       funs = "ggvis",
                       props = NULL,
                       allow_extra = TRUE,
                       not_called_msg = NULL,
                       incorrect_msg = NULL,
                       error_msg = NULL) {
  state <- ex()
  student_pd <- state$get("student_pd")
  solution_pd <- state$get("solution_pd")
  student_env <- state$get("student_env")
  solution_env <- state$get("solution_env")
  
  ggvis_pds_stud <- find_ggvis_pds(student_pd)
  ggvis_pds_sol <- find_ggvis_pds(solution_pd)
  
  if (length(ggvis_pds_sol) < index) {
    stop("Solution does not contain enough calls")
  }
  
  sol_calls <- find_function_calls(ggvis_pds_sol[[index]], name = funs[1], env = solution_env)
  if (length(sol_calls) > 1) {
    stop(sprintf("Function %s should only occur exactly once in the %s `ggvis` command of the solution.", funs[1], get_ord(index)))
  }
  
  sol_props <- get_all_props(funs[1], sol_calls[[1]]$call)
  if (is.null(sol_props)) {
    stop(sprintf("The %s ggvis command in your solution contains an error", get_ord(index)))
  }
  if (is.null(props)) {
    props <- names(sol_props)
  } else {
    # select from sol_props the ones to check on
    sol_props = sol_props[props]
    if (any(is.na(names(sol_props)))) {
      stop(sprintf("You defined properties that are not in %s() in command %i of the solution code", funs[1], index))
    }
  }
  
  # Set up default messages
  # message if specified function was not called
  if (is.null(not_called_msg)) {
    calls <- if (length(funs) == 1) "a call" else "calls"
    not_called_msg <- sprintf("The %s `ggvis` command in your code should contain %s to %s.", get_ord(index), calls, collapse_funs(funs))
  }
  # message if the properties or not found or set incorrectly
  if (is.null(incorrect_msg)) {
    propstr <- if (length(props) == 1) "property" else "properties"
    incorrect_msg <- sprintf("In the %s `ggvis` command of your code, make sure to correctly define the %s %s inside %s.",
                            get_ord(index), propstr, collapse_props(props), collapse_funs(funs, conn = " or "))
    if (length(props[!props  %in% c("x","y")]) > 0)
      incorrect_msg <- paste(incorrect_msg, "Beware of the difference between `=` and `:=`.")
    if (!allow_extra)
      incorrect_msg <- paste(incorrect_msg, "Do not define any other properties!")
    if (length(props) == 0)
      incorrect_msg <- sprintf("In the %s `ggvis` of your code, make sure that you do not define any properties inside %s.",
                              index, collapse_funs(funs))
  }
  if (is.null(error_msg)) {
    error_msg <- sprintf("There's an error in the %s `ggvis` command of your code. Have a look at the console output to fix the error.", get_ord(index))
  }
  
  pass <- FALSE
  keeptrying <- TRUE
  for (i in 1:length(funs)) {
    check_that(is_true(length(ggvis_pds_stud) >= index), feedback = not_called_msg)
    stud_calls <- find_function_calls(ggvis_pds_stud[[index]], name = funs[i], env = student_env)
    check_that(is_true(length(stud_calls) > 0), feedback = not_called_msg)
    
    if (pass) # if passed already, only check on the function being present, so next loop not needed anymore
      next
    
    # possibly more expressions are available
    for (j in 1:length(stud_calls)) {
      stud_props <- get_all_props(funs[i], stud_calls[[j]]$call)
      
      check_that(is_true(!is.null(stud_props)), feedback = error_msg)

      if (length(props) != length(stud_props) & !allow_extra) {
        # number of props specified does not correspond to function.
        # no extras were allowed ->  fail, and stop trying.
        pass <- FALSE
        keeptrying <- FALSE
        break
      }
      
      if (length(props) == 0) {
        # if no props specified, we're ok.
        pass <- TRUE
        break
      }
      
      stud_props <- stud_props[props]
      if (any(is.na(names(stud_props)))) {
        # not all properties in props are found in expression
        break
      }
      
      # check if property values correspond
      correct_val <- mapply(function(x,y) (x$value == y$value), x = sol_props, y = stud_props)
      # check if both properties were mapped or set!
      correct_mapset <- mapply(function(x,y) {
        if (is.null(x$scale)) {
          return(is.null(y$scale))
        }
        else {
          if (is.null(y$scale))
            return(FALSE)
          else
            return(x$scale == y$scale)
        }}, x = sol_props, y = stud_props)
      
      # if all passed, it's a pass
      if (all(correct_val) && all(correct_mapset)) {
        pass <- TRUE
        break
      }
    }
    
    if (!keeptrying) # if one expression contained more props, fail, and stop trying
      break
  }
  check_that(is_true(pass), feedback = incorrect_msg)
}


# Find all top-level ggvis calls
find_ggvis_pds <- function(pd) {
  top_ggvis_ids <- pd[pd$parent == 0 & grepl("ggvis\\(", pd$text), "id"]
  lapply(top_ggvis_ids, get_sub_pd, pd = pd)
}

get_all_props <- function(fun, call) {
  call[1] <- call("props")
  call <- call[-2]

  out <- try(eval(call), silent = TRUE)
  if(inherits(out, "try-error")) {
    return(NULL)
  }
  else {
    # tidy up names and return
    names(out) = gsub(".update","",names(out))
    return(out)
  }
}

props <- function(..., .props = NULL, inherit = TRUE, env = parent.frame()) {
  ggvis:::check_empty_args()
  args <- pluck(lazyeval::lazy_dots(...), "expr")
  all <- ggvis:::args_to_props(c(args, .props), env)
  structure(all, inherit = inherit, class = "ggvis_props")
}

pluck <- function(x, name) {
  lapply(x, `[[`, name)
}
