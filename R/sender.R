#' @title Submit forecast objects for STAT 463
#' @description This function performs check on the format of submission and subsequently submits the forecast object.
#' @param group An \code{integer} representing your group number (should be between 1 and 17).
#' @param prediction A \code{list} containing 7 elements representing your forecasts (refer to project description on Piazza).
#' @param from A \code{string} representing your group email address in the correct format.
#' @param key A \code{string} representing the unique key assigned to your group.
send_prediction = function(group, prediction, from, key){
  # Email receiver
  to = "psu.forecasting.instructors@gmail.com"

  # Test email
  if(group == 0 || from == "psu.forecasting.group.0@gmail.com") {
    warning("The entered email address is a test email address and the corresponding forecasts will not be evaluated.")
  }

  # Current date
  date = Sys.Date()

  if(!("gmailr" %in% installed.packages())){
    stop("Please install the package gmailr")
  }

  # Prior verifications
  if(!is.list(prediction)){
    stop("Your forecast is not a list, please correct and retry.")
  }

  if (length(prediction) != 7){
    stop("Your forecast doesn't have the right number of elements, please correct and retry.")
  }

  if(!is.numeric(group)){
    stop("Your group number is not a number, please enter a number and retry.")
  }

  if (group < 1 || group > 17){
    stop("Your group number is not between 1 and 17, please correct and retry.")
  }

  if (!(group%%1==0)){
    stop("Your group number is not an interger, please correct and retry.")
  }

  assumed_names = c('mobile','desktop','silvio','beyonce','chomsky','lazio','thanks')
  if (sum(names(prediction) == assumed_names) != 7){
    stop("Your forecast doesn't have the right structure. Either the number of elements is incorrect or their names don't correspond to the required names. It should contain the elements in the following order: mobile, desktop, silvio, beyonce, chomsky, lazio, thanks. Please correct and retry.")
  }

  for (i in 1:length(assumed_names)){
    current_element = prediction[[i]]

    if (is.null(current_element[[1]]) || is.na(current_element[[1]]) || length(current_element[[1]]) != 1){
      message = paste("The point predicition for ", assumed_names[i], " is not a scalar/number. Please correct and retry.", sep = "")
      stop(message)
    }

    if (sum(is.null(current_element[[2]])) > 0 || sum(is.na(current_element[[2]])) > 0 || length(current_element[[2]]) != 2){
      message = paste("The CI for ", assumed_names[i], " is not a vector of size 2 with valid numbers. Please correct and retry.", sep ="")
      stop(message)
    }
  }

  # Check sender
  info_from = strsplit(from, "@")
  if (info_from[[1]][2] != "gmail.com"){
    stop("Please use the gmail address you created for this class. Please correct and retry.")
  }

  info_from2 = strsplit(info_from[[1]][1],"\\.")

  if (as.numeric(info_from2[[1]][4]) != group){
    stop(paste("The gmail address you are using doesn't match your group number. It should be: psu.forecasting.group.",group,"@gmail.com. Please correct and retry.", sep = ""))
  }

  if (sum(info_from2[[1]][1:3] != c("psu", "forecasting", "group")) > 0) {
    stop(paste("The gmail address doesn't appear to be correctly formatted. It should be: psu.forecasting.group.",group,"@gmail.com. Please correct and retry.", sep = ""))
  }

  if (is.null(key) || is.na(key)){
    stop("Please supply a valid key. It should contain 15 elements. Please correct and retry.")
  }

  if (nchar(key) != 15){
    stop("Your key is not of the right length. It should contain 15 elements. Please correct and retry.")
  }

  # Send prediction
  gmailr::send_message(gmailr::mime(
    To = to,
    From = from,
    Subject = paste("[STAT 463] Group ", group, sep = ""),
    body = paste(key, date, paste(unlist(prediction), collapse = ","), sep = ";")))

  # Send confirmation
  gmailr::send_message(gmailr::mime(
    To = from,
    From = from,
    Subject = paste("[STAT 463] Group ", group, ": Your submission appears to be successful.", sep = ""),
    body = paste("Your have submitted the following information:", key, date, paste(unlist(prediction), collapse = ","), sep = ";")))
}
