#' @title FSRS Rating
#' @description Rating values for card reviews
#' @export
Rating <- list(
  Again = 1L,
  Hard = 2L,
  Good = 3L,
  Easy = 4L
)

#' @title FSRS State
#' @description Card states in the FSRS system
#' @export
State <- list(
  New = 0L,
  Learning = 1L,
  Review = 2L,
  Relearning = 3L
)

#' @title FSRS Card
#' @description R6 class representing a flashcard
#' @export
Card <- R6::R6Class(
  "Card",
  public = list(
    due = NULL,
    stability = NULL,
    difficulty = NULL,
    elapsed_days = NULL,
    scheduled_days = NULL,
    reps = NULL,
    lapses = NULL,
    state = NULL,
    last_review = NULL,
    initialize = function(due = Sys.time()) {
      self$due <- due
      self$stability <- 0
      self$difficulty <- 0
      self$elapsed_days <- 0
      self$scheduled_days <- 0
      self$reps <- 0L
      self$lapses <- 0L
      self$state <- State$New
      self$last_review <- NULL
    },
    get_retrievability = function() {
      if (self$state == State$New || is.null(self$stability) || self$stability == 0) return(1.0)
      elapsed <- as.numeric(difftime(Sys.time(), self$last_review, units = "days"))
      fsrs_retrievability(self$stability, elapsed)
    },
    to_json = function() {
      jsonlite::toJSON(list(
        due = format(self$due, "%Y-%m-%dT%H:%M:%S%z"),
        stability = self$stability, difficulty = self$difficulty,
        elapsed_days = self$elapsed_days, scheduled_days = self$scheduled_days,
        reps = self$reps, lapses = self$lapses, state = self$state,
        last_review = if (!is.null(self$last_review)) format(self$last_review, "%Y-%m-%dT%H:%M:%S%z") else NULL
      ), auto_unbox = TRUE)
    },
    print = function() {
      state_name <- names(State)[which(unlist(State) == self$state)]
      cat("FSRS Card\n")
      cat("  State:", state_name, "\n")
      cat("  Due:", format(self$due), "\n")
      cat("  Stability:", round(self$stability, 2), "days\n")
      cat("  Difficulty:", round(self$difficulty, 2), "\n")
      cat("  Reps:", self$reps, "\n")
      cat("  Lapses:", self$lapses, "\n")
      invisible(self)
    }
  )
)

#' @title Create Card from JSON
#' @param json JSON string
#' @return Card object
#' @export
Card_from_json <- function(json) {
  data <- jsonlite::fromJSON(json)
  card <- Card$new()
  card$due <- as.POSIXct(data$due, format = "%Y-%m-%dT%H:%M:%S%z")
  card$stability <- data$stability
  card$difficulty <- data$difficulty
  card$elapsed_days <- data$elapsed_days
  card$scheduled_days <- data$scheduled_days
  card$reps <- as.integer(data$reps)
  card$lapses <- as.integer(data$lapses)
  card$state <- as.integer(data$state)
  if (!is.null(data$last_review)) card$last_review <- as.POSIXct(data$last_review, format = "%Y-%m-%dT%H:%M:%S%z")
  card
}

#' @title FSRS Review Log
#' @description R6 class representing a review log entry
#' @export
ReviewLog <- R6::R6Class(
  "ReviewLog",
  public = list(
    rating = NULL, scheduled_days = NULL, elapsed_days = NULL, review_datetime = NULL, state = NULL,
    initialize = function(rating, scheduled_days, elapsed_days, review_datetime, state) {
      self$rating <- rating
      self$scheduled_days <- scheduled_days
      self$elapsed_days <- elapsed_days
      self$review_datetime <- review_datetime
      self$state <- state
    },
    to_json = function() {
      jsonlite::toJSON(list(
        rating = self$rating, scheduled_days = self$scheduled_days, elapsed_days = self$elapsed_days,
        review_datetime = format(self$review_datetime, "%Y-%m-%dT%H:%M:%S%z"), state = self$state
      ), auto_unbox = TRUE)
    },
    print = function() {
      rating_name <- names(Rating)[which(unlist(Rating) == self$rating)]
      cat("FSRS ReviewLog\n")
      cat("  Rating:", rating_name, "\n")
      cat("  Review time:", format(self$review_datetime), "\n")
      cat("  Scheduled days:", self$scheduled_days, "\n")
      invisible(self)
    }
  )
)

#' @title FSRS Scheduler
#' @description R6 class for scheduling card reviews using FSRS algorithm
#' @export
Scheduler <- R6::R6Class(
  "Scheduler",
  public = list(
    parameters = NULL, desired_retention = NULL, maximum_interval = NULL, enable_fuzzing = NULL,
    initialize = function(parameters = NULL, desired_retention = 0.9, maximum_interval = 36500L, enable_fuzzing = FALSE) {
      self$parameters <- if (is.null(parameters)) fsrs_default_parameters() else parameters
      self$desired_retention <- desired_retention
      self$maximum_interval <- maximum_interval
      self$enable_fuzzing <- enable_fuzzing
    },
    review_card = function(card, rating, review_datetime = Sys.time()) {
      if (!rating %in% 1:4) stop("Rating must be 1 (Again), 2 (Hard), 3 (Good), or 4 (Easy)")
      elapsed_days <- if (is.null(card$last_review) || card$state == State$New) 0 else as.numeric(difftime(review_datetime, card$last_review, units = "days"))
      review_log <- ReviewLog$new(rating = rating, scheduled_days = card$scheduled_days, elapsed_days = elapsed_days, review_datetime = review_datetime, state = card$state)
      if (card$state == State$New) {
        new_state <- fsrs_initial_state(rating)
        card$stability <- new_state$stability
        card$difficulty <- new_state$difficulty
        card$state <- if (rating == Rating$Again) State$Learning else State$Review
      } else {
        new_state <- fsrs_next_state(card$stability, card$difficulty, elapsed_days, rating)
        card$stability <- new_state$stability
        card$difficulty <- new_state$difficulty
        if (rating == Rating$Again) { card$lapses <- card$lapses + 1L; card$state <- State$Relearning } else { card$state <- State$Review }
      }
      interval <- fsrs_next_interval(card$stability, self$desired_retention)
      interval <- min(interval, self$maximum_interval)
      if (self$enable_fuzzing && interval > 2) { fuzz_range <- max(1, round(interval * 0.05)); interval <- interval + sample(-fuzz_range:fuzz_range, 1) }
      card$scheduled_days <- round(interval)
      card$elapsed_days <- elapsed_days
      card$reps <- card$reps + 1L
      card$last_review <- review_datetime
      card$due <- review_datetime + as.difftime(card$scheduled_days, units = "days")
      list(card = card, review_log = review_log)
    },
    get_card_retrievability = function(card) { card$get_retrievability() },
    to_json = function() {
      jsonlite::toJSON(list(parameters = self$parameters, desired_retention = self$desired_retention, maximum_interval = self$maximum_interval, enable_fuzzing = self$enable_fuzzing), auto_unbox = TRUE)
    },
    print = function() {
      cat("FSRS Scheduler\n")
      cat("  Desired retention:", self$desired_retention, "\n")
      cat("  Maximum interval:", self$maximum_interval, "days\n")
      cat("  Fuzzing:", if (self$enable_fuzzing) "enabled" else "disabled", "\n")
      invisible(self)
    }
  )
)

#' @title Create Scheduler from JSON
#' @param json JSON string
#' @return Scheduler object
#' @export
Scheduler_from_json <- function(json) {
  data <- jsonlite::fromJSON(json)
  Scheduler$new(parameters = data$parameters, desired_retention = data$desired_retention, maximum_interval = data$maximum_interval, enable_fuzzing = data$enable_fuzzing)
}
