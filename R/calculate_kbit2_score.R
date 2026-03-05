#' @title calculate_kbit2_score
#' @description Calculates intelligence quotient and age equivalents from the
#'     KBIT-2 score from look-up tables using the age of the participant and
#'     their verbal and nonverbal scores.
#' @param age_at_visit Age of the participant at visit
#' @param kbit2verbkraw Verbal Knowledge Raw Score
#' @param kbit2ridraw Riddles Raw Score
#' @param kbit2vnonvraw Nonverbal Raw Score
#' @param subject_label A subject ID number is required if calculating multiple KBIT-2 scores, Default: NULL
#' @param add_premorbid_id `r lifecycle::badge("experimental")` Add a premorbid intellectual disability level using only the KBIT-2 score.
#'     This argument is currently set as FALSE for the default until the premorbid intellectual disability
#'     calculation is better understood, Default: FALSE
#' @param print_kbit Print the KBIT-2 results to the console if calculating a single KBIT-2 score, Default: TRUE
#' @param doParallel Use parallel processing to speed up the calculation of multiple KBIT-2 scores, Default: TRUE
#' @return A list containing the verbal and nonverbal standard scores, intelligent quotients,
#' @details Calculates intelligence quotient and age equivalents from the
#'     KBIT-2 score from look-up tables using the age of the participant and
#'     their verbal and nonverbal scores.
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}
#' @rdname calculate_kbit2_score
#' @export
#' @importFrom dplyr filter select bind_rows
#' @importFrom parallel detectCores makeCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach `%dopar%` foreach
#' @importFrom plyr rbind.fill

calculate_kbit2_score <- function(
  age_at_visit,
  kbit2verbkraw,
  kbit2ridraw,
  kbit2vnonvraw,
  subject_label = NULL,
  add_premorbid_id = FALSE,
  print_kbit = TRUE,
  doParallel = TRUE
) {
  files <- list.files(
    path = system.file("extdata", package = "abcds"),
    pattern = "kbit2",
    full.names = TRUE
  )

  for (f in files) {
    load(f)
  }

  if (length(kbit2verbkraw) > 1 & is.null(subject_label)) {
    stop(
      "A subject_label vector is required when calculating multiple KBIT-2 scores."
    )
  }

  main_kbit2_calculator <- function(
    age_at_visit,
    kbit2verbkraw,
    kbit2ridraw,
    kbit2vnonvraw,
    subject_label
  ) {
    # Calculate sum of standard scores
    kbit2verbraw <- kbit2verbkraw + kbit2ridraw

    verbstd <-
      verbal %>%
      dplyr::filter(
        min_age <= age_at_visit &
          max_age >= age_at_visit &
          raw_score == kbit2verbraw
      ) %>%
      dplyr::select(-c(min_age, max_age))

    kbit2verbstd <- verbstd$standard_score
    kbit2verbstdci <- verbstd$confidence_interval
    kbit2verbstdpr <- verbstd$percentile_rank

    nonvbstd <-
      nonverbal %>%
      dplyr::filter(
        min_age <= age_at_visit &
          max_age >= age_at_visit &
          raw_score == kbit2vnonvraw
      ) %>%
      dplyr::select(-c(min_age, max_age))

    kbit2nonvbstd <- nonvbstd$standard_score
    kbit2nonvbstdci <- nonvbstd$confidence_interval
    kbit2nonvbstdpr <- nonvbstd$percentile_rank

    sumstd <- kbit2verbstd + kbit2nonvbstd

    # Calculate IQ

    iqcomp <-
      expand_lookup(
        data = subset(iq, min_age <= age_at_visit & max_age >= age_at_visit),
        variable = "sum_of_standard_scores"
      ) %>%
      dplyr::filter(sum_of_standard_scores == sumstd) %>%
      dplyr::select(-c(min_age, max_age))

    kbit2iqcompstd <- iqcomp$standard_score
    kbit2iqcompci <- iqcomp$confidence_interval
    kbit2iqcomppr <- iqcomp$percentile_rank

    # Calculate age equivalent
    aenonvb <-
      ae %>%
      expand_lookup(variable = "nonverbal_raw_score") %>%
      dplyr::filter(nonverbal_raw_score == kbit2vnonvraw)

    kbit2nonvbaey <- strsplit(aenonvb$age_equivalent, ":")[[1]][1]
    kbit2nonvbae <- strsplit(aenonvb$age_equivalent, ":")[[1]][2]

    aeverb <-
      ae %>%
      expand_lookup(variable = "verbal_raw_score") %>%
      dplyr::filter(verbal_raw_score == kbit2verbraw)

    kbit2verbaey <- strsplit(aeverb$age_equivalent, ":")[[1]][1]
    kbit2verbae <- strsplit(aeverb$age_equivalent, ":")[[1]][2]

    data <-
      data.frame(
        age_at_visit,
        kbit2verbkraw,
        kbit2ridraw,
        kbit2vnonvraw,
        kbit2verbraw,
        kbit2verbstd,
        kbit2verbstdci,
        kbit2verbstdpr,
        kbit2nonvbstd,
        kbit2nonvbstdci,
        kbit2nonvbstdpr,
        kbit2iqcompstd,
        kbit2iqcompci,
        kbit2iqcomppr,
        kbit2nonvbaey,
        kbit2nonvbae,
        kbit2verbaey,
        kbit2verbae
      )

    if (!is.null(subject_label)) {
      data <- cbind(subject_label, data)
    }

    if (add_premorbid_id) {
      if (data$kbit2iqcompstd > 40) {
        data$prefunclevel <- ifelse(data$kbit2iqcompstd >= 50, 1, 2)
      } else if (data$kbit2iqcompstd == 40) {
        data$prefunclevel <-
          ifelse(
            aenonvb$age_equivalent[1] %in%
              c("< 4:0", "4:0") &
              aeverb$age_equivalent[1] %in% c("< 4:0", "4:0"),
            3,
            2
          )
      }
    }
    return(data)
  }

  if (doParallel & length(kbit2verbkraw) > 1) {
    cores = parallel::detectCores()
    Ncores = cores - 1
    cl = parallel::makeCluster(Ncores)
    doParallel::registerDoParallel(cl)
    `%dopar%` = foreach::`%dopar%`
    kbitres <-
      foreach::foreach(i = 1:length(kbit2verbkraw), .packages = "abcds") %dopar%
      {
        main_kbit2_calculator(
          age_at_visit[i],
          kbit2verbkraw[i],
          kbit2ridraw[i],
          kbit2vnonvraw[i],
          subject_label[i]
        )
      } %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(subject_label)
  } else if (!doParallel & length(kbit2verbkraw) > 1) {
    kbitres <- data.frame()
    for (i in 1:length(kbit2verbkraw)) {
      cat("\r Processing data for ", subject_label[i])
      data <- main_kbit2_calculator(
        age_at_visit[i],
        kbit2verbkraw[i],
        kbit2ridraw[i],
        kbit2vnonvraw[i],
        subject_label[i]
      )
      kbitres <- plyr::rbind.fill(kbitres, data)
    }
    kbitres <- dplyr::arrange(kbitres, subject_label)
  } else if (length(kbit2verbkraw) == 1) {
    kbitres <- main_kbit2_calculator(
      age_at_visit,
      kbit2verbkraw,
      kbit2ridraw,
      kbit2vnonvraw,
      subject_label
    )
    kbitres$subject_label <- NULL
    if (print_kbit) print_kbit2_results(kbitres)
  } else {
    stop("An error occurred. Please check your data and try again.")
  }

  class(kbitres) <- c("tbl_df", "tbl", "data.frame")

  return(kbitres)
}

#' @title print_kbit2_results
#' @description Prints out the KBIT-2 results from the `calculate_kbit2_score` function.
#' @param kbitres Results from the `calculate_kbit2_score` function
#' @return NULL
#' @details Prints out the KBIT-2 results from the `calculate_kbit2_score` function.
#' @rdname print_kbit2_results
#' @export

print_kbit2_results <- function(kbitres) {
  message(
    sprintf(
      "KBIT-2 results for a %i-year old participant with a verbal score of %i and nonverbal score of %i.",
      kbitres$age_at_visit,
      kbitres$kbit2verbraw,
      kbitres$kbit2vnonvraw
    ),
    "\n\n",
    sprintf(
      "Verbal Standard Score: %i (90%% CI: %s); Percentile Rank: %s",
      kbitres$kbit2verbstd,
      kbitres$kbit2verbstdci,
      kbitres$kbit2verbstdpr
    ),
    "\n",
    sprintf(
      "Nonverbal Standard Score: %i (90%% CI: %s); Percentile Rank: %s",
      kbitres$kbit2nonvbstd,
      kbitres$kbit2nonvbstdci,
      kbitres$kbit2nonvbstdpr
    ),
    "\n",
    sprintf(
      "Intelligent Quotient %i (90%% CI: %s); Percentile Rank: %s",
      kbitres$kbit2iqcompstd,
      kbitres$kbit2iqcompci,
      kbitres$kbit2iqcomppr
    ),
    "\n\n",
    sprintf(
      "Verbal Age Equivalent: %s:%s; Nonverbal Age Equivalent: %s:%s",
      kbitres$kbit2verbaey,
      kbitres$kbit2verbae,
      kbitres$kbit2nonvbaey,
      kbitres$kbit2nonvbae
    )
  )
}
