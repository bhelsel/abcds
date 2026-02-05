#' Create ABCDS data reader functions
#'
#' This is a function factory that creates reader functions for ABCDS study data files.
#' It is used internally to generate the various `read_*` functions in this package.
#' The factory returns a function that reads CSV files matching a specified pattern,
#' optionally adds demographics, and applies custom post-processing.
#'
#' @param pattern Character string. Regular expression pattern to match file names.
#'   Uses Perl-compatible regex (PCRE).
#' @param default_cols Column specification object created by [readr::cols()].
#'   If NULL, readr will guess column types based on the first 2000 rows.
#' @param include_demographics Logical. If TRUE, merges demographic data
#'   (age, gender, race, ethnicity) with the returned data. Default is TRUE.
#' @param .f Function. Optional post-processing function applied to the data
#'   after reading and merging demographics. Should accept a data frame and
#'   return a modified data frame. Default is NULL (no post-processing).
#'
#' @return A function with the signature:
#'   `function(directory, controls, add_demographics, col_types, ...)`
#'
#'   The returned function accepts:
#'   \describe{
#'     \item{directory}{Character string. Path to the directory containing data files.
#'       If NULL, uses the path set by [set_abcds_directory()].}
#'     \item{controls}{Logical. If TRUE, reads control data; if FALSE, reads
#'       participant data. Default is FALSE.}
#'     \item{add_demographics}{Logical. If TRUE, merges demographic data
#'       (age, gender, race, ethnicity) with the returned data. Default is TRUE.}
#'     \item{col_types}{Column specification for [readr::read_csv()]. Defaults to
#'       the `default_cols` specified in the factory.}
#'     \item{...}{Additional arguments passed to [readr::read_csv()].}
#'   }
#'
#' @details
#' The factory creates a reader function that:
#' \enumerate{
#'   \item Validates the directory path
#'   \item Finds files matching the pattern
#'   \item Ensures only one file matches (aborts if multiple files found)
#'   \item Reads the CSV file with readr
#'   \item Removes the `update_stamp` column if present
#'   \item Optionally merges demographics based on `subject_label` and `event_sequence`
#'   \item Applies custom post-processing function if provided
#' }
#'
#' When `add_demographics = TRUE`, the function merges age at event and simplified
#' demographics. If the data contains an `event_sequence` column, it joins on both
#' `subject_label` and `event_sequence`; otherwise it joins only on `subject_label`
#' using baseline (event_sequence == 1) demographics.
#'
#' @seealso
#'  [cli::cli_abort()] for error messages
#'  [readr::read_csv()] for reading CSV files
#'  [dplyr::right_join()] for merging demographics
#'
#' @keywords internal
#' @noRd
#' @importFrom cli cli_abort
#' @importFrom readr read_csv
#' @importFrom dplyr right_join

new_abcds_reader <- function(
  pattern,
  default_cols = NULL,
  .f = NULL,
  include_demographics = TRUE
) {
  force(pattern)
  force(default_cols)
  force(.f)
  force(include_demographics)

  function(
    directory = NULL,
    controls = FALSE,
    add_demographics = include_demographics,
    col_types = default_cols
  ) {
    directory <- check_abcds_directory(directory)

    files <- list.files(directory, full.names = TRUE)

    files <- files[grepl(pattern, basename(files), perl = TRUE)]

    check_single_file <- function(file) {
      if (length(file) > 1) {
        cli::cli_abort(c(
          "Multiple files found matching pattern {.val {pattern}}.",
          "i" = "Found {length(files)} file{?s}: {.file {basename(files)}}",
          "i" = "Please use a more specific pattern or check your directory."
        ))
      } else {
        return(file)
      }
    }

    person_types <- unname(sapply(files, .detect_person))

    if (!controls & "participant" %in% person_types) {
      file <- check_single_file(files[!grepl("Controls", files)])
    } else if (controls & "control" %in% person_types) {
      file <- check_single_file(files[grepl("Controls", files)])
    }

    data <- readr::read_csv(
      file,
      col_types = col_types,
      guess_max = 2000,
      show_col_types = FALSE
    )
    if ("update_stamp" %in% colnames(data)) {
      data$update_stamp <- NULL
    }

    if (add_demographics) {
      demo <- dplyr::right_join(
        read_age_at_event(
          directory,
          controls = controls,
          add_demographics = FALSE
        ),
        read_simplified_demographics(
          directory,
          controls = controls,
          add_demographics = FALSE
        ),
        by = c("subject_label", "event_sequence")
      )
      if ("event_sequence" %in% colnames(data)) {
        data <- dplyr::right_join(
          demo,
          data,
          by = c("subject_label", "event_sequence")
        )
      } else {
        demo <- demo[demo$event_sequence == 1, ]
        data <- dplyr::right_join(demo, data, by = "subject_label")
      }
    }

    if (!is.null(.f)) {
      data <- .f(data)
    }

    return(dplyr::arrange(data, subject_label, event_sequence))
  }
}

#' Create grouped ABCDS data reader functions
#'
#' This is a function factory that creates grouped reader functions for related
#' ABCDS study data types. It returns a function that accepts a type argument
#' (quoted or unquoted) and routes to the appropriate reader.
#'
#' @param readers Named list of reader functions created by [new_abcds_reader()].
#'   Names should be the valid type options (e.g., "mri", "amyloid").
#'
#' @return A function with the signature:
#'   `function(type, directory, controls, add_demographics)`
#'
#'   The returned function accepts:
#'   \describe{
#'     \item{type}{Character string or unquoted name. The type of data to read.
#'       Must be one of the names in the `readers` list.}
#'     \item{directory}{Character string. Path to the directory containing data files.
#'       If NULL, uses the path set by [set_abcds_directory()].}
#'     \item{controls}{Logical. If TRUE, reads control data; if FALSE, reads
#'       participant data. Default is FALSE.}
#'     \item{add_demographics}{Logical. If TRUE, merges demographic data
#'       with the returned data. Default is TRUE.}
#'   }
#'
#' @details
#' This factory is used internally to create grouped reader functions like
#' [read_imaging()], [read_biomarkers()], and [read_clinical()]. It allows
#' users to specify the data type either quoted (`"mri"`) or unquoted (`mri`)
#' using non-standard evaluation via [rlang::ensym()].
#'
#' @seealso
#'  [new_abcds_reader()] for creating individual reader functions
#'  [rlang::ensym()] for symbol capture
#'  [cli::cli_abort()] for error messages
#'
#' @keywords internal
#' @noRd
#' @importFrom rlang ensym
#' @importFrom cli cli_abort

new_abcds_grouped_reader <- function(readers) {
  force(readers)
  function(
    type,
    directory = NULL,
    controls = FALSE,
    add_demographics = TRUE
  ) {
    type <- as.character(rlang::ensym(type))
    if (!type %in% names(readers)) {
      cli::cli_abort(c(
        "Invalid type: {.val {type}}",
        "i" = "Must be one of: {.val {names(readers)}}"
      ))
    }

    readers[[type]](
      directory = directory,
      controls = controls,
      add_demographics = add_demographics
    )
  }
}

#' Read ApoE genotype data
#'
#' Reads ApoE genotype data files from the ABCDS study.
#'
#' @inheritParams read_demographics
#'
#' @return A tibble containing ApoE genotype data, optionally merged with
#'   demographics.
#'
#' @export
#' @examples
#' \dontrun{
#' # Read participant ApoE data with demographics
#' apoe_data <- read_apoe()
#'
#' # Read control data without demographics
#' apoe_controls <- read_apoe(controls = TRUE, add_demographics = FALSE)
#'
#' # Specify custom directory
#' apoe_data <- read_apoe(directory = "path/to/data")
#' }

read_apoe <- new_abcds_reader("ApoE")

#' Read health history data
#'
#' Reads health history data files from the ABCDS study, excluding
#' follow-up and medication worksheet files.
#'
#' @inheritParams read_demographics
#'
#' @return A tibble containing health history data, optionally merged with
#'   demographics.
#'
#' @export
#' @examples
#' \dontrun{
#' # Read participant health history
#' health <- read_health_history()
#'
#' # Read controls only
#' health_controls <- read_health_history(controls = TRUE)
#' }

read_health_history <- new_abcds_reader("^Health_History(?!_Followup)")

#' Read age at event data
#'
#' Reads age at event data from the ABCDS study and returns a simplified
#' dataset containing only subject label, event sequence, and age at visit.
#'
#' @inheritParams read_demographics
#'
#' @return A tibble containing subject_label, event_sequence, and age_at_visit
#'   columns.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get age data for participants
#' ages <- read_age_at_event()
#' }

read_age_at_event <- new_abcds_reader(
  "Age_at_Event",
  include_demographics = FALSE,
  .f = function(data) {
    data[, c("subject_label", "event_sequence", "age_at_visit")]
  }
)

#' Read demographics data
#'
#' Reads complete demographics data files from the ABCDS study.
#'
#' @param directory Character string. Path to the directory containing data files.
#'   If NULL, uses the path set by [set_abcds_directory()]. Default is NULL.
#' @param controls Logical. If TRUE, reads control data; if FALSE, reads
#'   participant data. Default is FALSE.
#' @param add_demographics Logical. If TRUE, merges demographic data
#'   (age, gender, race, ethnicity) with the returned data. Default is TRUE.
#' @param col_types Column specification for [readr::read_csv()]. If NULL,
#'   column types will be guessed.
#'
#' @return A tibble containing demographics data.
#'
#' @export
#' @examples
#' \dontrun{
#' # Read all participant demographics
#' demographics <- read_demographics()
#'
#' # Read control demographics without additional demographics merge
#' demo_controls <- read_demographics(controls = TRUE, add_demographics = FALSE)
#' }

read_demographics <- new_abcds_reader(
  "Demographics",
  include_demographics = FALSE
)

#' Read simplified demographics data
#'
#' Reads demographics data from the ABCDS study and returns a simplified
#' dataset containing only baseline (event_sequence == 1) data with
#' subject identifier and core demographic variables.
#'
#' @inheritParams read_demographics
#'
#' @return A tibble containing subject_label, de_gender, de_race, and
#'   de_ethnicity columns for baseline visits only.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get simplified baseline demographics for participants
#' simple_demo <- read_simplified_demographics()
#' }

read_simplified_demographics <- new_abcds_reader(
  "Demographics",
  include_demographics = FALSE,
  .f = function(data) {
    data[
      data$event_sequence == 1,
      c(
        "subject_label",
        "event_sequence",
        "de_gender",
        "de_race",
        "de_ethnicity"
      )
    ]
  }
)

#' Read imaging data from ABCDS study
#'
#' Reads various imaging modality data files including MRI and PET scans
#' from the ABCDS study.
#'
#' @param type Character string or unquoted name. Type of imaging data to read.
#'   Options: `mri`, `amyloid`, `fdg`, or `tau`.
#' @param directory Character string. Path to the directory containing data files.
#'   If NULL, uses the path set by [set_abcds_directory()]. Default is NULL.
#' @param controls Logical. If TRUE, reads control data; if FALSE, reads
#'   participant data. Default is FALSE.
#' @param add_demographics Logical. If TRUE, merges demographic data
#'   (age, gender, race, ethnicity) with the returned data. Default is TRUE.
#'
#' @return A tibble containing the requested imaging data, optionally merged
#'   with demographics.
#'
#' @export
#' @examples
#' \dontrun{
#' # Read MRI data (quoted)
#' mri_data <- read_imaging("mri")
#'
#' # Read amyloid PET data (unquoted)
#' amyloid_data <- read_imaging(amyloid)
#'
#' # Read tau PET for controls without demographics
#' tau_controls <- read_imaging(tau, controls = TRUE, add_demographics = FALSE)
#'
#' # Specify custom directory
#' fdg_data <- read_imaging(fdg, directory = "path/to/data")
#' }

read_imaging <- new_abcds_grouped_reader(
  list(
    mri = new_abcds_reader("MRI_Scan"),
    amyloid = new_abcds_reader("Amyloid_PET_Scan"),
    fdg = new_abcds_reader("FDG_PET_Scan"),
    tau = new_abcds_reader("Tau_PET_Scan")
  )
)

#' Read medication data
#'
#' Reads medication data files from the ABCDS study
#'
#' @inheritParams read_demographics
#'
#' @return A tibble containing medication data, optionally merged with
#'   demographics.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get medication data for participants
#' medications <- read_medications()
#' }

read_medications <- new_abcds_reader("Medications_Health_History_Worksheet")

#' Read physical and neurological exam data
#'
#' Reads physical and neurological exam data files from the ABCDS study
#'
#' @inheritParams read_demographics
#'
#' @return A tibble containing physical and neurological exam data, optionally merged with
#'   demographics.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get physical and neurological exam data for participants
#' neuro_exam <- read_neuro_exam()
#' }

read_neuro_exam <- new_abcds_reader("Physical_and_Neurological")


#' Read anthropometric data
#'
#' Reads height and weight data from the physical exam data files from the ABCDS study
#' and converts height and weight to metric before calculating and returning body mass index.
#'
#' @inheritParams read_demographics
#'
#' @return A tibble containing height, weight, and body mass index optionally merged with
#'   demographics.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get height, weight, and body mass index data for participants
#' anthro_data <- read_athropometrics()
#' }

read_athropometrics <- new_abcds_reader(
  pattern = "Physical_and_Neurological",
  .f = function(data) {
    demovars <- c("age_at_event", "de_gender", "de_race", "de_ethnicity")
    data <- tidyr::fill(data, ht, htu, .direction = "down")
    data$ht <- ifelse(data$htu == 1, data$ht * 2.54, data$ht)
    data$wt <- ifelse(data$wtu == 1, data$wt / 2.205, data$wt)
    data$bmi <- data$wt / (data$ht / 100)^2
    demovars <- demovars[demovars %in% names(data)]
    data[, c("subject_label", "event_sequence", demovars, "ht", "wt", "bmi")]
  }
)

#' @title read_cognitive
#' @description Reads the files containing the cognitive function results from the directory
#'   for the participants and controls. The cognitive function assessments are identified as those
#'   starting with `cog` using the `dd_crf_name` variable in the data dictionary.
#' @param directory A path name containing the downloaded ABC-DS data from the
#'   University of South Carolina Laboratory of Neuro Imaging's (LONI) Image
#'   and Data Archive
#' @param person The specific group for which to read in the data, Default: c("participants", "controls")
#' @param include_demographics An optional parameter to merge demographic information
#'   before returning the data, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details Reads the files containing the cognitive function results from the
#'   directory for the participants and controls. The cognitive function
#'   assessments are identified as those starting with `cog` using the
#'   `dd_crf_name` variable in the data dictionary. End users also have an
#'   option to include the demographics with the `include_demographics` argument.
#' @rdname read_cognitive
#' @export

read_cognitive <- new_abcds_reader("cog")

load(system.file("extdata/data_dictionary.RData", package = "abcds"))

cog_labels <- gsub(
    "\u2013| |:|/",
    "_",
    unique(data_dictionary[
      startsWith(data_dictionary$dd_crf_name, "cog"),
      "dd_crf_label"
    ])
  )

  if ("participants" %in% person) {
    participants <- data.frame()
    for (i in cog_labels[!grepl("Montreal", cog_labels)]) {
      file <- list.files(directory, pattern = i, full.names = TRUE)
      if (length(file) != 0) {
        temp_df <- utils::read.csv(file)
      }
      if ("update_stamp" %in% colnames(temp_df)) {
        temp_df$update_stamp <- NULL
      }
      if (all(dim(participants) == 0)) {
        participants <- temp_df
      } else {
        participants <- merge(
          participants,
          temp_df,
          by = c(
            "subject_label",
            "event_sequence",
            "language_code",
            "language_label"
          ),
          all = TRUE
        )
      }
    }
    if (include_demographics) {
      participants <- merge(
        read_demographics(directory),
        participants,
        by = c(
          "subject_label",
          "event_sequence",
          "language_code",
          "language_label"
        )
      )
    }
    participants <- add_latency(
      directory,
      data = participants,
      person = "participants",
      visit = "clinical"
    )
    participants <- participants[
      order(participants$subject_label, participants$event_sequence),
    ]
    class(participants) <- c("tbl_df", "tbl", "data.frame")
  }

  if ("controls" %in% person) {
    controls <- utils::read.csv(
      list.files(
        directory,
        pattern = cog_labels[grepl("Montreal", cog_labels)],
        full.names = TRUE
      )
    )
    if ("update_stamp" %in% colnames(controls)) {
      controls$update_stamp <- NULL
    }
    if (include_demographics) {
      controls <- merge(
        read_demographics(
          directory,
          controls = TRUE
        ),
        controls,
        by = c(
          "subject_label",
          "event_sequence",
          "language_code",
          "language_label"
        )
      )
    }
    controls <- add_latency(
      directory,
      data = controls,
      person = "controls",
      visit = "clinical"
    )
    controls <- controls[
      order(controls$subject_label, controls$event_sequence),
    ]
    class(controls) <- c("tbl_df", "tbl", "data.frame")
  }

  if (
    exists("participants", inherits = FALSE) &
      exists("controls", inherits = FALSE)
  ) {
    return(list(participants = participants, controls = controls))
  } else if (exists("participants", inherits = FALSE)) {
    return(participants)
  } else if (exists("controls", inherits = FALSE)) {
    return(controls)
  } else {
    stop("Did not find any cognitive files")
  }
}
