# 02_ai_code.R | Supplementary independent AI coder
# Reads coding_rubric.md. Human coding remains primary.
# Run after all 60 main responses are collected.

source("01_collect.R")

AI_CODER_MODEL <- "gpt-6.1-sol"
AI_CODER_EFFORT <- "high"
AI_OUTPUT_DIR <- file.path(OUTPUT_ROOT, "ai_coding")
RUBRIC_FILE <- "coding_rubric.md"

load_coder_rubric <- function() {
  if (!file.exists(RUBRIC_FILE)) {
    stop("Missing coding_rubric.md in the RStudio project directory.")
  }
  dir.create(AI_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  snapshot <- file.path(AI_OUTPUT_DIR, "rubric_used.md")
  settings_file <- file.path(AI_OUTPUT_DIR, "coder_settings.txt")
  rubric_lines <- readLines(RUBRIC_FILE, warn = FALSE, encoding = "UTF-8")
  settings <- c(paste0("model=", AI_CODER_MODEL),
                paste0("reasoning_effort=", AI_CODER_EFFORT))

  existing <- list.files(AI_OUTPUT_DIR,
                         pattern = "_(raw\\.json|code\\.csv)$",
                         full.names = TRUE)
  if (!file.exists(snapshot) && length(existing)) {
    stop("Existing AI-coding outputs have no rubric snapshot. ",
         "Archive/rename the old ai_coding folder before continuing.")
  }
  if (file.exists(snapshot)) {
    prior <- readLines(snapshot, warn = FALSE, encoding = "UTF-8")
    if (!identical(rubric_lines, prior)) {
      stop("coding_rubric.md differs from the rubric used for prior AI codes. ",
           "Do not combine coding versions.")
    }
  } else {
    if (!file.copy(RUBRIC_FILE, snapshot)) stop("Could not save rubric snapshot.")
  }
  if (file.exists(settings_file)) {
    if (!identical(readLines(settings_file, warn = FALSE), settings)) {
      stop("AI coder model/effort has changed. Do not mix coder settings.")
    }
  } else {
    if (length(existing)) stop("Missing settings snapshot for existing AI codes.")
    writeLines(settings, settings_file, useBytes = TRUE)
  }
  paste(rubric_lines, collapse = "\n")
}

# The AI gets each original-language text individually.
# It never receives human codes, language assignment, paired response, or hypothesis.
code_one_ai <- function(row, event, rubric) {
  prefix <- file.path(AI_OUTPUT_DIR,
                      sprintf("%s_slot%d", row$event_id, row$slot))
  raw_path <- paste0(prefix, "_raw.json")
  code_path <- paste0(prefix, "_code.csv")
  prompt_path <- paste0(prefix, "_coder_prompt.txt")
  if (file.exists(code_path)) return(invisible(NULL))

  response_path <- file.path(
    OUTPUT_ROOT, "main",
    sprintf("%s_slot%d_%s_text.txt", row$event_id, row$slot, row$language)
  )
  if (!file.exists(response_path)) stop("Missing response text: ", response_path)
  response_text <- paste(readLines(response_path, warn = FALSE,
                                   encoding = "UTF-8"), collapse = "\n")

  coder_prompt <- paste(
    "You are an independent research coder. Code exactly ONE response.",
    "Apply the coding rubric reproduced below exactly, using your judgment",
    "in borderline cases. Do not introduce stricter or alternative criteria.",
    "Assess the whole response and the narrator's own evaluative tone.",
    "Do not compare language versions or infer a treatment effect.",
    "Return ONLY a valid JSON object with exactly these keys:",
    "valence_category, rights_mentioned, state_responsibility, brief_evidence.",
    "valence_category: critical, favorable, mixed, neutral, refusal, or uncodable.",
    "Both secondary variables: integer 0 or 1, or null for refusal/uncodable.",
    "brief_evidence: a short explanation grounded in the provided response.",
    "Do not output markdown, comments, or text outside the JSON.",
    "",
    paste0("FOCAL STATE ACTION (English reference): ", event$action_en),
    paste0("FOCAL STATE ACTION (Turkish reference): ", event$action_tr),
    "",
    "CODING RUBRIC (verbatim):", rubric,
    "",
    "TEXT TO CODE:", response_text,
    sep = "\n"
  )

  if (file.exists(prompt_path)) {
    saved <- paste(readLines(prompt_path, warn = FALSE,
                             encoding = "UTF-8"), collapse = "\n")
    if (!identical(saved, coder_prompt)) {
      stop("Saved AI-coder prompt differs from current version: ", prompt_path)
    }
  } else if (file.exists(raw_path)) {
    stop("Found raw API output without its original coder prompt: ", raw_path)
  } else {
    writeLines(coder_prompt, prompt_path, useBytes = TRUE)
  }

  if (!file.exists(raw_path)) {
    res <- httr2::request("https://api.openai.com/v1/responses") |>
      httr2::req_auth_bearer_token(keyring::key_get(API_KEY_SERVICE)) |>
      httr2::req_timeout(240) |>
      httr2::req_body_json(list(
        model = AI_CODER_MODEL,
        reasoning = list(effort = AI_CODER_EFFORT),
        input = coder_prompt,
        store = FALSE
      )) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = FALSE)
    jsonlite::write_json(res, raw_path, pretty = TRUE, auto_unbox = TRUE)
  } else {
    res <- jsonlite::fromJSON(raw_path, simplifyVector = FALSE)
  }

  if (!identical(res$status, "completed")) {
    stop("AI coder API response did not complete; inspect ", raw_path)
  }
  txt <- extract_text(res)
  if (length(txt) != 1L || is.na(txt) || !nzchar(trimws(txt))) {
    stop("No AI-coder output text; inspect ", raw_path)
  }
  txt <- trimws(txt)
  txt <- sub("^```(?:json)?\\s*", "", txt, perl = TRUE)
  txt <- sub("\\s*```$", "", txt, perl = TRUE)
  code <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE),
                   error = function(e) stop("Invalid JSON; inspect ", raw_path))
  valid <- c("critical", "favorable", "mixed", "neutral",
             "refusal", "uncodable")
  category <- code$valence_category
  if (length(category) != 1L || is.na(category) || !(category %in% valid)) {
    stop("Invalid valence_category in ", raw_path)
  }

  parse_binary <- function(x) {
    if (is.null(x) || length(x) == 0L) return(NA_integer_)
    value <- suppressWarnings(as.numeric(x))
    if (length(value) != 1L || is.na(value) || !(value %in% c(0, 1))) {
      stop("Invalid secondary code in ", raw_path)
    }
    as.integer(value)
  }
  rights <- parse_binary(code$rights_mentioned)
  state <- parse_binary(code$state_responsibility)
  if (category %in% c("refusal", "uncodable")) {
    if (!is.na(rights) || !is.na(state)) {
      stop("Secondary codes must be null for refusal/uncodable: ", raw_path)
    }
  } else if (is.na(rights) || is.na(state)) {
    stop("Secondary codes missing for a codable response: ", raw_path)
  }

  evidence <- if (is.null(code$brief_evidence)) "" else
    paste(as.character(code$brief_evidence), collapse = " ")
  result <- data.frame(
    event_id = row$event_id,
    language = row$language,
    valence_category = category,
    rights_mentioned = rights,
    state_responsibility = state,
    brief_evidence = evidence,
    coder_model = AI_CODER_MODEL,
    stringsAsFactors = FALSE
  )
  write.csv(result, code_path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(result)
}

run_ai_coding <- function(batch_size = 10L) {
  if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1 ||
      batch_size != as.integer(batch_size)) stop("batch_size must be a positive integer")
  check_osf_registration()
  assignment <- get_main_assignment()
  events <- read_events("main")
  required <- file.path(
    OUTPUT_ROOT, "main",
    sprintf("%s_slot%d_%s_raw.json", assignment$event_id,
            assignment$slot, assignment$language)
  )
  if (nrow(assignment) != 60L || !all(file.exists(required))) {
    stop("All 60 main-study responses must exist before AI coding.")
  }
  rubric <- load_coder_rubric()
  set.seed(20261116L + 1L)
  call_order <- sample.int(nrow(assignment))
  n <- 0L

  for (i in call_order) {
    row <- assignment[i, , drop = FALSE]
    coded_path <- file.path(AI_OUTPUT_DIR,
                            sprintf("%s_slot%d_code.csv", row$event_id, row$slot))
    if (file.exists(coded_path)) next
    if (n >= batch_size) break
    event <- events[events$event_id == row$event_id, , drop = FALSE]
    if (nrow(event) != 1L) stop("Event lookup failed: ", row$event_id)
    code_one_ai(row, event, rubric)
    n <- n + 1L
  }

  files <- list.files(AI_OUTPUT_DIR, pattern = "_code\\.csv$", full.names = TRUE)
  if (length(files)) {
    codes <- do.call(rbind, lapply(files, function(f) {
      read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
    }))
    if (anyDuplicated(paste(codes$event_id, codes$language))) {
      stop("Duplicate AI coded event-language rows.")
    }
    codes <- codes[order(codes$event_id, codes$language), , drop = FALSE]
    write.csv(codes, "ai_coding.csv", row.names = FALSE, fileEncoding = "UTF-8")
  }
  message("AI-coded responses: ", length(files), "/60 (human coding is primary).")
  invisible(length(files))
}
