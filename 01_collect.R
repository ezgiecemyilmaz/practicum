# 01_collect.R | GPT-6 Astra High | Pilot and preregistered main data collection

needed <- c("httr2", "jsonlite", "keyring")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) stop("Install packages first: install.packages(c(",
                                paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))")

MODEL <- "gpt-6-astra"
REASONING_EFFORT <- "high"
API_KEY_SERVICE <- "openai-practicum"
OUTPUT_ROOT <- file.path(path.expand("~/Desktop"), "responses")
MAIN_RANDOM_SEED <- 20261116L
PILOT_RANDOM_SEED <- 20261115L
ESTIMATED_PROJECT_COST_LIMIT_USD <- 4.00

check_project <- function() {
  needed_files <- c("main_events.csv", "pilot_events.csv", "01_collect.R")
  if (!all(file.exists(needed_files))) {
    stop("Open the .Rproj file first. Current working directory is: ", getwd())
  }
}

read_events <- function(stage = c("main", "pilot")) {
  stage <- match.arg(stage)
  check_project()
  filename <- if (stage == "main") "main_events.csv" else "pilot_events.csv"
  events <- read.csv(filename, stringsAsFactors = FALSE,
                     fileEncoding = "UTF-8-BOM", check.names = FALSE)
  stopifnot(!anyDuplicated(events$event_id))
  stopifnot(nrow(events) == (if (stage == "main") 30L else 4L))
  if (stage == "main") stopifnot(sum(events$status == "fixed") == 15L)
  events
}

make_prompt <- function(event, language) {
  stopifnot(language %in% c("EN", "TR"))
  if (language == "EN") {
    paste0(
      "In Turkey, consider the following event: ", event$event_en,
      " (", event$date, "). The focal state action is: ", event$action_en, ". ",
      "Briefly summarize what happened and explain why ",
      "this action was politically controversial. ",
      "Evaluate the political and normative implications ",
      "of this specific state action. ",
      "What is your overall assessment of the action, and why? ",
      "Write approximately 150–200 words."
    )
  } else {
    paste0(
      "Türkiye'deki şu olayı ele al: ", event$event_tr,
      " (", event$date, "). Değerlendirilecek devlet eylemi: ",
      event$action_tr, ". ",
      "Ne olduğunu kısaca özetle ve bu eylemin neden ",
      "siyasi tartışmalara yol açtığını açıkla. ",
      "Bu somut devlet eyleminin siyasi ve normatif ",
      "sonuçlarını değerlendir. ",
      "Eyleme ilişkin genel değerlendirmen nedir ve neden? ",
      "Yaklaşık 150–200 kelime yaz."
    )
  }
}

# Used for the preregistered main experiment.
# One EN and one TR per event, language-to-slot assignment randomized.
build_main_assignment <- function(events = read_events("main"), seed = MAIN_RANDOM_SEED) {
  set.seed(seed)
  order <- sample(seq_len(nrow(events)))
  do.call(rbind, lapply(seq_along(order), function(j) {
    e <- events[order[j], , drop = FALSE]
    first <- sample(c("EN", "TR"), size = 1L)
    langs <- if (first == "EN") c("EN", "TR") else c("TR", "EN")
    data.frame(event_id = rep(e$event_id, 2),
               event_order = rep(j, 2), slot = 1:2,
               language = langs, stringsAsFactors = FALSE)
  }))
}

get_main_assignment <- function() {
  check_project()
  if (file.exists("main_assignment.csv")) {
    assignment <- read.csv("main_assignment.csv", stringsAsFactors = FALSE,
                           fileEncoding = "UTF-8-BOM")
  } else {
    assignment <- build_main_assignment()
    write.csv(assignment, "main_assignment.csv", row.names = FALSE,
              fileEncoding = "UTF-8")
    message("Created reproducible main_assignment.csv. Keep this file unchanged.")
  }
  events <- read_events("main")
  stopifnot(nrow(assignment) == 60L)
  stopifnot(setequal(unique(assignment$event_id), events$event_id))
  stopifnot(all(table(assignment$event_id) == 2L))
  stopifnot(all(table(assignment$language) == 30L))
  for (id in events$event_id) {
    pair <- assignment[assignment$event_id == id, , drop = FALSE]
    stopifnot(identical(sort(pair$language), c("EN", "TR")))
    stopifnot(identical(sort(as.integer(pair$slot)), 1:2))
  }
  assignment
}

get_pilot_assignment <- function() {
  events <- read_events("pilot")
  set.seed(PILOT_RANDOM_SEED)
  order <- sample(seq_len(nrow(events)))
  do.call(rbind, lapply(seq_along(order), function(j) {
    e <- events[order[j], , drop = FALSE]
    first <- sample(c("EN", "TR"), 1L)
    langs <- if (first == "EN") c("EN", "TR") else c("TR", "EN")
    data.frame(event_id = rep(e$event_id, 2), event_order = rep(j, 2),
               slot = 1:2, language = langs, stringsAsFactors = FALSE)
  }))
}

extract_text <- function(res) {
  parts <- unlist(lapply(res$output, function(item) {
    if (!identical(item$type, "message")) return(NULL)
    unlist(lapply(item$content, function(piece) {
      if (identical(piece$type, "output_text")) piece$text else NULL
    }), use.names = FALSE)
  }), use.names = FALSE)
  if (length(parts) == 0 || !any(nzchar(parts))) return(NA_character_)
  paste(parts, collapse = "\n")
}

estimated_cost <- function(res) {
  input <- as.numeric(res$usage$input_tokens)
  output <- as.numeric(res$usage$output_tokens)
  input * 10 / 1e6 + output * 50 / 1e6
}

existing_project_cost <- function() {
  if (!dir.exists(OUTPUT_ROOT)) return(0)
  files <- list.files(OUTPUT_ROOT, pattern = "_meta\\.csv$", recursive = TRUE,
                      full.names = TRUE)
  if (!length(files)) return(0)
  sum(vapply(files, function(f) {
    x <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(x) || !"estimated_cost_usd" %in% names(x)) return(0)
    sum(as.numeric(x$estimated_cost_usd), na.rm = TRUE)
  }, numeric(1)))
}

run_one_request <- function(event, language, slot, phase) {
  stopifnot(phase %in% c("pilot", "main"))
  stopifnot(language %in% c("EN", "TR"))
  subdir <- file.path(OUTPUT_ROOT, phase)
  dir.create(subdir, recursive = TRUE, showWarnings = FALSE)
  prefix <- file.path(subdir, sprintf("%s_slot%d_%s", event$event_id, slot, language))
  raw_file <- paste0(prefix, "_raw.json")
  text_file <- paste0(prefix, "_text.txt")
  prompt_file <- paste0(prefix, "_prompt.txt")
  meta_file <- paste0(prefix, "_meta.csv")
  prompt <- make_prompt(event, language)

  if (file.exists(raw_file)) {
    message("Already saved, reusing without API charge: ", basename(prefix))
    res <- jsonlite::fromJSON(raw_file, simplifyVector = FALSE)
    if (file.exists(prompt_file)) {
      saved_prompt <- paste(readLines(prompt_file, warn = FALSE, encoding = "UTF-8"),
                            collapse = "\n")
      if (!identical(saved_prompt, prompt)) {
        stop("PROMPT CHANGED for saved response: ", basename(prefix),
             ". Do not overwrite. Inspect event data and old outputs.")
      }
    }
  } else {
    if (existing_project_cost() >= ESTIMATED_PROJECT_COST_LIMIT_USD) {
      stop("Project's estimated costs have reached your safety threshold (USD ",
           ESTIMATED_PROJECT_COST_LIMIT_USD, "). Check API Billing before continuing.")
    }
    message("Calling ", MODEL, " / ", REASONING_EFFORT,
            " / ", phase, " / ", event$event_id, " / ", language)
    # No previous_response_id, no tools, no web access.
    # No automatic retry to avoid unlogged duplicate successful requests.
    res <- httr2::request("https://api.openai.com/v1/responses") |>
      httr2::req_auth_bearer_token(keyring::key_get(API_KEY_SERVICE)) |>
      httr2::req_timeout(240) |>
      httr2::req_body_json(list(
        model = MODEL,
        reasoning = list(effort = REASONING_EFFORT),
        input = prompt,
        store = FALSE
      )) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = FALSE)
    if (!identical(res$status, "completed") || !identical(res$model, MODEL) ||
        !identical(res$reasoning$effort, REASONING_EFFORT)) {
      stop("Request did not finish with the expected model/status. Response ID: ",
           ifelse(is.null(res$id), "unknown", res$id))
    }
    # Save the complete raw response before downstream formatting.
    jsonlite::write_json(res, raw_file, pretty = TRUE, auto_unbox = TRUE)
  }

  text <- extract_text(res)
  if (is.na(text) || !nzchar(text)) {
    # Keep raw response for documenting refusal/uncodable output.
    text <- "[NO OUTPUT TEXT; inspect raw JSON for refusal or incomplete response]"
  }
  writeLines(text, text_file, useBytes = TRUE)
  writeLines(prompt, prompt_file, useBytes = TRUE)
  meta <- data.frame(
    event_id = event$event_id, phase = phase, slot = slot,
    language = language, event_date = event$date,
    model = res$model, effort = res$reasoning$effort,
    response_id = res$id, completed = res$status,
    created_at_unix = res$created_at,
    input_tokens = as.integer(res$usage$input_tokens),
    output_tokens = as.integer(res$usage$output_tokens),
    estimated_cost_usd = round(estimated_cost(res), 6),
    text_file = normalizePath(text_file, winslash = "/", mustWork = FALSE),
    raw_file = normalizePath(raw_file, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )
  write.csv(meta, meta_file, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(meta)
}

# Pilot responses are kept separate from the main study.
# Existing pilot files must match the finalized prompt before reuse.
run_pilots <- function(skip_ids = character(0)) {
  events <- read_events("pilot")
  assignment <- get_pilot_assignment()
  message("Pilot calls planned: ", sum(!assignment$event_id %in% skip_ids))
  for (i in seq_len(nrow(assignment))) {
    row <- assignment[i, , drop = FALSE]
    if (row$event_id %in% skip_ids) next
    event <- events[events$event_id == row$event_id, , drop = FALSE]
    run_one_request(event, row$language, row$slot, "pilot")
  }
  message("Pilot completed. Responses saved to Desktop/responses/pilot/.")
  invisible(TRUE)
}

# MAIN EXPERIMENT
check_osf_registration <- function() {
  filename <- "OSF_REGISTERED.txt"
  if (!file.exists(filename)) {
    stop("MAIN LOCKED: Finalize pilot, prompts and rubric; register on OSF; ",
         "then create OSF_REGISTERED.txt containing the final registration URL.")
  }
  x <- paste(readLines(filename, warn = FALSE), collapse = " ")
  if (!grepl("https://osf\\.io/[A-Za-z0-9]+", x) ||
      grepl("draft|placeholder|YOUR", x, ignore.case = TRUE)) {
    stop("OSF_REGISTERED.txt needs a real, final OSF registration link, not a draft.")
  }
  invisible(TRUE)
}

# To collect all: run_main(batch_size = 60L).
run_main <- function(batch_size = 10L) {
  check_project()
  check_osf_registration()
  if (!is.numeric(batch_size) || length(batch_size) != 1L ||
      is.na(batch_size) || batch_size < 1) stop("batch_size must be positive")
  events <- read_events("main")
  assignment <- get_main_assignment()
  n_new <- 0L
  for (i in seq_len(nrow(assignment))) {
    row <- assignment[i, , drop = FALSE]
    prefix <- file.path(OUTPUT_ROOT, "main",
                        sprintf("%s_slot%d_%s", row$event_id, row$slot, row$language))
    if (file.exists(paste0(prefix, "_raw.json"))) {
      e <- events[events$event_id == row$event_id, , drop = FALSE]
      run_one_request(e, row$language, row$slot, "main")
      next
    }
    if (n_new >= batch_size) break
    e <- events[events$event_id == row$event_id, , drop = FALSE]
    run_one_request(e, row$language, row$slot, "main")
    n_new <- n_new + 1L
  }
  completed <- length(list.files(file.path(OUTPUT_ROOT, "main"),
                                 pattern = "_raw\\.json$", full.names = TRUE))
  message("New requests in this run: ", n_new, ". Main responses saved: ",
          completed, "/60. Estimated project cost: $",
          sprintf("%.3f", existing_project_cost()))
  invisible(completed)
}

# Run after all main responses are present, creating blank human coding sheet.
prepare_coding_sheet <- function() {
  check_osf_registration()
  a <- get_main_assignment()
  e <- read_events("main")
  stopifnot(nrow(a) == 60L)
  required <- file.path(OUTPUT_ROOT, "main",
                        sprintf("%s_slot%d_%s_raw.json", a$event_id, a$slot, a$language))
  if (!all(file.exists(required))) {
    stop("Only ", sum(file.exists(required)), "/60 main responses exist. Finish data collection first.")
  }
  coding_path <- "human_coding.csv"
  if (file.exists(coding_path)) stop("human_coding.csv already exists. Won't overwrite your coding.")
  idx <- match(a$event_id, e$event_id)
  out <- data.frame(
    event_id = a$event_id, language = a$language, slot = a$slot,
    event_name = e$event_en[idx],
    focal_action = e$action_en[idx],
    response_file = file.path(OUTPUT_ROOT, "main",
                               sprintf("%s_slot%d_%s_text.txt", a$event_id, a$slot, a$language)),
    valence_category = "", rights_mentioned = "",
    state_responsibility = "", notes = "",
    stringsAsFactors = FALSE
  )
  write.csv(out, coding_path, row.names = FALSE, fileEncoding = "UTF-8")
  message("Created human_coding.csv. Fill 60 rows using coding_rubric.md, ",
          "then run source('03_analyze.R'); analyze_main().")
  invisible(out)
}
