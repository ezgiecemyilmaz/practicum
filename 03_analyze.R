# 03_analyze.R | Human-primary paired analysis + supplementary AI comparison
# Original preregistered valence test is retained (10,000 within-event swaps).
# Secondary preregistered outcomes are saved separately as exploratory.

VALENCE_CATEGORIES <- c("critical", "neutral", "mixed", "favorable", "refusal", "uncodable")
SCORE_MAP <- c(critical = -1, neutral = 0, mixed = 0, favorable = 1)

normalize_category <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x == "-1"] <- "critical"
  x[x == "1"] <- "favorable"
  if (any(x == "0", na.rm = TRUE)) {
    stop("Numeric 0 is ambiguous: enter 'mixed' or 'neutral' explicitly.")
  }
  x
}

read_main_coding <- function(path) {
  if (!file.exists(path)) stop("Coding file not found: ", path)
  d <- read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM",
                na.strings = c("", "NA"), check.names = FALSE)
  needed <- c("event_id", "language", "valence_category",
              "rights_mentioned", "state_responsibility")
  if (!all(needed %in% names(d))) {
    stop("Missing required columns: ", paste(setdiff(needed, names(d)), collapse = ", "))
  }
  if (nrow(d) != 60L || anyDuplicated(paste(d$event_id, d$language))) {
    stop("Expected 60 unique event-language rows in ", path)
  }
  if (!all(d$language %in% c("EN", "TR")) ||
      any(table(d$event_id) != 2L) ||
      !all(vapply(split(d$language, d$event_id),
                  function(z) setequal(z, c("EN", "TR")), logical(1)))) {
    stop("Each event must have exactly one EN and one TR response.")
  }
  d$valence_category <- normalize_category(d$valence_category)
  if (anyNA(d$valence_category) ||
      !all(d$valence_category %in% VALENCE_CATEGORIES)) {
    stop("Invalid valence_category. Use six exact lowercase category labels.")
  }
  d$valence <- unname(SCORE_MAP[d$valence_category])
  d$neutral <- as.numeric(d$valence_category == "neutral")
  d$mixed <- as.numeric(d$valence_category == "mixed")
  d$neutral[is.na(d$valence)] <- NA_real_
  d$mixed[is.na(d$valence)] <- NA_real_
  for (nm in c("rights_mentioned", "state_responsibility")) {
    x <- suppressWarnings(as.numeric(d[[nm]]))
    if (any(!is.na(x) & !(x %in% c(0, 1)))) stop(nm, " must contain 0, 1, or blank")
    if (any(!is.na(d$valence) & is.na(x))) stop("Missing ", nm, " in codable response")
    x[is.na(d$valence)] <- NA_real_
    d[[nm]] <- x
  }
  d
}

paired_values <- function(d, variable) {
  en <- d[d$language == "EN", c("event_id", variable)]
  tr <- d[d$language == "TR", c("event_id", variable)]
  names(en)[2] <- "EN"; names(tr)[2] <- "TR"
  p <- merge(en, tr, by = "event_id", sort = TRUE)
  p$difference <- p$TR - p$EN
  p
}

# Preregistered randomization test: 10,000 within-event sign swaps.
paired_randomization_p <- function(differences, B = 10000L, seed = 20261117L) {
  differences <- differences[!is.na(differences)]
  n <- length(differences)
  if (n == 0L) return(NA_real_)
  set.seed(seed)
  signs <- matrix(sample(c(-1, 1), B * n, replace = TRUE), nrow = B)
  null <- rowMeans(sweep(signs, 2L, differences, `*`))
  (1 + sum(abs(null) >= abs(mean(differences)) - 1e-12)) / (B + 1)
}

# Supplementary sign-flip reference, when few pairs differ.
paired_exact_p <- function(differences, max_nonzero = 17L) {
  x <- differences[!is.na(differences) & abs(differences) > 1e-12]
  k <- length(x)
  if (k == 0L) return(1)
  if (k > max_nonzero) return(NA_real_)
  signs <- as.matrix(expand.grid(rep(list(c(-1, 1)), k)))
  simulated <- as.vector(signs %*% x)
  mean(abs(simulated) >= abs(sum(x)) - 1e-12)
}

summarize_pairs <- function(d, variable, primary = FALSE) {
  pair <- paired_values(d, variable)
  y <- pair$difference[!is.na(pair$difference)]
  n <- length(y)
  ate <- if (n) mean(y) else NA_real_
  se <- if (n > 1L) sd(y) / sqrt(n) else NA_real_
  ci <- if (n > 1L) ate + c(-1, 1) * qt(.975, df = n - 1L) * se else c(NA_real_, NA_real_)
  data.frame(outcome = variable, n_pairs = n, ATE_TR_minus_EN = ate,
             SE = se, CI95_low = ci[1], CI95_high = ci[2],
             p_value_randomization = if (primary) paired_randomization_p(y) else NA_real_,
             p_value_exact_signflip = if (primary) paired_exact_p(y) else NA_real_)
}

# The complete-case primary analysis and prespecified worst/best-case bounds are unchanged. With 0 missing responses, bounds equal the point estimate.
missing_extreme_bounds <- function(d) {
  p <- paired_values(d, "valence")
  data.frame(n_missing_pairs = sum(is.na(p$TR) | is.na(p$EN)),
             lower_ATE = mean(ifelse(is.na(p$TR), -1, p$TR) - ifelse(is.na(p$EN), 1, p$EN)),
             upper_ATE = mean(ifelse(is.na(p$TR), 1, p$TR) - ifelse(is.na(p$EN), -1, p$EN)))
}

# ------------------------- Figures -------------------------
with_plot <- function(output_dir, basename, plot_function, width = 10, height = 6) {
  png(file.path(output_dir, paste0(basename, ".png")),
      width = as.integer(width * 160), height = as.integer(height * 160), res = 160)
  tryCatch(plot_function(), finally = dev.off())
  pdf(file.path(output_dir, paste0(basename, ".pdf")),
      width = width, height = height, useDingbats = FALSE)
  tryCatch(plot_function(), finally = dev.off())
}

plot_outcome_distribution <- function(d, output_dir) {
  labels <- c("Critical", "Mixed", "Favorable")
  categories <- tolower(labels)
  tab <- table(factor(d$valence_category, levels = categories),
               factor(d$language, levels = c("EN", "TR")))
  en <- as.numeric(tab[, "EN"]); tr <- as.numeric(tab[, "TR"])
  with_plot(output_dir, "figure1_outcome_distribution", function() {
    par(mar = c(5, 5, 3.5, 2), family = "sans")
    plot(NA, xlim = c(.45, 3.55), ylim = c(0, max(30, en, tr) + 2),
         xlab = "", ylab = "Number of responses", axes = FALSE,
         main = "Normative evaluations by prompt language")
    abline(h = seq(0, 30, 5), col = "#E8EDF1", lwd = 1)
    axis(2, at = seq(0, 30, 5), las = 1, col.axis = "#4B5563", col = NA)
    axis(1, at = 1:3, labels = labels, tick = FALSE, cex.axis = 1.1, col.axis = "#283747")
    bar_w <- .28
    rect((1:3)-.33, 0, (1:3)-.33+bar_w, en, col = "#334E68", border = NA)
    rect((1:3)+.05, 0, (1:3)+.05+bar_w, tr, col = "#16857D", border = NA)
    text((1:3)-.33+bar_w/2, en+.8, labels = en, cex = .95, font = 2)
    text((1:3)+.05+bar_w/2, tr+.8, labels = tr, cex = .95, font = 2)
    legend("topright", legend = c("English", "Turkish"), fill = c("#334E68", "#16857D"),
           horiz = TRUE, bty = "n", inset = .02, cex = .95)
    mtext("30 events per language | No neutral, refusal, or uncodable responses",
          side = 1, line = 3.3, cex = .86, col = "#56616B")
    box(bty = "l", col = "#A3ACB3")
  }, width = 9, height = 5.6)
}

plot_discordant_pairs <- function(d, output_dir) {
  p <- paired_values(d, "valence")
  valid <- p[!is.na(p$difference), , drop = FALSE]
  altered <- valid[abs(valid$difference) > 1e-12, , drop = FALSE]
  altered <- altered[order(altered$event_id), , drop = FALSE]
  k <- nrow(altered)
  identical_n <- nrow(valid) - k
  with_plot(output_dir, "figure2_discordant_pairs", function() {
    par(mar = c(5.2, 5.3, 3.5, 2.5), family = "sans")
    if (!k) {
      plot.new(); title("Paired comparisons by event")
      text(.5, .5, "All scorable pairs have identical valence scores.", cex = 1.1)
      return(invisible(NULL))
    }
    ys <- rev(seq_len(k))
    plot(NA, xlim = c(-1.3, 1.3), ylim = c(.3, k+.85), axes = FALSE,
         xlab = "Normative valence (critical = -1; mixed = 0; favorable = +1)",
         ylab = "", main = "Events with different Turkish and English scores")
    abline(v = c(-1, 0, 1), col = "#E7EAED", lty = 2)
    axis(1, at = c(-1, 0, 1), labels = c("Critical (-1)", "Mixed (0)", "Favorable (+1)"),
         tick = FALSE, cex.axis = .9)
    axis(2, at = ys, labels = altered$event_id, las = 1, tick = FALSE, cex.axis = .95)
    for (i in seq_len(k)) {
      segments(altered$EN[i], ys[i], altered$TR[i], ys[i],
               col = "#AAB4BD", lwd = 3)
      points(altered$EN[i], ys[i], pch = 21, bg = "#334E68", col = "white", cex = 1.6)
      points(altered$TR[i], ys[i], pch = 21, bg = "#16857D", col = "white", cex = 1.6)
    }
    legend("topright", legend = c("English", "Turkish"), pch = 21,
           pt.bg = c("#334E68", "#16857D"), col = "white", bty = "n", horiz = TRUE)
    mtext(sprintf("%d of %d pairs had identical scores and are omitted for readability.",
                  identical_n, nrow(valid)), side = 1, line = 3.75,
          col = "#56616B", cex = .84)
  }, width = 9, height = max(5.1, k*.53+2.6))
}

plot_ate_comparison <- function(estimate_table, output_dir) {
  with_plot(output_dir, "figure3_human_ai_estimates", function() {
    # Extra left margin prevents the two coder labels from being clipped.
    par(mar = c(6.0, 10.5, 3.5, 2.0), family = "sans")
    xlo <- min(-.12, estimate_table$CI95_low) - .04
    xhi <- max(.44, estimate_table$CI95_high) + .06
    plot(NA, xlim = c(xlo, xhi), ylim = c(.45, 2.55), axes = FALSE,
         xlab = "Mean difference in valence (Turkish - English)", ylab = "",
         main = "Estimated language differences by coder")
    abline(v = 0, lty = 2, col = "#9CA3AF", lwd = 1.4)
    axis(1, cex.axis = .9)
    axis(2, at = c(2, 1), labels = c("Human (primary)", "AI (supplementary)"),
         tick = FALSE, las = 1, cex.axis = 0.97, line = 0.4)
    cols <- c("#16857D", "#334E68")
    for (i in seq_len(nrow(estimate_table))) {
      y <- 3 - i
      segments(estimate_table$CI95_low[i], y, estimate_table$CI95_high[i], y,
               lwd = 3, col = cols[i])
      segments(estimate_table$CI95_low[i], y-.075, estimate_table$CI95_low[i], y+.075,
               lwd = 2, col = cols[i])
      segments(estimate_table$CI95_high[i], y-.075, estimate_table$CI95_high[i], y+.075,
               lwd = 2, col = cols[i])
      points(estimate_table$ATE_TR_minus_EN[i], y, pch = 21,
             bg = cols[i], col = "white", cex = 1.9)
    }
    mtext("Points: paired mean differences | Bars: conventional 95% paired t confidence intervals",
          side = 1, line = 4.0, cex = .8, col = "#56616B")
  }, width = 9, height = 5.1)
}

cohens_kappa <- function(x, y, categories) {
  tab <- table(factor(x, levels = categories), factor(y, levels = categories))
  N <- sum(tab)
  if (N == 0) return(NA_real_)
  observed <- sum(diag(tab)) / N
  expected <- sum(rowSums(tab) * colSums(tab)) / (N * N)
  if (abs(1 - expected) < 1e-12) return(NA_real_)
  (observed - expected) / (1 - expected)
}

plot_coder_agreement <- function(h, ai, output_dir) {
  m <- merge(h[, c("event_id", "language", "valence_category")],
             ai[, c("event_id", "language", "valence_category")],
             by = c("event_id", "language"), suffixes = c("_human", "_ai"))
  cats <- c("critical", "mixed", "favorable")
  tab <- table(factor(m$valence_category_human, levels = cats),
               factor(m$valence_category_ai, levels = cats))
  n_agree <- sum(m$valence_category_human == m$valence_category_ai)
  kap <- cohens_kappa(m$valence_category_human, m$valence_category_ai,
                      c("critical", "neutral", "mixed", "favorable", "refusal", "uncodable"))
  with_plot(output_dir, "figure4_coder_agreement", function() {
    # Reserve room for category names, axis titles and footer separately.
    par(mar = c(8.0, 8.3, 4.6, 2.1), family = "sans")
    plot(NA, xlim = c(.5, 3.5), ylim = c(.5, 3.5), xlab = "",
         ylab = "", axes = FALSE,
         main = "Independent valence coding agreement")
    for (i in 1:3) for (j in 1:3) {
      value <- tab[i,j]
      fill <- if (i == j) "#D8F0E8" else if (value > 0) "#FCE9D9" else "#F3F5F6"
      rect(j-.45, 4-i-.45, j+.45, 4-i+.45, col = fill, border = "white", lwd = 3)
      text(j, 4-i, value, cex = 1.65, font = if (value) 2 else 1,
           col = "#243746")
    }
    axis(1, at = 1:3, labels = c("Critical", "Mixed", "Favorable"),
         tick = FALSE, cex.axis = 1.0, line = 0.2)
    axis(2, at = 3:1, labels = c("Critical", "Mixed", "Favorable"),
         tick = FALSE, las = 1, cex.axis = 1.0, line = 0.2)
    mtext("AI coder", side = 1, line = 2.7, cex = 1.05)
    mtext("Human coder", side = 2, line = 6.0, cex = 1.05)
    box(bty = "n")
    subtxt <- sprintf("%d/%d agreed (%.1f%%); Cohen's kappa = %.3f",
                      n_agree, nrow(m), 100*n_agree/nrow(m), kap)
    mtext(subtxt, side = 3, line = .35, cex = .98, col = "#3F5361")
    mtext("Diagonal cells show agreement; off-diagonal cells show disagreements.",
          side = 1, line = 5.0, cex = .78, col = "#56616B")
  }, width = 8.3, height = 7.1)
}

# ------------------------- Analysis output -------------------------
resolve_human_path <- function() {
  if (file.exists("human_coding_final.csv")) return("human_coding_final.csv")
  "human_coding.csv"
}

analyze_main <- function(path = resolve_human_path(),
                         output_dir = "analysis_results", ai_path = "ai_coding.csv") {
  human <- read_main_coding(path)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # PRIMARY TABLE: valence for the primary human coder.
  human_primary <- summarize_pairs(human, "valence", primary = TRUE)
  human_primary$coding <- "Human (primary)"
  write.csv(human_primary, file.path(output_dir, "estimates.csv"), row.names = FALSE)

  # Prespecified secondary outcomes remain available separately, not in main table.
  secondary <- do.call(rbind, lapply(c("neutral", "mixed", "rights_mentioned", "state_responsibility"),
                                      function(v) summarize_pairs(human, v, primary = FALSE)))
  write.csv(secondary, file.path(output_dir, "secondary_exploratory.csv"), row.names = FALSE)
  dist <- as.data.frame(table(human$language, human$valence_category),
                        stringsAsFactors = FALSE)
  names(dist) <- c("language", "category", "n")
  write.csv(dist, file.path(output_dir, "outcome_distribution.csv"), row.names = FALSE)
  p <- paired_values(human, "valence")
  write.csv(p, file.path(output_dir, "paired_outcomes.csv"), row.names = FALSE)

  bounds <- missing_extreme_bounds(human)
  write.csv(bounds, file.path(output_dir, "missingness_bounds.csv"), row.names = FALSE)
  message("Missing event pairs: ", bounds$n_missing_pairs,
          if (bounds$n_missing_pairs == 0) " (no missingness to discuss in main text)" else "")

  plot_outcome_distribution(human, output_dir)
  plot_discordant_pairs(human, output_dir)

  estimates_comparison <- human_primary
  if (file.exists(ai_path)) {
    ai <- read_main_coding(ai_path)
    if (!setequal(paste(human$event_id, human$language),
                  paste(ai$event_id, ai$language))) stop("AI and human files have different keys")
    ai_primary <- summarize_pairs(ai, "valence", primary = TRUE)
    ai_primary$coding <- "AI (supplementary)"
    write.csv(ai_primary, file.path(output_dir, "ai_valence_estimates.csv"), row.names = FALSE)
    estimates_comparison <- rbind(human_primary, ai_primary)
    plot_ate_comparison(estimates_comparison, output_dir)
    plot_coder_agreement(human, ai, output_dir)
    compare <- merge(human[, c("event_id", "language", "valence_category",
                                "rights_mentioned", "state_responsibility")],
                     ai[, c("event_id", "language", "valence_category",
                             "rights_mentioned", "state_responsibility")],
                     by = c("event_id", "language"), suffixes = c("_human", "_ai"))
    write.csv(compare, file.path(output_dir, "coder_comparison.csv"), row.names = FALSE)
    cvars <- c("valence_category", "rights_mentioned", "state_responsibility")
    agreement <- do.call(rbind, lapply(cvars, function(v) {
      a <- as.character(compare[[paste0(v, "_human")]])
      b <- as.character(compare[[paste0(v, "_ai")]])
      used <- !is.na(a) & !is.na(b)
      agreement_n <- sum(a[used] == b[used])
      data.frame(outcome = v, n_compared = sum(used), n_agree = agreement_n,
                 agreement_rate = agreement_n / sum(used),
                 stringsAsFactors = FALSE)
    }))
    write.csv(agreement, file.path(output_dir, "coder_agreement_summary.csv"), row.names = FALSE)
  } else {
    message("No ai_coding.csv found. Human-primary analysis completed without AI figures.")
  }

  write.csv(estimates_comparison,
            file.path(output_dir, "valence_estimates_comparison.csv"), row.names = FALSE)
  print(estimates_comparison[, c("coding", "n_pairs", "ATE_TR_minus_EN", "SE",
                                 "CI95_low", "CI95_high", "p_value_randomization",
                                 "p_value_exact_signflip")], digits = 4, row.names = FALSE)
  message("Saved plots (PNG + PDF) and tables to: ", normalizePath(output_dir))
  invisible(list(estimates = estimates_comparison, secondary = secondary,
                 missing_bounds = bounds))
}
