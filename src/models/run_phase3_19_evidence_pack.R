source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")

phase3_19_read_csv <- function(path, required = TRUE) {
  full_path <- project_path(path)
  if (!file.exists(full_path)) {
    if (isTRUE(required)) {
      stop(sprintf("Required Phase 3.19 input not found: %s", full_path), call. = FALSE)
    }
    return(data.frame())
  }
  x <- utils::read.csv(full_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  names(x) <- sub("^\ufeff", "", names(x))
  x
}

phase3_19_write_csv <- function(x, path) {
  write_csv_output(x, project_path(path), overwrite = TRUE)
  invisible(path)
}

phase3_19_as_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

phase3_19_count <- function(x, col, value = NULL, contains = NULL) {
  if (!col %in% names(x)) return(0L)
  if (!is.null(value)) return(sum(x[[col]] == value, na.rm = TRUE))
  if (!is.null(contains)) return(sum(grepl(contains, x[[col]], fixed = TRUE), na.rm = TRUE))
  length(stats::na.omit(x[[col]]))
}

phase3_19_best_row <- function(x, rank_col) {
  if (!rank_col %in% names(x) || nrow(x) == 0) return(data.frame())
  x[[rank_col]] <- phase3_19_as_num(x[[rank_col]])
  x[order(x[[rank_col]], x$hedge, x$model), , drop = FALSE][1, , drop = FALSE]
}

phase3_19_refresh_table_10a <- function() {
  src <- "results/tables/tras/tras_final_ranking.csv"
  x <- phase3_19_read_csv(src)
  rank_cols <- c(
    "hedging_effectiveness_rank",
    "mean_var_reduction_rank",
    "mean_es_reduction_rank",
    "drawdown_reduction_rank",
    "portfolio_delta_covar_improvement_rank",
    "lower_tail_absorption_score_rank",
    "direct_protection_layer_rank",
    "systemic_absorption_layer_rank",
    "conditional_tail_layer_rank",
    "tras_flat_rank",
    "tras_flat_score",
    "tras_layered_rank",
    "tras_layered_score",
    "tras_rank",
    "tras_score"
  )
  keep <- c("hedge", "model", rank_cols)
  out <- x[, intersect(keep, names(x)), drop = FALSE]
  out <- out[order(phase3_19_as_num(out$tras_rank), out$hedge, out$model), , drop = FALSE]
  names(out) <- sub("_rank$", "_rank", names(out))
  dst <- "paper/tables/table_10a_tras_component_rank_guardrail.csv"
  phase3_19_write_csv(out, dst)
  data.frame(
    table = "table_10a_tras_component_rank_guardrail",
    source = src,
    destination = dst,
    rows = nrow(out),
    stringsAsFactors = FALSE
  )
}

phase3_19_create_guardrail_table <- function() {
  daily <- phase3_19_read_csv("results/tables/safe_haven_tests/safe_haven_interpretation_matrix.csv")
  weekly <- phase3_19_read_csv("results/tables/robustness/weekly_safe_haven_interpretation.csv", required = FALSE)
  tras <- phase3_19_read_csv("results/tables/tras/tras_final_ranking.csv")
  leave_one <- phase3_19_read_csv("results/tables/tras/tras_leave_one_metric_out.csv")
  ml_perf <- phase3_19_read_csv("results/tables/adaptive_ml_upgrade/ml_upgrade_benchmark_comparison.csv", required = FALSE)
  ml_perm <- phase3_19_read_csv("results/tables/adaptive_ml_upgrade/ml_upgrade_permutation_test.csv", required = FALSE)
  ml_final <- phase3_19_read_csv("results/tables/adaptive_ml_final/ml_final_benchmark_comparison.csv", required = FALSE)

  daily_total <- nrow(daily)
  daily_strong_safe <- phase3_19_count(daily, "safe_haven_classification", contains = "strong_safe_haven")
  daily_strong_hedge <- phase3_19_count(daily, "hedge_classification", contains = "strong_hedge")
  daily_weak_safe <- phase3_19_count(daily, "safe_haven_classification", "weak_safe_haven")
  daily_positive_stress <- phase3_19_count(daily, "safe_haven_classification", "not_a_safe_haven_positive_stress_comovement")

  weekly_gold <- weekly[weekly$asset_role == "weekly_gold_premium_channel", , drop = FALSE]
  weekly_gold_strong <- phase3_19_count(weekly_gold, "safe_haven_classification", contains = "strong_safe_haven")

  best_tras <- phase3_19_best_row(tras, "tras_rank")
  omit_connectedness <- leave_one[leave_one$omitted_metric == "lower_tail_absorption_score", , drop = FALSE]
  best_ex_connectedness <- phase3_19_best_row(omit_connectedness, "tras_rank_without_metric")

  best_total_return <- data.frame()
  best_es <- data.frame()
  if (nrow(ml_perf) > 0) {
    ml_perf$total_return_pct <- phase3_19_as_num(ml_perf$total_return_pct)
    ml_perf$es_reduction_vs_unhedged <- phase3_19_as_num(ml_perf$es_reduction_vs_unhedged)
    best_total_return <- ml_perf[order(-ml_perf$total_return_pct), , drop = FALSE][1, , drop = FALSE]
    best_es <- ml_perf[order(-ml_perf$es_reduction_vs_unhedged), , drop = FALSE][1, , drop = FALSE]
  }
  perm_col <- intersect(c("p_value_upper_tail", "permutation_p_value_upper_tail"), names(ml_perm))
  perm_p <- if (nrow(ml_perm) > 0 && length(perm_col) > 0) {
    phase3_19_as_num(ml_perm[[perm_col[1]]][1])
  } else {
    NA_real_
  }
  best_final_return <- data.frame()
  best_final_es <- data.frame()
  ml_final_row <- data.frame()
  if (nrow(ml_final) > 0) {
    ml_final$total_return_pct <- phase3_19_as_num(ml_final$total_return_pct)
    ml_final$es_reduction_vs_unhedged <- phase3_19_as_num(ml_final$es_reduction_vs_unhedged)
    best_final_return <- ml_final[order(-ml_final$total_return_pct), , drop = FALSE][1, , drop = FALSE]
    best_final_es <- ml_final[order(-ml_final$es_reduction_vs_unhedged), , drop = FALSE][1, , drop = FALSE]
    ml_final_row <- ml_final[ml_final$strategy == "ml_final_nested", , drop = FALSE]
  }

  rows <- list(
    data.frame(
      issue = "daily_strong_safe_haven",
      empirical_result = sprintf("Strong safe-haven classifications in daily Baur-Lucey matrix: %s out of %s.", daily_strong_safe, daily_total),
      required_manuscript_language = "Do not claim any daily asset is a strong safe haven. Present daily evidence as mostly weak/non-harm evidence.",
      source = "results/tables/safe_haven_tests/safe_haven_interpretation_matrix.csv",
      stringsAsFactors = FALSE
    ),
    data.frame(
      issue = "daily_strong_hedge",
      empirical_result = sprintf("Strong hedge classifications in daily Baur-Lucey matrix: %s out of %s.", daily_strong_hedge, daily_total),
      required_manuscript_language = "Do not claim any daily asset is a statistically strong hedge against VNIndex.",
      source = "results/tables/safe_haven_tests/safe_haven_interpretation_matrix.csv",
      stringsAsFactors = FALSE
    ),
    data.frame(
      issue = "daily_weak_safe_haven_meaning",
      empirical_result = sprintf("Weak safe-haven rows: %s; positive stress-comovement failures: %s.", daily_weak_safe, daily_positive_stress),
      required_manuscript_language = "Define weak safe haven as absence of statistically positive stress comovement, not proof of active protection.",
      source = "results/tables/safe_haven_tests/safe_haven_interpretation_matrix.csv",
      stringsAsFactors = FALSE
    ),
    data.frame(
      issue = "weekly_gold_premium_signal",
      empirical_result = sprintf("Weekly gold-premium channel has %s strong-safe-haven specification rows.", weekly_gold_strong),
      required_manuscript_language = "Use weekly gold-premium evidence as fragile robustness only because the 1 percent weekly tail contains few observations.",
      source = "results/tables/robustness/weekly_safe_haven_interpretation.csv",
      stringsAsFactors = FALSE
    ),
    data.frame(
      issue = "tras_layered_ranking",
      empirical_result = sprintf("TRAS-layered rank 1: %s/%s with score %.4f.", best_tras$hedge[1], best_tras$model[1], phase3_19_as_num(best_tras$tras_score[1])),
      required_manuscript_language = "Report TRAS-layered as a three-layer composite risk-absorption ranking, not as direct proof of lower realized investor losses.",
      source = "results/tables/tras/tras_final_ranking.csv",
      stringsAsFactors = FALSE
    ),
    data.frame(
      issue = "tras_ex_connectedness_robustness",
      empirical_result = sprintf("Best rank after omitting lower-tail connectedness absorption: %s/%s with rank %.1f.", best_ex_connectedness$hedge[1], best_ex_connectedness$model[1], phase3_19_as_num(best_ex_connectedness$tras_rank_without_metric[1])),
      required_manuscript_language = "Always discuss TRAS-layered together with the ex-connectedness robustness result and component ranks.",
      source = "results/tables/tras/tras_leave_one_metric_out.csv;paper/tables/table_10a_tras_component_rank_guardrail.csv",
      stringsAsFactors = FALSE
    ),
    data.frame(
      issue = "brent_role",
      empirical_result = "Brent is no longer in the core hedge-asset set used for TRAS; core hedge assets are SJC, XAU/VND and Bond TRI.",
      required_manuscript_language = "Treat Brent as an oil-price shock/control variable, not as a Vietnamese safe-haven asset that investors can naturally hold.",
      source = "src/models/run_phase3_11_tras_score.R;results/tables/tras/tras_final_ranking.csv",
      stringsAsFactors = FALSE
    ),
    data.frame(
      issue = "ml_upgrade_guardrail",
      empirical_result = sprintf(
        "Best ML total-return strategy: %s (%.2f%%); best ES-reduction strategy: %s (%.4f); permutation p-value for logistic accuracy: %.3f.",
        best_total_return$strategy[1], phase3_19_as_num(best_total_return$total_return_pct[1]),
        best_es$strategy[1], phase3_19_as_num(best_es$es_reduction_vs_unhedged[1]),
        perm_p
      ),
      required_manuscript_language = "Describe ML as an adaptive application layer. Do not claim robust predictive superiority when the permutation sanity test is not significant.",
      source = "results/tables/adaptive_ml_upgrade/ml_upgrade_benchmark_comparison.csv;results/tables/adaptive_ml_upgrade/ml_upgrade_permutation_test.csv",
      stringsAsFactors = FALSE
    ),
    data.frame(
      issue = "ml_final_guardrail",
      empirical_result = sprintf(
        "Final ML total return: %.2f%%; best total-return strategy: %s (%.2f%%); best ES-reduction strategy: %s (%.4f).",
        phase3_19_as_num(ml_final_row$total_return_pct[1]),
        best_final_return$strategy[1], phase3_19_as_num(best_final_return$total_return_pct[1]),
        best_final_es$strategy[1], phase3_19_as_num(best_final_es$es_reduction_vs_unhedged[1])
      ),
      required_manuscript_language = "Frame Phase 3.20 as final adaptive-ML validation: it improves the tail-risk/ES layer in this run, but it does not dominate every total-return or variance benchmark.",
      source = "results/tables/adaptive_ml_final/ml_final_benchmark_comparison.csv;paper/tables/table_19_ml_final_validation.csv",
      stringsAsFactors = FALSE
    )
  )

  out <- do.call(rbind, rows)
  dst <- "paper/tables/table_13_interpretation_guardrails.csv"
  phase3_19_write_csv(out, dst)
  data.frame(
    table = "table_13_interpretation_guardrails",
    source = paste(unique(out$source), collapse = ";"),
    destination = dst,
    rows = nrow(out),
    stringsAsFactors = FALSE
  )
}

phase3_19_audit_paper_tables <- function() {
  manifest <- phase3_19_read_csv("paper/tables/table_manifest.csv", required = FALSE)
  if (nrow(manifest) == 0) {
    return(data.frame())
  }
  manifest <- manifest[manifest$table != "table_18_phase3_19_evidence_audit", , drop = FALSE]
  audit <- lapply(seq_len(nrow(manifest)), function(i) {
    dst <- manifest$destination[i]
    full <- project_path(dst)
    exists <- file.exists(full)
    rows_actual <- NA_integer_
    cols_actual <- NA_integer_
    status <- "missing"
    note <- "Destination file is missing."
    if (exists) {
      x <- phase3_19_read_csv(dst, required = FALSE)
      rows_actual <- nrow(x)
      cols_actual <- ncol(x)
      if (rows_actual == 0 || cols_actual == 0) {
        status <- "fail"
        note <- "Table is empty."
      } else if (rows_actual == 1 && any(grepl("\\.csv$|\\.xlsx$|results/|data/", as.character(unlist(x[1, , drop = FALSE]))))) {
        status <- "review"
        note <- "Single-row table contains a path-like value; verify it is not a pointer table."
      } else {
        status <- "pass"
        note <- "Table exists and contains data rows."
      }
    }
    data.frame(
      table = manifest$table[i],
      destination = dst,
      manifest_rows = manifest$rows[i],
      actual_rows = rows_actual,
      actual_columns = cols_actual,
      audit_status = status,
      audit_note = note,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, audit)
  dst <- "paper/tables/table_18_phase3_19_evidence_audit.csv"
  phase3_19_write_csv(out, dst)
  data.frame(
    table = "table_18_phase3_19_evidence_audit",
    source = "paper/tables/table_manifest.csv;paper/tables/*.csv",
    destination = dst,
    rows = nrow(out),
    stringsAsFactors = FALSE
  )
}

phase3_19_update_manifest <- function(rows_to_upsert) {
  manifest_path <- "paper/tables/table_manifest.csv"
  manifest <- phase3_19_read_csv(manifest_path, required = FALSE)
  if (nrow(manifest) == 0) {
    manifest <- rows_to_upsert
  } else {
    manifest <- manifest[!manifest$table %in% rows_to_upsert$table, , drop = FALSE]
    manifest <- rbind(manifest, rows_to_upsert)
  }
  table_order <- c(
    "table_01_descriptive_statistics",
    "table_02_garch_selection",
    "table_03_dcc_adcc_summary",
    "table_04_hedging_effectiveness",
    "table_05_safe_haven_regression",
    "table_06_robustness_summary",
    "table_07_markov_switching_robustness",
    "table_08_connectedness",
    "table_09_tail_risk",
    "table_10_tras_ranking",
    "table_10a_tras_component_rank_guardrail",
    "table_11_sector_application",
    "table_12_adaptive_ml",
    "table_13_interpretation_guardrails",
    "table_14_bond_term_structure_channel",
    "table_15_bond_tenor_proxy_hedging",
    "table_16_bond_tri_tenor_hedging",
    "table_17_ml_upgrade_validation",
    "table_18_phase3_19_evidence_audit",
    "table_19_ml_final_validation"
  )
  manifest$.order <- match(manifest$table, table_order)
  manifest$.order[is.na(manifest$.order)] <- length(table_order) + seq_len(sum(is.na(manifest$.order)))
  manifest <- manifest[order(manifest$.order), setdiff(names(manifest), ".order"), drop = FALSE]
  phase3_19_write_csv(manifest, manifest_path)
  manifest
}

phase3_19_write_report <- function(guardrail_row, table10a_row, audit_row, manifest) {
  guardrails <- phase3_19_read_csv(guardrail_row$destination)
  audit <- phase3_19_read_csv(audit_row$destination)
  audit_fail <- audit[audit$audit_status != "pass", , drop = FALSE]

  lines <- c(
    "# Phase 3.19 Evidence Pack Report",
    "",
    "## Objective",
    "",
    "Phase 3.19 locks the paper-ready evidence pack: it checks Table 01, refreshes Safe Haven/TRAS/ML interpretation guardrails, and creates an audit table to prevent empty tables or pointer-only tables.",
    "",
    "## Main Outputs",
    "",
    sprintf("- Updated `%s`: `%s` rows covering full component ranks for all core hedge-model combinations.", table10a_row$destination, table10a_row$rows),
    sprintf("- Updated `%s`: `%s` interpretation guardrails.", guardrail_row$destination, guardrail_row$rows),
    sprintf("- Created `%s`: `%s` paper-ready tables audited.", audit_row$destination, audit_row$rows),
    sprintf("- Manifest now contains `%s` tables.", nrow(manifest)),
    "",
    "## Required Manuscript Guardrails",
    "",
    "- Do not state that daily data proves any asset is a strong safe haven or a strong hedge.",
    "- Weak safe haven means absence of statistically positive stress comovement, not active downside protection.",
    "- TRAS-layered must be reported together with TRAS-flat, TRAS-ex-connectedness, and component ranks.",
    "- Brent should be framed as an oil-price shock/control, not as a core Vietnamese safe-haven asset.",
    "- ML upgrade is an adaptive application layer; the permutation sanity test does not support a strong predictive-superiority claim.",
    "- Final ML validation should be read as an adaptive tail-risk application layer, not as proof that ML dominates every return or variance benchmark.",
    "",
    "## Audit",
    ""
  )
  if (nrow(audit_fail) == 0) {
    lines <- c(lines, "All manifest tables exist and contain data.")
  } else {
    lines <- c(lines, "Tables requiring manual review:", "")
    for (i in seq_len(nrow(audit_fail))) {
      lines <- c(lines, sprintf("- `%s`: %s", audit_fail$table[i], audit_fail$audit_note[i]))
    }
  }
  lines <- c(
    lines,
    "",
    "## Output Files",
    "",
    "- `paper/tables/table_10a_tras_component_rank_guardrail.csv`",
    "- `paper/tables/table_13_interpretation_guardrails.csv`",
    "- `paper/tables/table_18_phase3_19_evidence_audit.csv`",
    "- `data/metadata/phase3_19_evidence_pack_report.md`"
  )

  report_path <- project_path("data/metadata/phase3_19_evidence_pack_report.md")
  ensure_dir(dirname(report_path))
  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_19_evidence_pack <- function() {
  ensure_dir(project_path("paper/tables"))
  ensure_dir(project_path("data/metadata"))

  table10a_row <- phase3_19_refresh_table_10a()
  guardrail_row <- phase3_19_create_guardrail_table()
  manifest_mid <- phase3_19_update_manifest(rbind(table10a_row, guardrail_row))
  audit_row <- phase3_19_audit_paper_tables()
  manifest <- phase3_19_update_manifest(audit_row)
  report <- phase3_19_write_report(guardrail_row, table10a_row, audit_row, manifest)

  list(
    table10a = table10a_row,
    guardrails = guardrail_row,
    audit = audit_row,
    manifest = manifest,
    report = report
  )
}

is_direct_rscript_call <- function(file) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  length(file_arg) > 0 && identical(
    normalizePath(file_arg[1], winslash = "/", mustWork = FALSE),
    normalizePath(file, winslash = "/", mustWork = FALSE)
  )
}

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_19_evidence_pack.R")) {
  result <- run_phase3_19_evidence_pack()
  cat("Phase 3.19 evidence pack completed.\n")
  cat(sprintf("Updated guardrail rows: %s\n", result$guardrails$rows))
  cat(sprintf("Audited paper tables: %s\n", result$audit$rows))
  cat(sprintf("Report: %s\n", result$report))
}
