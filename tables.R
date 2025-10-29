# install.packages(c("reactable", "htmltools", "scales")) # if needed
library(reactable)
library(htmltools)
library(scales)

# Helper: compute AF from RR and prevalence p
af_from_rr <- function(rr, p) p * (rr - 1) / (p * (rr - 1) + 1)

# Minimal demo data (replace with your own estimates)
# NOTE: RR values here are placeholders – please swap in your sourced numbers.
obesity_rr_demo <- data.frame(
  system  = c(
    rep("Respiratory", 2),
    rep("Cancer", 3),
    rep("Cardiovascular", 4)
  ),
  disease = c(
    "Asthma exacerbation", "COPD progression",
    "Colorectal cancer", "Postmenopausal breast cancer", "Endometrial cancer",
    "Coronary heart disease", "Stroke", "Heart failure", "Atrial fibrillation"
  ),
  RR = c(1.30, 1.20, 1.15, 1.20, 1.50, 1.25, 1.20, 1.40, 1.20),
  stringsAsFactors = FALSE
)

# Table factory
obesity_rr_table <- function(
    df,
    p_obesity = 0.30,   # assumed obesity prevalence in your population
    digits_rr = 2,
    digits_af = 1
) {
  df$AF <- af_from_rr(df$RR, p_obesity)
  
  badge <- function(x, title = NULL) {
    div(
      title = title,
      style = "
        display:inline-block;padding:2px 8px;border-radius:9999px;
        font-weight:600;font-size:0.85rem;background:#f5f5f7;color:#333;
        border:1px solid #e6e6eb;
      ",
      x
    )
  }
  
  af_bar <- function(value) {
    # value is AF in [0,1]
    width_pct <- paste0(round(100 * value, 1), "%")
    div(
      style = "display:flex;align-items:center;gap:8px;",
      div(
        style = sprintf("
          position:relative;flex:1;height:10px;border-radius:6px;
          background:linear-gradient(90deg, rgba(0,0,0,0.08) 0%%, rgba(0,0,0,0.08) 100%%);
        ")
        ,
        # fill
        div(style = sprintf("
          position:relative;height:10px;border-radius:6px;width:%s;
          background:rgba(0,0,0,0.3);
        ", width_pct))
      ),
      div(style="min-width:52px;text-align:right;font-variant-numeric:tabular-nums;",
          percent(value, accuracy = 0.1))
    )
  }
  
  reactable(
    df,
    groupBy = "system",
    columns = list(
      system = colDef(name = "System", aggregate = "unique", html = TRUE),
      disease = colDef(
        name = "Disease",
        minWidth = 220,
        cell = function(x) htmltools::tags$span(style="font-weight:600;", x)
      ),
      RR = colDef(
        name = "Relative Risk",
        align = "center",
        minWidth = 150,
        cell = function(x) badge(formatC(x, format = "f", digits = digits_rr),
                                 title = "Relative Risk vs. non-obese")
      ),
      AF = colDef(
        name = sprintf("Attributable Fraction (p = %s)", percent(p_obesity)),
        minWidth = 260,
        cell = function(x) af_bar(x),
        sortable = TRUE,
        aggregate = "mean",
        format = colFormat(percent = TRUE, digits = digits_af)
      )
    ),
    defaultSorted = list(AF = "desc"),
    defaultExpanded = TRUE,
    bordered = FALSE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    pagination = TRUE,
    defaultPageSize = 10,
    theme = reactableTheme(
      color = "#1f2937",
      backgroundColor = "#ffffff",
      borderColor = "#e5e7eb",
      highlightColor = "#f9fafb",
      stripedColor = "#fafafa",
      cellPadding = "10px 12px",
      tableStyle = list(boxShadow = "0 4px 18px rgba(0,0,0,0.05)", borderRadius = "12px"),
      headerStyle = list(
        background = "#ffffff",
        borderBottom = "1px solid #e5e7eb",
        fontWeight = 700
      ),
      groupHeaderStyle = list(
        background = "#ffffff",
        color = "#111827",
        fontWeight = 700,
        borderBottom = "1px solid #e5e7eb"
      )
    )
  )
}

# ---- Example usage ----
obesity_rr_table(obesity_rr_demo, p_obesity = 0.30)


# install.packages(c("reactable", "htmltools", "scales")) # if needed
library(reactable)
library(htmltools)
library(scales)

# ---- Demo data (replace with your own sourced figures) ----
# prevalence as proportion in [0,1]; DW in [0,1]
# NOTE: Values below are placeholders.
morbidity_prev_dw_demo <- data.frame(
  system  = c(
    rep("Respiratory", 2),
    rep("Cancer", 3),
    rep("Cardiovascular", 4)
  ),
  disease = c(
    "Asthma", "COPD",
    "Colorectal cancer", "Breast cancer (postmenopausal)", "Endometrial cancer",
    "Coronary heart disease", "Stroke", "Heart failure", "Atrial fibrillation"
  ),
  prevalence = c(0.085, 0.030, 0.010, 0.020, 0.004, 0.045, 0.020, 0.018, 0.025),
  DW = c(0.05, 0.15, 0.23, 0.20, 0.25, 0.12, 0.30, 0.22, 0.10),
  stringsAsFactors = FALSE
)

# ---- Table factory ----
prevalence_dw_table <- function(
    df,
    digits_prev = 1,
    digits_dw   = 2,
    show_yld    = TRUE   # toggle the YLD/1,000 column
) {
  # computed YLD rate per 1,000 population (approx): prevalence * DW * 1000
  df$YLD_per_1000 <- df$prevalence * df$DW * 1000
  
  badge <- function(x, title = NULL) {
    div(
      title = title,
      style = "
        display:inline-block;padding:2px 8px;border-radius:9999px;
        font-weight:600;font-size:0.85rem;background:#f5f5f7;color:#333;
        border:1px solid #e6e6eb;
      ",
      x
    )
  }
  
  tiny_bar <- function(value, label_fun, title = NULL) {
    width_pct <- paste0(round(100 * value, 1), "%")
    div(
      title = title,
      style = "display:flex;align-items:center;gap:8px;",
      div(
        style = "
          position:relative;flex:1;height:10px;border-radius:6px;
          background:linear-gradient(90deg, rgba(0,0,0,0.08), rgba(0,0,0,0.08));
        ",
        div(style = sprintf("
          position:relative;height:10px;border-radius:6px;width:%s;
          background:rgba(0,0,0,0.3);
        ", width_pct))
      ),
      div(style="min-width:64px;text-align:right;font-variant-numeric:tabular-nums;",
          label_fun(value))
    )
  }
  
  cols <- list(
    system = colDef(name = "System", aggregate = "unique", html = TRUE),
    disease = colDef(
      name = "Disease",
      minWidth = 220,
      cell = function(x) htmltools::tags$span(style="font-weight:600;", x)
    ),
    prevalence = colDef(
      name = "Prevalence",
      minWidth = 260,
      cell = function(x) tiny_bar(
        x,
        label_fun = function(v) percent(v, accuracy = 0.1),
        title = "Share of population with condition"
      ),
      aggregate = "mean",
      sortable = TRUE,
      format = colFormat(percent = TRUE, digits = digits_prev)
    ),
    DW = colDef(
      name = "Disability/Disutility Weight",
      align = "center",
      minWidth = 200,
      cell = function(x) badge(formatC(x, format = "f", digits = digits_dw),
                               title = "Severity weight on 0–1 scale"),
      sortable = TRUE
    ),
    YLD_per_1000 = colDef(
      name = "YLD per 1,000",
      minWidth = 160,
      cell = function(x) badge(number(x, accuracy = 0.1),
                               title = "Approx. prevalence × DW × 1000"),
      sortable = TRUE,
      aggregate = "mean",
      format = colFormat(digits = 1)
    )
  )
  
  if (!show_yld) {
    cols$YLD_per_1000 <- NULL
  }
  
  reactable(
    df,
    groupBy = "system",
    columns = cols,
    defaultSorted = list(prevalence = "desc"),
    defaultExpanded = TRUE,
    bordered = FALSE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    pagination = TRUE,
    defaultPageSize = 10,
    theme = reactableTheme(
      color = "#1f2937",
      backgroundColor = "#ffffff",
      borderColor = "#e5e7eb",
      highlightColor = "#f9fafb",
      stripedColor = "#fafafa",
      cellPadding = "10px 12px",
      tableStyle = list(boxShadow = "0 4px 18px rgba(0,0,0,0.05)", borderRadius = "12px"),
      headerStyle = list(
        background = "#ffffff",
        borderBottom = "1px solid #e5e7eb",
        fontWeight = 700
      ),
      groupHeaderStyle = list(
        background = "#ffffff",
        color = "#111827",
        fontWeight = 700,
        borderBottom = "1px solid #e5e7eb"
      )
    )
  )
}

# ---- Example usage ----
prevalence_dw_table(morbidity_prev_dw_demo, show_yld = TRUE)
