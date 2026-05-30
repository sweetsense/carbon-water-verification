# ============================================================
# RQ3 re-analysis — models + maintenance (live data)
# ============================================================
suppressMessages({library(dplyr); library(tidyr); library(brms); library(readr); library(janitor)})
d <- readRDS("prepped.rds")
inst <- d$inst |> filter(!is.na(risk))
inst$risk <- factor(inst$risk, levels=c("Low Risk/Safe","Intermediate Risk","High Risk","Very High Risk/Unsafe"))

cat("===== INSTITUTIONAL MODEL (Bayesian hierarchical multinomial) =====\n")
cat("N obs:", nrow(inst), " schools:", dplyr::n_distinct(inst$school_id), "\n")
m <- brm(risk ~ period + (1|school_id), data=inst, family=categorical(),
         chains=4, cores=4, iter=2000, seed=20260529, refresh=0, silent=2)
print(summary(m))

# Extract monitoring log-odds + OR + 95% CrI per non-reference category
fx <- fixef(m)
rows <- grep("period", rownames(fx), value=TRUE)
cat("\n===== ODDS RATIOS (Monitoring vs Baseline; ref = Low Risk/Safe) =====\n")
or_tab <- data.frame(
  term = rows,
  log_odds = round(fx[rows,"Estimate"],3),
  OR = round(exp(fx[rows,"Estimate"]),4),
  CrI_low = round(exp(fx[rows,"Q2.5"]),4),
  CrI_high = round(exp(fx[rows,"Q97.5"]),4)
)
or_tab$pct_reduction <- round((1-or_tab$OR)*100,1)
print(or_tab, row.names=FALSE)
saveRDS(list(model_summary=summary(m), or_tab=or_tab), "model_out.rds")
saveRDS(m, "model.rds")

# ---- predicted probabilities ----
cat("\n===== PREDICTED PROBABILITIES (population-level) =====\n")
nd <- data.frame(period=factor(c("Baseline","Monitoring"), levels=c("Baseline","Monitoring")))
pp <- fitted(m, newdata=nd, re_formula=NA, summary=TRUE)
dimnames(pp)[[3]] -> cats
for(i in 1:2){
  cat(as.character(nd$period[i]),":\n")
  est <- pp[i,"Estimate",]; print(round(est,3))
}

# ---- Asili ----
cat("\n===== ASILI =====\n")
as <- d$asili
print(as |> count(period, risk) |> group_by(period) |> mutate(pct=round(n/sum(n)*100,1)) |> ungroup() |> as.data.frame())
cat("Note: monitoring is 100% Low Risk/Safe -> complete separation; descriptive only.\n")

# ---- DRIP trajectory ----
cat("\n===== DRIP-FUNDI TRAJECTORY (% any E. coli) =====\n")
print(d$drip_traj |> mutate(pct_contaminated=round(pct_contaminated,1)) |> as.data.frame())

# ---- Maintenance (Amazi) ----
cat("\n===== AMAZI MAINTENANCE =====\n")
LIVE<-"data"
mfile <- file.path(LIVE,"AmaziMeza_Maintenance.csv")
# pull maintenance grid if not present (same as Drive file); fall back to Drive copy
if(!file.exists(mfile)){
  drive<-"/Users/ethomas/Dropbox/Claude/rq3-repro/data/RQ3_Amazi_Meza_Maintenance_Records.csv"
  if(file.exists(drive)) mfile<-drive
}
if(file.exists(mfile)){
  mnt <- suppressWarnings(read_csv(mfile, show_col_types=FALSE)) |> clean_names()
  lc <- sapply(mnt, is.logical)
  cat("Maintenance records rows:", nrow(mnt), "\n")
  print(sort(colSums(mnt[,lc,drop=FALSE], na.rm=TRUE), decreasing=TRUE))
} else cat("(maintenance file not found — will pull live grid)\n")
cat("\nDONE\n")
