# ============================================================
# RQ3 re-analysis — config-driven prep against LIVE mWater grids
# Data source: /Users/ethomas/Dropbox/Claude/rq3-repro/mwater_live/*.csv
# ============================================================
suppressMessages({library(readr); library(dplyr); library(janitor); library(tidyr); library(stringr)})
LIVE <- "/Users/ethomas/Dropbox/Claude/rq3-repro/mwater_live"
rd <- function(f) suppressWarnings(read_csv(file.path(LIVE, f), show_col_types = FALSE, progress = FALSE)) |> clean_names()

# ---- canonical risk recode (4-level WHO) ----
relab <- function(x) dplyr::case_when(
  x %in% c("Intermediate Risk/Possibly Safe","Intermediate Risk/Probably Safe") ~ "Intermediate Risk",
  x %in% c("High Risk/Probably Unsafe","High Risk/Possibly Unsafe") ~ "High Risk",
  x == "Unsafe" ~ "Very High Risk/Unsafe",
  TRUE ~ x)
LEV4 <- c("Low Risk/Safe","Intermediate Risk","High Risk","Very High Risk/Unsafe")

# ---- DEPLOYMENT CONFIG (the agreed partitioning) ----
LS_BASE <- "July 2024 Baseline"
LS_MON  <- "September 2024 Monitoring"
AM_BASE <- "2023 Baseline"
AM_MON  <- c("January 2026 Monitoring Campaign","2025 Valver monitoring campaign") # routine, pooled
AM_EXCL <- c("July 2025 Cleaning Campaign","Contaminated Filters")                  # special campaigns -> verification narrative
AS_BASE <- "August/September 2024 Baseline"
AS_MON  <- "January 2025 - Monitoring"

# ============================================================
# INSTITUTIONAL: LifeStraw + Amazi  (baseline vs pooled monitoring)
# ============================================================
ls <- rd("LifeStraw_WQ.csv")
ls_b <- ls |> filter(deployment==LS_BASE, water_point_or_filter=="Water point") |>
  transmute(school_id=as.character(school_id), risk=relab(ecoli_mpn_risk), period="Baseline", country="Kenya")
ls_m <- ls |> filter(deployment==LS_MON,  water_point_or_filter=="Filter") |>
  transmute(school_id=as.character(school_id), risk=relab(ecoli_mpn_risk), period="Monitoring", country="Kenya")
# cohort restriction: schools present in BOTH
ls_cohort <- intersect(ls_b$school_id, ls_m$school_id)
ls_b <- ls_b |> filter(school_id %in% ls_cohort); ls_m <- ls_m |> filter(school_id %in% ls_cohort)

am <- rd("AmaziMeza_WQ.csv") |> rename(risk_raw=record_results_cbt_mpn_e, school=select_the_school_where_the_sa)
am <- am |> mutate(school_id=str_trim(school), district=str_trim(str_extract(school,"^[^.]+")))
am_b <- am |> filter(deployment==AM_BASE) |> transmute(school_id, district, risk=relab(risk_raw), period="Baseline", country="Rwanda")
am_m <- am |> filter(deployment %in% AM_MON) |> transmute(school_id, district, risk=relab(risk_raw), period="Monitoring", country="Rwanda")
am_cohort <- intersect(am_b$school_id, am_m$school_id)
am_b <- am_b |> filter(school_id %in% am_cohort); am_m <- am_m |> filter(school_id %in% am_cohort)

inst <- bind_rows(
  ls_b |> mutate(district=NA_character_), ls_m |> mutate(district=NA_character_), am_b, am_m
) |> mutate(risk=factor(risk, levels=LEV4), period=factor(period, levels=c("Baseline","Monitoring")))
inst$school_id <- paste(inst$country, inst$school_id) # avoid id collisions across countries

# ============================================================
# ASILI (DRC): baseline vs monitoring
# ============================================================
as_g <- rd("Asili_WQ.csv") |> rename(risk_raw=record_results_cbt_mpn_e)
as_b <- as_g |> filter(deployment==AS_BASE) |> transmute(risk=relab(risk_raw), period="Baseline")
as_m <- as_g |> filter(deployment==AS_MON)  |> transmute(risk=relab(risk_raw), period="Monitoring")
asili <- bind_rows(as_b, as_m) |> mutate(risk=factor(risk, levels=LEV4), period=factor(period, levels=c("Baseline","Monitoring")))

# ============================================================
# DRIP-FUNDI / MWA (Kenya): 3-point presence/absence trajectory
# ============================================================
mwa_base <- rd("MWA_baseline.csv")           # name, ecoli_pa
drip1    <- rd("DRIP_first_monitor.csv")     # P/A first monitoring
pa_col1  <- names(drip1)[1]
mwa_cur  <- rd("MWA_monitor.csv") |> mutate(risk=relab(ecoli_risk))  # MPN current
drip_traj <- tibble(
  stage = factor(c("Baseline\n(no intervention)","First monitoring\n(pre-improvement)","Current monitoring\n(post-improvement)"),
                 levels=c("Baseline\n(no intervention)","First monitoring\n(pre-improvement)","Current monitoring\n(post-improvement)")),
  n = c(nrow(mwa_base), nrow(drip1), nrow(mwa_cur)),
  pct_contaminated = c(
    mean(mwa_base$ecoli_pa=="Present")*100,
    mean(drip1[[pa_col1]]=="Present")*100,
    mean(mwa_cur$risk!="Low Risk/Safe")*100
  )
)

# ============================================================
# AMAZI WATER (Burundi): baseline household, graded MPN risk
# ============================================================
bur_base <- rd("Burundi_WQ.csv") |> filter(deployment=="Baseline March 2026") |>
  transmute(risk=relab(results_cbt_mpn_e_coli_health_risk_category))

# ============================================================
# WATER MISSION (Tanzania): baseline, hard-coded in map.html as JS baselineSamples[]
# (raw/untreated water, fecal coliform CFU/100ml; >0 or TNTC = Present, 0 = Absent).
# Presence/absence only -> joins the "any E. coli" axis, like DRIP-FUNDI.
# ============================================================
wm_base <- rd("WaterMission_baseline.csv")  # site, ecoli_cfu, ecoli_pa

# ============================================================
# HELVETAS (Madagascar): baseline (Apr 2025) vs monitoring (Internal campaign Oct 2025).
# Ignore "Post_ Baseline_test_2025". Now a before/after case.
# ============================================================
hel_raw <- rd("Helvetas_WQ.csv") |> mutate(risk=relab(ecoli_risk)) |>
  filter(risk %in% LEV4)
hel   <- hel_raw |> filter(deployment=="April 2025_ Baseline") |> transmute(risk)  # baseline (general baseline char.)
hel_b <- hel_raw |> filter(deployment=="April 2025_ Baseline") |> transmute(risk, period="Baseline")
hel_m <- hel_raw |> filter(deployment=="Internal campaign October 2025") |> transmute(risk, period="Monitoring")
helvetas <- bind_rows(hel_b, hel_m) |>
  mutate(risk=factor(risk, levels=LEV4), period=factor(period, levels=c("Baseline","Monitoring")))

# ============================================================
# GENERAL BASELINE (5 projects, baseline rounds only)
# NOTE: uses FULL baseline rounds (all available baseline data, regardless of
# follow-up) per the paper's General Baseline section — NOT cohort-restricted.
# ============================================================
am_base_full <- am |> filter(deployment==AM_BASE) |> transmute(risk=relab(risk_raw), project="Amazi Meza - Rwanda")
ls_base_full <- ls |> filter(deployment==LS_BASE, water_point_or_filter=="Water point") |>
  transmute(risk=relab(ecoli_mpn_risk), project="LifeStraw - Kenya")
base5 <- bind_rows(
  am_base_full,
  ls_base_full,
  as_b |> transmute(risk, project="Asili - DRC"),
  hel |> transmute(risk, project="Ranovola - Madagascar"),
  bur_base |> transmute(risk, project="Amazi Water - Burundi"),
  wm_base |> transmute(risk=ifelse(ecoli_pa=="Present","Generally unsafe (E. coli present)*","Low Risk/Safe"), project="Water Mission - Tanzania*"),
  mwa_base |> transmute(risk=ifelse(ecoli_pa=="Present","Generally unsafe (E. coli present)*","Low Risk/Safe"), project="DRIP-FUNDI - Kenya*")
)

saveRDS(list(inst=inst, asili=asili, drip_traj=drip_traj, hel=hel, helvetas=helvetas, wm_base=wm_base, bur_base=bur_base, base5=base5,
             ls_cohort=ls_cohort, am_cohort=am_cohort), "/Users/ethomas/Dropbox/Claude/rq3-repro/prepped.rds")

# ============================================================
# REPORT
# ============================================================
cat("================= PARTITION / COHORT =================\n")
cat(sprintf("LifeStraw cohort schools (baseline∩monitoring): %d | baseline n=%d, monitoring n=%d\n",
            length(ls_cohort), nrow(ls_b), nrow(ls_m)))
cat(sprintf("Amazi cohort schools (baseline∩monitoring): %d | baseline n=%d, monitoring n=%d\n",
            length(am_cohort), nrow(am_b), nrow(am_m)))
cat(sprintf("Institutional total rows: %d\n", nrow(inst)))
cat(sprintf("Asili baseline n=%d, monitoring n=%d\n", nrow(as_b), nrow(as_m)))

cat("\n================= GENERAL BASELINE (5 projects) =================\n")
cat(sprintf("Total baseline tests: %d\n", nrow(base5)))
pospct <- mean(base5$risk!="Low Risk/Safe")*100
vhpct  <- mean(base5$risk=="Very High Risk/Unsafe")*100
cat(sprintf("%% any E. coli (not Low/Safe): %.1f%%  [paper said 83%%]\n", pospct))
cat(sprintf("%% Very High/Unsafe: %.1f%%  [paper said 47.6%%]\n", vhpct))
print(base5 |> count(project, risk) |> pivot_wider(names_from=risk, values_from=n, values_fill=0))

cat("\n================= INSTITUTIONAL crosstab (period x risk) =================\n")
print(inst |> count(period, risk) |> pivot_wider(names_from=risk, values_from=n, values_fill=0))
cat("\n-- by country --\n")
print(inst |> count(country, period, risk) |> pivot_wider(names_from=risk, values_from=n, values_fill=0))

cat("\n================= ASILI =================\n")
print(asili |> count(period, risk) |> group_by(period) |> mutate(pct=round(n/sum(n)*100,1)) |> ungroup())

cat("\n================= DRIP-FUNDI TRAJECTORY =================\n")
print(as.data.frame(drip_traj))

cat("\n================= HELVETAS baseline =================\n")
cat(sprintf("n=%d | %% any E.coli=%.1f%% | %% Very High/Unsafe=%.1f%%\n",
            nrow(hel), mean(hel$risk!="Low Risk/Safe")*100, mean(hel$risk=="Very High Risk/Unsafe")*100))
print(hel |> count(risk))
