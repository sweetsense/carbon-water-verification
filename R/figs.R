# ============================================================
# RQ3 re-analysis — regenerate all data figures (live data)
# Writes PDFs directly into the Overleaf repo.
# ============================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(scales); library(brms); library(readr); library(janitor); library(forcats)})
OUT <- "figures"
d <- readRDS("prepped.rds")
LEV4 <- c("Low Risk/Safe","Intermediate Risk","High Risk","Very High Risk/Unsafe")
LEV5 <- c(LEV4, "Generally unsafe (E. coli present)*")
pal <- "Dark2"

# ---- Fig: baseline by project ----
b <- d$base5 |> filter(!is.na(risk)) |> mutate(risk=factor(risk, levels=LEV5))
ggplot(b, aes(project, fill=risk)) + geom_bar(position="fill") +
  scale_fill_brewer(palette=pal) + scale_y_continuous(labels=percent) + theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title="E. coli Risk by Country/Project at Baseline", x="Projects",
       y="Percentage of Observations", fill="E. coli MPN Risk")
ggsave(file.path(OUT,"ecoli_risk_by_project.pdf"), width=8, height=6)

# ---- Fig: institutional by country & period ----
inst <- d$inst |> filter(!is.na(risk)) |> mutate(risk=factor(risk, levels=LEV4))
ggplot(inst, aes(period, fill=risk)) + geom_bar(position="fill") + facet_wrap(~country) +
  scale_fill_brewer(palette=pal) + scale_y_continuous(labels=percent) + theme_minimal() +
  labs(title="E. coli MPN Risk by Country and Intervention Period", x="Intervention Period",
       y="Percentage of Observations", fill="E. coli MPN Risk")
ggsave(file.path(OUT,"ecoli_risk_schools.pdf"), width=8, height=6)

# ---- Fig: Asili before/after ----
as <- d$asili |> filter(!is.na(risk)) |> mutate(risk=factor(risk, levels=LEV4))
ggplot(as, aes(period, fill=risk)) + geom_bar(position="fill") +
  scale_fill_brewer(palette=pal) + scale_y_continuous(labels=percent) + theme_minimal() +
  labs(title="E. coli MPN Risk by Intervention Period (Asili, DRC)", x="Intervention Period",
       y="Percentage of Observations", fill="E. coli MPN Risk")
ggsave(file.path(OUT,"asili_risk.pdf"), width=8, height=6)

# ---- Fig: DRIP-FUNDI trajectory (NEW) ----
dt <- d$drip_traj |> mutate(safe = 100 - pct_contaminated)
dt_long <- dt |> select(stage, Contaminated=pct_contaminated, Safe=safe) |>
  pivot_longer(-stage, names_to="status", values_to="pct") |>
  mutate(status=factor(status, levels=c("Safe","Contaminated")))
ggplot(dt_long, aes(stage, pct/100, fill=status)) + geom_col(width=0.6) +
  geom_text(data=dt, aes(x=stage, y=1.04, label=paste0("n=",n)), inherit.aes=FALSE, size=3.2, color="grey30") +
  scale_fill_manual(values=c("Safe"="#1B9E77","Contaminated"="#D95F02")) +
  scale_y_continuous(labels=percent, limits=c(0,1.08)) + theme_minimal() +
  labs(title="DRIP-FUNDI (Kenya): E. coli detection across verification rounds",
       subtitle="Presence/absence at baseline & first monitoring; MPN risk at current monitoring",
       x=NULL, y="Percentage of samples", fill="E. coli")
ggsave(file.path(OUT,"drip_trajectory.pdf"), width=8, height=5)

# ---- Fig: predicted probabilities (needs model) ----
mpath <- "model.rds"
if(file.exists(mpath)){
  m <- readRDS(mpath)
  nd <- data.frame(period=factor(c("Baseline","Monitoring"), levels=c("Baseline","Monitoring")))
  pp <- fitted(m, newdata=nd, re_formula=NA, summary=TRUE)
  cats <- dimnames(pp)[[3]]
  rows <- list()
  for(i in 1:2) for(ct in cats) rows[[length(rows)+1]] <- data.frame(
    period=nd$period[i], risk=ct, est=pp[i,"Estimate",ct], lo=pp[i,"Q2.5",ct], hi=pp[i,"Q97.5",ct])
  pdf_df <- bind_rows(rows) |> mutate(risk=factor(risk, levels=LEV4))
  ggplot(pdf_df, aes(period, est, fill=risk)) +
    geom_col(position="dodge", color="black") +
    geom_errorbar(aes(ymin=lo, ymax=hi), position=position_dodge(width=0.9), width=0.2) +
    scale_fill_brewer(palette=pal) + theme_minimal() + theme(text=element_text(size=11)) +
    labs(title="Predicted Probabilities of E. coli Risk Levels by Monitoring Status",
         x="Intervention Period", y="Predicted Probability", fill="E. coli Risk")
  ggsave(file.path(OUT,"predicted_prob.pdf"), width=8, height=6)
  cat("predicted_prob.pdf written\n")
} else cat("model.rds not found — run model first for predicted_prob.pdf\n")

cat("FIGS DONE\n")
