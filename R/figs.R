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
  clean_cat <- function(x) { x <- gsub("^P\\(Y = ", "", x); gsub("\\)$", "", x) }  # "P(Y = Low Risk/Safe)" -> "Low Risk/Safe"
  rows <- list()
  for(i in 1:2) for(ct in cats) rows[[length(rows)+1]] <- data.frame(
    period=nd$period[i], risk=clean_cat(ct), est=pp[i,"Estimate",ct], lo=pp[i,"Q2.5",ct], hi=pp[i,"Q97.5",ct])
  pdf_df <- bind_rows(rows) |> mutate(risk=factor(risk, levels=LEV4))
  stopifnot(!any(is.na(pdf_df$risk)))   # guard: every category must map to a known level
  dodge <- position_dodge(width=0.7)
  ggplot(pdf_df, aes(risk, est, fill=period)) +
    geom_col(position=dodge, width=0.65) +
    geom_errorbar(aes(ymin=lo, ymax=hi), position=dodge, width=0.18, linewidth=0.4, colour="grey30") +
    geom_text(aes(label=sprintf("%.2f", est), y=hi), position=dodge, vjust=-0.6, size=3, colour="grey25") +
    scale_fill_manual(values=c(Baseline="#D95F02", Monitoring="#1B9E77")) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2), expand=expansion(mult=c(0,0.08))) +
    theme_minimal(base_size=12) +
    theme(panel.grid.major.x=element_blank(), legend.position="top",
          legend.title=element_blank(), axis.text.x=element_text(size=10)) +
    labs(title="Predicted probability of each E. coli risk level, by intervention period",
         subtitle="Institutional filter projects (model-estimated; error bars are 95% credible intervals)",
         x="E. coli risk category", y="Predicted probability")
  ggsave(file.path(OUT,"predicted_prob.pdf"), width=8, height=5)
  cat("predicted_prob.pdf written\n")
} else cat("model.rds not found — run model first for predicted_prob.pdf\n")

cat("FIGS DONE\n")
