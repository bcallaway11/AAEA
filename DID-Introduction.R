## ----echo=FALSE---------------------------------------------------------------
library(revealEquations)


## ----echo=FALSE, results="asis"-----------------------------------------------

title <- "DID with Two Periods"

before <- "

<br>

::: {.callout-note}

### Parallel Trends Assumption

$$\\color{red}{\\E[\\Delta Y_i(0) | G_i=1]} = \\E[\\Delta Y_i(0) | G_i=0]$$

:::

<br>

Explanation: Mean path of untreated potential outcomes is the same for the treated group as for the untreated group

. . .

<span class=\"alert\">Identification: </span>Under PTA, we can identify $ATT$:"

after <- "

. . .

$\\implies ATT$ is identified can be recovered by the difference in outcomes over time (difference 1) relative to the difference in outcomes over time for the untreated group (difference 2)"

eqlist <- list(
  "ATT &= \\E[\\Delta Y_i | G_i=1] - \\E[\\Delta Y_i(0) | G_i=1]",
  "&= \\E[\\Delta Y_i | G_i=1] - \\E[\\Delta Y_i | G_i=0]"
)

step_by_step_eq(
  eqlist = eqlist,
  before = before,
  after = after,
  title = title
)


## ----message=FALSE------------------------------------------------------------
library(did)
library(BMisc)
library(twfeweights)
library(fixest)
library(modelsummary)
library(ggplot2)
load(url("https://github.com/bcallaway11/did_chapter/raw/master/mw_data_ch2.RData"))


## -----------------------------------------------------------------------------
# drops NE region and a couple of small groups
mw_data_ch2 <- subset(mw_data_ch2, (G %in% c(2004, 2006, 2007, 0)) & (region != "1"))
head(mw_data_ch2[, c("id", "year", "G", "lemp", "lpop", "lavg_pay", "region")])


## -----------------------------------------------------------------------------
# drop 2007 as these are right before fed. minimum wage change
data2 <- subset(mw_data_ch2, G != 2007 & year >= 2003)
# keep 2007 => larger sample size
data3 <- subset(mw_data_ch2, year >= 2003)


## -----------------------------------------------------------------------------
twfe_res2 <- fixest::feols(lemp ~ post | id + year,
  data = data2,
  cluster = "id"
)


## -----------------------------------------------------------------------------
modelsummary(list(twfe_res2), gof_omit = ".*")


## ----warning=FALSE------------------------------------------------------------
attgt <- did::att_gt(
  yname = "lemp",
  idname = "id",
  gname = "G",
  tname = "year",
  data = data2,
  control_group = "nevertreated",
  base_period = "universal"
)
tidy(attgt)[, 1:5] # print results, drop some extra columns


## ----fig.align="center", fig.width=10, fig.height=8, echo=FALSE---------------
ggdid(attgt, ylim = c(-.2, .05))


## -----------------------------------------------------------------------------
attO <- did::aggte(attgt, type = "group")
summary(attO)


## ----echo=FALSE, fig.align="center", fig.width=10, fig.height=8---------------
tw_res <- twfeweights::twfe_weights(attgt)
tw <- tw_res$weights_df
twfe_est <- sum(tw$weight * tw$attgt)
ggplot(
  data = tw,
  mapping = aes(x = weight, y = attgt, color = post)
) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  geom_vline(xintercept = 0, linewidth = 1.5) +
  geom_point(size = 6) +
  theme_bw() +
  ylim(c(-.15, .05)) +
  xlim(c(-.4, .7))


## ----echo=FALSE, fig.align="center", fig.width=10, fig.height=8---------------
wO_res <- attO_weights(attgt)
wO <- wO_res$weights_df
attO_est <- sum(wO$weight * wO$attgt)
ggplot(
  data = wO,
  mapping = aes(x = weight, y = attgt, color = post)
) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  geom_vline(xintercept = 0, linewidth = 1.5) +
  geom_point(shape = 18, size = 8) +
  theme_bw() +
  ylim(c(-.15, .05)) +
  xlim(c(-.4, .7))


## ----echo=FALSE, fig.align="center", fig.width=10, fig.height=8---------------
plot_df <- cbind.data.frame(tw, wOgt = wO$weight)
plot_df <- plot_df[plot_df$post == 1, ]
plot_df$g.t <- as.factor(paste0(plot_df$group, ",", plot_df$time.period))
plot_df$wTWFEgt <- plot_df$weight

ggplot(plot_df, aes(x = wTWFEgt, y = attgt, color = g.t)) +
  geom_point(size = 6) +
  theme_bw() +
  ylim(c(-.15, .05)) +
  xlim(c(-.4, .7)) +
  geom_point(aes(x = wOgt), shape = 18, size = 8) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  geom_vline(xintercept = 0, linewidth = 1.5) +
  xlab("weight")


## ----echo=FALSE, eval=FALSE---------------------------------------------------
## twfe_post <- sum(tw$weight[tw$post==1] * tw$attgt[tw$post==1])
## twfe_post
##
## # pre-treatment contamination/bias
## pre_bias <- sum(tw$weight[tw$post==0] * tw$attgt[tw$post==0])
## pre_bias
##
## twfe_bias <- twfe_est - attO_est
## pre_bias/twfe_bias # bias from pre-treatment PTA violations
## (twfe_post-attO_est)/twfe_bias # bias from TWFE weights instead of ATT^o weights


## -----------------------------------------------------------------------------
attes <- aggte(attgt, type = "dynamic")
ggdid(attes)


## ----echo=FALSE, results="asis"-----------------------------------------------
title <- "TWFE Explanation (cont'd)"

before <- "Let\'s keep going:
\\begin{align*}
  \\alpha = \\underbrace{\\Big( \\E[\\Delta Y_i | G_i=2] - \\E[\\Delta Y_i | G_i=1]\\Big)}_{\\textrm{What is this?}} w_1 + \\underbrace{\\Big( \\E[\\Delta Y_i | G_i=2] - \\E[\\Delta Y_i|G_i=\\infty]\\Big)}_{ATT(2,2)} w_\\infty
\\end{align*}
Working on the first term, we have that"

eqlist <- list(
  " & \\E[\\Delta Y_{i2} | G_i=2] - \\E[\\Delta Y_{i2} | G_i=1] \\hspace{300pt}",
  "&\\hspace{10pt} = \\E[Y_{i2}(2) - Y_{i1}(\\infty) | G_i=2] - \\E[Y_{i2}(1) - Y_{i1}(1) | G_i=1] ",
  "&\\hspace{10pt} = \\E[Y_{i2}(2) - Y_{i2}(\\infty) | G_i=2] + \\underline{\\E[Y_{i2}(\\infty) - Y_{i1}(\\infty) | G_i=2]}",
  "&\\hspace{20pt} - \\Big( \\E[Y_{i2}(1) - Y_{i2}(\\infty) | G_i=1] - \\E[Y_{i1}(1) - Y_{i1}(\\infty) | G_i=1] + \\underline{\\E[Y_{i2}(\\infty) - Y_{i1}(\\infty) | G_i=1]} \\Big)",
  "&\\hspace{10pt} = \\underbrace{ATT(2,2)}_{\\textrm{causal effect}} - \\underbrace{\\Big(ATT(1,2) - ATT(1,1)\\Big)}_{\\textrm{treatment effect dynamics}}"
)

after <- "Plug this expression back in $\\rightarrow$ "

step_by_step_eq(
  eqlist = eqlist,
  before = before,
  after = after,
  title = title,
  count = FALSE
)


## ----echo=FALSE, eval=FALSE---------------------------------------------------
## att_simple_est <- aggte(attgt, type="simple")$overall.att #-0.0646
## (att_simple_est - attO_est) / attO_est
##

## ----echo=FALSE, fig.align="center", fig.width=10, fig.height=8---------------
# calculate att^simple weights
wsimple_res <- att_simple_weights(attgt)
wsimple <- wsimple_res$weights_df
attsimple_est <- sum(wsimple$weights * wsimple$attgt)

# comparison of att^o and att^simple weights
plot_df <- cbind.data.frame(wsimple, wOgt = wO$weight)
plot_df$wsimplegt <- plot_df$weights
plot_df <- plot_df[plot_df$post == 1, ]
plot_df$g.t <- as.factor(paste0(plot_df$group, ",", plot_df$time.period))

ggplot(plot_df, aes(x = wsimplegt, y = attgt, color = g.t)) +
  geom_point(shape = 15, size = 6) +
  theme_bw() +
  ylim(c(-.15, .05)) +
  xlim(c(-.4, .7)) +
  geom_point(aes(x = wOgt), shape = 18, size = 8) +
  geom_hline(yintercept = 0, linewidth = 1.5) +
  geom_vline(xintercept = 0, linewidth = 1.5) +
  xlab("weight")
