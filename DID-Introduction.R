# load packages
library(revealEquations)
library(did)
library(BMisc)
library(twfeweights)
library(fixest)
library(modelsummary)
library(ggplot2)
load(url("https://github.com/bcallaway11/did_chapter/raw/master/mw_data_ch2.RData"))


## -----------------------------------------------------------------------------
## setup data
## -----------------------------------------------------------------------------
# drops NE region and a couple of small groups
mw_data_ch2 <- subset(mw_data_ch2, (G %in% c(2004, 2006, 2007, 0)) & (region != "1"))
head(mw_data_ch2[, c("id", "year", "G", "lemp", "lpop", "lavg_pay", "region")])

# drop 2007 as these are right before fed. minimum wage change
data2 <- subset(mw_data_ch2, G != 2007 & year >= 2003)
# keep 2007 => larger sample size
data3 <- subset(mw_data_ch2, year >= 2003)


## -----------------------------------------------------------------------------
## twfe results
## -----------------------------------------------------------------------------
twfe_res2 <- fixest::feols(lemp ~ post | id + year,
  data = data2,
  cluster = "id"
)

modelsummary(list(twfe_res2), gof_omit = ".*")


## -----------------------------------------------------------------------------
## estimate att(g,t)
## -----------------------------------------------------------------------------
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

ggdid(attgt, ylim = c(-.2, .05))


## -----------------------------------------------------------------------------
## overall att
## -----------------------------------------------------------------------------
attO <- did::aggte(attgt, type = "group")
summary(attO)


## -----------------------------------------------------------------------------
## twfe as a weighted average of att(g,t)
## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
## att^O as a weighted average of att(g,t)
## -----------------------------------------------------------------------------
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

# comparison of att^o and twfe weights
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



## -----------------------------------------------------------------------------
## event study
## -----------------------------------------------------------------------------
attes <- aggte(attgt, type = "dynamic")
ggdid(attes)


## -----------------------------------------------------------------------------
## "simple" aggregation
## -----------------------------------------------------------------------------
wsimple_res <- att_simple_weights(attgt)
wsimple <- wsimple_res$weights_df
attsimple_est <- sum(wsimple$weight * wsimple$attgt)

# comparison of att^o and att^simple weights
plot_df <- cbind.data.frame(wsimple, wOgt = wO$weight)
plot_df$wsimplegt <- plot_df$weight
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
