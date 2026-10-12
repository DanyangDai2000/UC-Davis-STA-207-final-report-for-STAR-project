# ============================================================================
#  Project STAR — 小班教学对幼儿园学业成绩的因果效应
#  分析脚本（依据《STAR project report》重建）
#
#  数据 : Project STAR 第一阶段（田纳西州班级规模实验，1985–1989）
#         Harvard Dataverse, DOI 10.7910/DVN/SIWH9F
#  作者 : Danyang Dai
#  用法 : 1) 将 STAR_Students.sav 放入 DATA_DIR（默认 ./data）
#         2) Rscript star_kindergarten_analysis.R
#
#  复现说明：
#    · 主效应模型（Model 1–4）的系数 / 标准误 / R2 / RMSE 与报告逐位一致；
#    · 随机化平衡检验、异质性、分位数回归、纵向与剂量-反应，
#      按报告 §2.3 的模型设定重建；
#    · §9 对应报告 §3.5「敏感性与稳健性分析」的七类检验
#      （标准误设定 / 连续规模替代 / 学校固定效应 / 子样本 / 极端值 / 多重插补 / 伪实验）。
# ============================================================================

## 0. 依赖与全局设置 ---------------------------------------------------------
pkgs <- c("haven", "dplyr", "tidyr", "ggplot2", "modelsummary",
          "sandwich", "lmtest", "car", "quantreg", "mice")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, function(p)
  suppressPackageStartupMessages(library(p, character.only = TRUE))))

DATA_DIR <- "data"                 # STAR_Students.sav 所在目录
FIG_DIR  <- "report/figures"       # 输出图片目录
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
set.seed(20260317)

STARS <- c("+" = .1, "*" = .05, "**" = .01, "***" = .001)   # 星号口径同报告
GOF   <- c("nobs", "r.squared", "r.squared.adj", "rmse")

## 1. 数据读取与分层样本 -----------------------------------------------------
raw <- haven::read_sav(file.path(DATA_DIR, "STAR_Students.sav"))

d0 <- raw %>% filter(FLAGSGK == 1)          # 仅保留幼儿园阶段参与者的记录

# 变量重编码（编码依据 STAR 原始变量的取值标签）
recode_vars <- function(df) {
  df %>% mutate(
    Small   = as.numeric(gkclasstype == 1),          # 小班
    Aide    = as.numeric(gkclasstype == 3),          # 常规班 + 全职助教
    Male    = as.numeric(gender == 1),               # 1=男，2=女
    NonFree = as.numeric(gkfreelunch == 2),          # 1=免费午餐，2=非免费午餐
    Texp    = as.numeric(gktyears),                  # 教师教龄（年）
    # 种族：原值 1=白 2=黑 3=亚 4=西语裔 5=原住民 6=其他；以「亚裔」为参照
    race_f  = factor(race, levels = c(3, 1, 2, 4, 5, 6),
                     labels = c("Asian", "White", "Black", "Hispanic",
                                "NativeAmerican", "OtherRace")),
    # 教师学历：原值 2=学士 3=硕士 4=硕士+ 5=专家证；以「学士」为参照
    # （第 4/5 档沿用报告表中的 Specialist / Doctorate 叫法）
    deg_f   = factor(gkthighdegree, levels = c(2, 3, 4, 5),
                     labels = c("Bachelors", "Masters", "Specialist", "Doctorate")),
    # 学校位置：1=市中心 2=郊区 3=农村 4=城镇；以「市中心」为参照
    urban_f = factor(gksurban, levels = c(1, 2, 3, 4),
                     labels = c("InnerCity", "Suburban", "Rural", "SmallTown"))
  )
}
d0 <- recode_vars(d0)

# 分层样本（报告 §2.1.2）
base <- d0   %>% filter(!is.na(gktreadss), !is.na(gktmathss))               # 基础效应
fair <- base %>% filter(!is.na(gender), !is.na(race), !is.na(gkfreelunch))  # 教育公平
main <- fair %>% filter(!is.na(gktyears), !is.na(gkthighdegree))            # 主模型 3/4

cat(sprintf("样本量 | 基础 %d | 教育公平 %d | 主模型 %d\n",
            nrow(base), nrow(fair), nrow(main)))

## 2. 描述性统计与随机化平衡检验 --------------------------------------------
# 2.1 班型分布与班级规模
base %>%
  mutate(class = factor(gkclasstype, 1:3, c("Small", "Regular", "Aide"))) %>%
  group_by(class) %>%
  summarise(n = n(), pct = round(100 * n() / nrow(base), 1),
            size_mean = round(mean(gkclasssize), 1), .groups = "drop") %>%
  print()

# 2.2 结果变量描述
base %>% summarise(
  read_mean = mean(gktreadss), read_sd = sd(gktreadss), read_med = median(gktreadss),
  math_mean = mean(gktmathss), math_sd = sd(gktmathss), math_med = median(gktmathss)) %>%
  print()

# 2.3 组间标准化均值差（SMD）——连续变量用合并标准差，二元变量用比例差
smd_num <- function(x, g, a, b) {
  xa <- x[g == a]; xb <- x[g == b]
  sp <- sqrt(((length(xa) - 1) * var(xa) + (length(xb) - 1) * var(xb)) /
             (length(xa) + length(xb) - 2))
  (mean(xa) - mean(xb)) / sp
}
smd_bin <- function(x, g, a, b) {
  pa <- mean(x[g == a] == 1); pb <- mean(x[g == b] == 1); p <- (pa + pb) / 2
  (pa - pb) / sqrt(p * (1 - p))
}
b <- base %>% transmute(
  gkclasstype,
  `男生比例`       = as.numeric(gender == 1),
  `白人比例`       = as.numeric(race == 1),
  `黑人比例`       = as.numeric(race == 2),
  `免费午餐`       = as.numeric(gkfreelunch == 1),
  `教师男`         = as.numeric(gktgen == 1),
  `教师硕士及以上` = as.numeric(gkthighdegree >= 3),
  `城市学校`       = as.numeric(gksurban == 1),
  `郊区学校`       = as.numeric(gksurban == 2),
  `教师经验`       = as.numeric(gktyears))
smd_rows <- lapply(setdiff(names(b), "gkclasstype"), function(nm) {
  x <- b[[nm]]
  f <- if (nm == "教师经验") smd_num else smd_bin
  data.frame(变量 = nm,
             "小班vs常规" = f(x, b$gkclasstype, 1, 2),
             "小班vs助教" = f(x, b$gkclasstype, 1, 3),
             "常规vs助教" = f(x, b$gkclasstype, 2, 3),
             check.names = FALSE)
})
smd_tab <- do.call(rbind, smd_rows)
smd_tab[, -1] <- round(smd_tab[, -1], 3)
print(smd_tab)

## 3. 政策效应主模型（OLS） --------------------------------------------------
# 逐步纳入控制变量：M1 无控制 → M2 学生 → M3 教师 → M4 学校
mods_read <- list(
  "Model 1" = lm(gktreadss ~ Small + Aide, data = base),
  "Model 2" = lm(gktreadss ~ Small + Aide + Male + race_f + NonFree, data = fair),
  "Model 3" = lm(gktreadss ~ Small + Aide + Male + race_f + NonFree + Texp + deg_f, data = main),
  "Model 4" = lm(gktreadss ~ Small + Aide + Male + race_f + NonFree + Texp + deg_f + urban_f, data = main))
mods_math <- list(
  "Model 1" = lm(gktmathss ~ Small + Aide, data = base),
  "Model 2" = lm(gktmathss ~ Small + Aide + Male + race_f + NonFree, data = fair),
  "Model 3" = lm(gktmathss ~ Small + Aide + Male + race_f + NonFree + Texp + deg_f, data = main),
  "Model 4" = lm(gktmathss ~ Small + Aide + Male + race_f + NonFree + Texp + deg_f + urban_f, data = main))

coef_lab <- c(
  "Small" = "Small Class", "Aide" = "Aide Class", "Male" = "Male",
  "race_fWhite" = "White", "race_fBlack" = "Black", "race_fHispanic" = "Hispanic",
  "race_fNativeAmerican" = "Native American", "race_fOtherRace" = "Other Race",
  "NonFree" = "Non-free Lunch", "Texp" = "Teacher Experience",
  "deg_fMasters" = "Master's", "deg_fSpecialist" = "Specialist",
  "deg_fDoctorate" = "Doctorate", "urban_fSuburban" = "Suburban",
  "urban_fRural" = "Rural", "urban_fSmallTown" = "Small Town")

modelsummary(mods_read, title = "Kindergarten Reading Achievement",
             stars = STARS, statistic = "std.error",
             gof_map = GOF, coef_map = coef_lab, output = "markdown")
modelsummary(mods_math, title = "Kindergarten Math Achievement",
             stars = STARS, statistic = "std.error",
             gof_map = GOF, coef_map = coef_lab, output = "markdown")

# 异方差稳健标准误（对照报告「统一采用稳健标准误」的说明）
modelsummary(mods_read[["Model 4"]], vcov = "HC3", stars = STARS,
             statistic = "std.error", gof_map = GOF, coef_map = coef_lab,
             title = "Reading M4 (HC3 robust SE)", output = "markdown")

## 4. 模型诊断 ---------------------------------------------------------------
diag_model <- function(m, tag) {
  cat("\n==== 诊断：", tag, "====\n")
  cat("Shapiro-Wilk 正态性 p =", signif(shapiro.test(resid(m))$p.value, 4), "\n")
  cat("Breusch-Pagan 异方差 p =", signif(lmtest::bptest(m)$p.value, 4), "\n")
  cat("VIF (GVIF^(1/(2*Df))) 最大值 =",
      round(max(car::vif(m)[, 3]), 3), "\n")
}
diag_model(mods_read[["Model 4"]], "阅读 Model 4")
diag_model(mods_math[["Model 4"]], "数学 Model 4")

for (nm in c("reading", "math")) {
  m <- if (nm == "reading") mods_read[["Model 4"]] else mods_math[["Model 4"]]
  png(file.path(FIG_DIR, sprintf("diag_%s.png", nm)), width = 1100, height = 520)
  par(mfrow = c(1, 2)); plot(m, which = 1); plot(m, which = 2); dev.off()
}

## 5. 标准化效应量 Cohen's d -------------------------------------------------
cohen_d <- function(x, g, a = 1, b = 2) {
  xa <- x[g == a]; xb <- x[g == b]
  sp <- sqrt(((length(xa) - 1) * var(xa) + (length(xb) - 1) * var(xb)) /
             (length(xa) + length(xb) - 2))
  (mean(xa) - mean(xb)) / sp
}
cat(sprintf("Cohen's d（小班 vs 常规）| 阅读 %.3f | 数学 %.3f\n",
            cohen_d(base$gktreadss, base$gkclasstype),
            cohen_d(base$gktmathss, base$gkclasstype)))

## 6. 异质性效应（性别 / 社会经济地位 / 种族） -------------------------------
# 6.1 交互项：Y ~ Small + Aide + Group + Small:Group + 控制
het_specs <- list(
  list(y = "gktreadss", g = "Male"),
  list(y = "gktmathss", g = "Male"),
  list(y = "gktreadss", g = "NonFree"),
  list(y = "gktmathss", g = "NonFree"))
for (s in het_specs) {
  f <- as.formula(sprintf("%s ~ Small + Aide + %s + Small:%s", s$y, s$g, s$g))
  cat(sprintf("\n[交互] %s × %s\n", s$y, s$g))
  print(round(coef(lm(f, data = fair)), 3))
}
# 种族异质性（黑人 vs 白人）
cat("\n[分层] 黑人学生阅读/数学\n")
print(round(coef(lm(gktreadss ~ Small + Aide, data = filter(fair, race == 2))), 3))
print(round(coef(lm(gktmathss ~ Small + Aide, data = filter(fair, race == 2))), 3))

# 6.2 分组回归：性别
cat("\n[分层] 男生 / 女生\n")
print(round(coef(lm(gktreadss ~ Small + Aide, data = filter(fair, Male == 1))), 3))
print(round(coef(lm(gktreadss ~ Small + Aide, data = filter(fair, Male == 0))), 3))

# 6.3 成绩差距与缩小率（不依赖 attach，直接传入变量向量）
gap_sex <- function(y, g, sex) {
  d_reg <- mean(y[g == 2 & sex == 2]) - mean(y[g == 2 & sex == 1])
  d_sml <- mean(y[g == 1 & sex == 2]) - mean(y[g == 1 & sex == 1])
  c(regular = d_reg, small = d_sml, reduction = (d_reg - d_sml) / d_reg * 100)
}
cat("\n[差距] 阅读性别差距\n"); print(round(gap_sex(base$gktreadss, base$gkclasstype, base$gender), 2))
cat("[差距] 数学性别差距\n"); print(round(gap_sex(base$gktmathss, base$gkclasstype, base$gender), 2))

## 7. 分位数回归 -------------------------------------------------------------
taus <- c(.1, .25, .5, .75, .9)
qr_tab <- function(y) {
  m <- quantreg::rq(as.formula(sprintf("%s ~ Small + Aide", y)), tau = taus, data = base)
  t(sapply(seq_along(taus), function(i) coef(m)[, i]))
}
colnames_tau <- paste0("τ=", taus)
res_qr_r <- qr_tab("gktreadss"); colnames(res_qr_r) <- colnames_tau
res_qr_m <- qr_tab("gktmathss"); colnames(res_qr_m) <- colnames_tau
cat("\n[分位数] 阅读\n"); print(round(res_qr_r, 3))
cat("[分位数] 数学\n");   print(round(res_qr_m, 3))

## 8. 时间持续性：滞后因变量与剂量-反应 --------------------------------------
# 纵向样本：K、G1、G2、G3 四期阅读与数学成绩均完整
# （对应报告 §2.1.2 的纵向分析样本 N = 2619）
long <- base %>% filter(!is.na(g1treadss), !is.na(g2treadss), !is.na(g3treadss),
                        !is.na(g1tmathss), !is.na(g2tmathss), !is.na(g3tmathss))
cat(sprintf("\n纵向样本 N = %d\n", nrow(long)))

# 控制集与主模型（Model 4）一致：基线成绩 + 学生/教师/学校特征
# （控制变量有缺失的样本由 lm 自动剔除，实际进入约 2605 例）
long_ctrl <- "Male + race_f + NonFree + Texp + deg_f + urban_f"
lm_long <- function(y, base_var)
  lm(as.formula(sprintf("%s ~ Small + Aide + %s + %s", y, base_var, long_ctrl)),
     data = long)
cat("[G1 阅读] "); print(round(coef(lm_long("g1treadss", "gktreadss")), 3))
cat("[G3 阅读] "); print(round(coef(lm_long("g3treadss", "gktreadss")), 3))
cat("[G1 数学] "); print(round(coef(lm_long("g1tmathss", "gktmathss")), 3))
cat("[G3 数学] "); print(round(coef(lm_long("g3tmathss", "gktmathss")), 3))

# 剂量-反应：参与小班年数 0–4（参照组 = 0 年），控制基线成绩
print(round(coef(lm(g3treadss ~ factor(yearssmall) + gktreadss, data = long)), 3))

## 9. 稳健性检验（对应报告 §3.5） -------------------------------------------
m4_r <- mods_read[["Model 4"]]
m4_m <- mods_math[["Model 4"]]
CTRL_F <- "Male + race_f + NonFree + Texp + deg_f + urban_f"

# 9.1 标准误设定：经典 / HC3 异方差稳健 / 学校层面聚类
se_of <- function(m) c(
  经典 = coef(summary(m))["Small", 2],
  HC3  = sqrt(diag(sandwich::vcovHC(m, type = "HC3")))["Small"],
  聚类 = sqrt(diag(sandwich::vcovCL(m, cluster = ~ gkschid, type = "HC1")))["Small"])
cat("\n[9.1] Small 标准误（经典 / HC3 / 学校聚类）\n")
print(rbind(阅读 = se_of(m4_r), 数学 = se_of(m4_m)))

# 9.2 替代设定：以连续班级规模替代班型哑变量
size_r <- lm(as.formula(paste("gktreadss ~ gkclasssize +", CTRL_F)), data = main)
size_m <- lm(as.formula(paste("gktmathss ~ gkclasssize +", CTRL_F)), data = main)
cat(sprintf("[9.2] 班级规模系数 | 阅读 %.4f (se %.4f) | 数学 %.4f (se %.4f)\n",
            coef(size_r)["gkclasssize"], sqrt(diag(vcov(size_r)))["gkclasssize"],
            coef(size_m)["gkclasssize"], sqrt(diag(vcov(size_m)))["gkclasssize"]))

# 9.3 学校固定效应
fe_r <- lm(as.formula(paste("gktreadss ~ Small + Aide +", CTRL_F, "+ factor(gkschid)")), data = main)
fe_m <- lm(as.formula(paste("gktmathss ~ Small + Aide +", CTRL_F, "+ factor(gkschid)")), data = main)
cat(sprintf("[9.3] 学校固定效应 | 阅读 Small %.3f (se %.3f) | 数学 Small %.3f (se %.3f)\n",
            coef(fe_r)["Small"], sqrt(diag(vcov(fe_r)))["Small"],
            coef(fe_m)["Small"], sqrt(diag(vcov(fe_m)))["Small"]))

# 9.4 子样本：按学校地理位置
for (u in 1:4) {
  s <- main %>% filter(gksurban == u)
  rr <- lm(as.formula(paste("gktreadss ~ Small + Aide +", CTRL_F)), data = s)
  mm <- lm(as.formula(paste("gktmathss ~ Small + Aide +", CTRL_F)), data = s)
  cat(sprintf("[9.4] 学校位置 %d（N=%4d）Small | 阅读 %7.3f (se %.3f) | 数学 %7.3f (se %.3f)\n",
              u, nrow(s), coef(rr)["Small"], sqrt(diag(vcov(rr)))["Small"],
              coef(mm)["Small"], sqrt(diag(vcov(mm)))["Small"]))
}

# 9.5 极端值：剔除 |标准化残差| > 3
keep_r <- abs(rstandard(m4_r)) <= 3
keep_m <- abs(rstandard(m4_m)) <= 3
sub_r <- lm(as.formula(paste("gktreadss ~ Small + Aide +", CTRL_F)), data = main[keep_r, ])
sub_m <- lm(as.formula(paste("gktmathss ~ Small + Aide +", CTRL_F)), data = main[keep_m, ])
cat(sprintf("[9.5] 剔除极端值 | 阅读 剔除 %d 例 → Small %.3f (N=%d) | 数学 剔除 %d 例 → Small %.3f (N=%d)\n",
            sum(!keep_r), coef(sub_r)["Small"], sum(keep_r),
            sum(!keep_m), coef(sub_m)["Small"], sum(keep_m)))

# 9.6 多重插补（全样本，m = 5，Rubin 合并）
imp <- mice::mice(
  d0 %>% select(gktreadss, gktmathss, Small, Aide, Male, race, gkfreelunch,
                gktyears, gkthighdegree, gksurban),
  m = 5, seed = 20260317, printFlag = FALSE)
cat("\n[9.6] 多重插补 · 阅读 主效应\n")
print(summary(mice::pool(with(imp, lm(gktreadss ~ Small + Aide)))))
cat("[9.6] 多重插补 · 数学 主效应\n")
print(summary(mice::pool(with(imp, lm(gktmathss ~ Small + Aide)))))

# 9.7 伪实验：校内随机重排班型（500 次）
set.seed(20260317)
B <- 500
obs <- c(阅读 = coef(m4_r)["Small"], 数学 = coef(m4_m)["Small"])
cls <- main %>% group_by(gktchid) %>%
  summarise(sch = first(gkschid), ct = first(gkclasstype), .groups = "drop")
perm <- replicate(B, {
  ct <- cls %>% group_by(sch) %>% mutate(ct = sample(ct)) %>% pull(ct)
  mp <- setNames(ct, cls$gktchid)
  ci <- mp[as.character(main$gktchid)]
  d  <- main
  d$Small <- as.numeric(ci == 1); d$Aide <- as.numeric(ci == 3)
  c(coef(lm(as.formula(paste("gktreadss ~ Small + Aide +", CTRL_F)), data = d))["Small"],
    coef(lm(as.formula(paste("gktmathss ~ Small + Aide +", CTRL_F)), data = d))["Small"])
})
cat(sprintf("\n[9.7] 伪实验（%d 次校内重排）观察值 阅读 %.3f / 数学 %.3f\n", B, obs[1], obs[2]))
cat(sprintf("      零分布 阅读 mean %.3f sd %.3f | 数学 mean %.3f sd %.3f\n",
            mean(perm[1, ]), sd(perm[1, ]), mean(perm[2, ]), sd(perm[2, ])))
cat(sprintf("      经验 p 阅读 %.4f / 数学 %.4f\n",
            (1 + sum(abs(perm[1, ]) >= abs(obs[1]))) / (B + 1),
            (1 + sum(abs(perm[2, ]) >= abs(obs[2]))) / (B + 1)))

cat("\n完成。图表输出目录：", FIG_DIR, "\n")
