# UC Davis STA 207 Final Report · Project STAR 小班教学效应研究

课程终期报告（UC Davis STA 207, Prof. Shizhe Chen，2025 Winter）· 独立完成

## 数据

- **Project STAR**（Tennessee, 1985–1989）：79 校 / 11,601 名学生 / 379 个变量，K–3 随机分配三种班型（小班 13–17 人 / 常规 22–26 人 / 常规 + 全职助教）
- 来源：Harvard Dataverse 官方发布（Achilles et al. 2008, DOI `10.7910/DVN/SIWH9F`），`STAR_Students.sav` 为原始数据

## 文件

| 文件 | 说明 |
|---|---|
| `final-analysis-progress.Rmd` | 分析代码（数据清理 → 模型 → 诊断） |
| `STAR_Students.sav` | 原始数据 |
| `starUsersGuide.pdf` | STAR 数据用户手册 |

## 方法

- 随机化检验与平衡性诊断，识别并调整基线不平衡
- 多层次回归 + 交互项检验群体异质性；分位数回归考察效应分布（重点关注中低分学生）
- 模型诊断：残差分析、Shapiro-Wilk、Q-Q 图、Breusch-Pagan（稳健标准误）、VIF

## 主要发现

- 小班教学显著提升阅读与数学成绩（Cohen's d = 0.18 / 0.16），多次稳健性检验结论一致
- 男生、黑人及经济弱势群体获益更大，支持教育公平含义
