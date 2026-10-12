# data/ —— 数据文件清单与下载说明

**本目录下的数据与官方文档不随仓库分发**（已在根目录 `.gitignore` 中忽略）。运行分析脚本前，请先按下载说明自行下载。

## 需要哪些文件

下载后放到本目录（`data/`）下，文件名需与原文件一致：

| 文件 | 内容 | 规模 |
|---|---|---|
| `STAR_Students.sav` | 主数据：学生层面，K–3 成绩、班型、学生/教师/学校背景（另含 4–8 年级与高中阶段延伸数据） | 11,601 名学生 |
| `STAR_K-3_Schools.sav` | K–3 学校层面附加信息 | 80 所 STAR 学校 |
| `STAR_High_Schools.sav` | 就读高中层面附加信息 | 学校层面 |
| `Comparison_Students.sav` | 派生数据：21 所对照学校 1–3 年级学生 | 1,780 人 |

`reference/` 内为官方文档（用户手册、事实表、后续研究 PDF），分析脚本不读取，仅作资料来源留存。

## 怎么下载

1. 打开数据集落地页：<https://doi.org/10.7910/DVN/SIWH9F>
2. 下载 **PROJECT STAR.zip**（原始 SPSS 格式数据文件）
   直链：<https://dataverse.harvard.edu/file.xhtml?fileId=666720&version=1.0>
3. 解压，并把文件按下列结构放好：

```
data/
├── README.md
├── STAR_Students.sav
├── STAR_K-3_Schools.sav
├── STAR_High_Schools.sav
├── Comparison_Students.sav
└── reference/
```

## 数据来源与引用

- 数据集：*Tennessee's Student Teacher Achievement Ratio (STAR) project*，Harvard Dataverse，V1（2008-10-07），DOI [10.7910/DVN/SIWH9F](https://doi.org/10.7910/DVN/SIWH9F)
- 许可：**CC0 1.0 Universal**（公有领域奉献）—— 可自由复制、修改与再分发
- 数据整理方：HEROS, Inc.（<http://www.heros-inc.org/data.htm>）

引用格式：

> Achilles, C. M., Bain, H. P., Bellott, F., Boyd-Zaharias, J., Finn, J., Folger, J., Johnston, J., & Word, E. (2008). *Tennessee's Student Teacher Achievement Ratio (STAR) project* [Data set]. Harvard Dataverse. https://doi.org/10.7910/DVN/SIWH9F

> Finn, J. D., Boyd-Zaharias, J., Fish, R. M., & Gerber, S. B. (2007). *Project STAR and Beyond: Database User's Guide*. HEROS, Inc.

## 使用注意

- 请勿改动原始 `.sav` 文件；子集筛选与变量重编码在 `analysis/star_kindergarten_analysis.R` 中完成。
- 本数据为**去标识的公开使用数据**，本研究属于二次分析，不涉及人类被试招募与审批。
