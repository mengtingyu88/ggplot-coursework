# ISSS608 Take-home Exercise 1
### Motor Insurance Portfolio · Visual Analytics

A Quarto project producing **two deliverables** from a real-world 354K-row Spanish motor-insurance portfolio:

1. **Technical Report** — `Take-home_Ex/Take-home_Ex01/index.qmd` → HTML
2. **Executive Summary** — `Take-home_Ex/Take-home_Ex01/exec_summary.qmd` → revealjs HTML slides

---

## 🚀 Quick start (3 commands)

```bash
# 1. install R packages (one-time, ~5 minutes)
Rscript Take-home_Ex/Take-home_Ex01/setup_packages.R

# 2. (optional) test the data wrangling end-to-end
Rscript Take-home_Ex/Take-home_Ex01/_test_run.R

# 3. render the whole site
quarto render
```

The site lands in `_site/`.

---

## 📁 Project layout

```
.
├── _quarto.yml                          # site config (navbar, theme)
├── index.qmd                            # site home page
├── about.qmd
├── styles.css                           # site-wide CSS
│
├── Take-home_Ex/Take-home_Ex01/
│   ├── index.qmd                        # ⭐ technical report (the main deliverable)
│   ├── exec_summary.qmd                 # ⭐ revealjs executive summary
│   ├── custom.scss                      # report styling (Oswald + Inter)
│   ├── custom.css                       # fine-tuning
│   ├── slides_custom.scss               # slide-deck styling (matches White Research Center)
│   ├── setup_packages.R                 # one-click R package install
│   ├── _test_run.R                      # verifies wrangling + key charts
│   │
│   ├── data/
│   │   ├── insurance_raw.csv            # the original CSV (drop here)
│   │   └── analytical_sandbox.rds       # cached wrangled data (auto-generated)
│   │
│   └── img/                             # preview PNGs for the executive slides
│
└── Hands-on_Ex/Hands-on_Ex0[1-4]/       # existing hands-on coursework (unchanged)
```

---

## 📦 Data

The project expects the raw Mendeley CSV at:

```
Take-home_Ex/Take-home_Ex01/data/insurance_raw.csv
```

The file can be **either** semicolon-delimited (original) **or** comma-delimited — the wrangling code auto-detects.

---

## 🧰 R packages needed

The `setup_packages.R` script installs all of these via `pacman`. They are:

| Group | Packages |
|---|---|
| Core | `tidyverse`, `lubridate`, `scales` |
| Reporting | `knitr`, `kableExtra`, `glue` |
| ggplot2 extensions | `ggridges`, `ggdist`, `ggrepel`, `ggstatsplot`, `ggcorrplot`, `ggpubr`, `patchwork`, `ggh4x`, `ggtext` |
| Interactive | `plotly`, `ggiraph`, `crosstalk`, `DT` |
| Statistics | `performance`, `FSA` |

If any package fails to install, `pacman::p_load()` will report which one — usually a system-library issue on Linux (`libcurl`, `libxml2`).

---

## 🎨 Visual style

Both deliverables use the same **"White Research Center"** minimalist-academic identity:

- **Background**: light grey `#F2F2F2` with faint diagonal geometric line accents
- **Headings**: Oswald (uppercase, weight 700), heavy borders
- **Body**: Inter (400/500), grey `#1a1a1a`
- **Accent**: a single red `#C8102E` for emphasis only
- **Success**: green `#2E7D32`

All chart colours are explicitly set to follow this palette — no rainbow defaults.

---

## 📑 Key findings (one-line preview)

| Theme | Finding |
|---|---|
| 💰 Profitability | **COMP-N policies** crossed **100% LR in 2024** — re-pricing overdue |
| 💸 Adverse retention | **Cancelled policies run 128% LR** vs Active 64% (1.7× claim frequency) |
| 🎯 Tail risk | **Gini = 0.956** — top 1% of policies generate ~50% of losses |
| 📉 Hidden driver | LR climb is a **pricing problem**, not loss inflation (premium/exposure ↓9%) |
| ⚖️ Class leakage | **Bonus = Neutral (81%)** runs *worse* than Bad (79%) |

---

## 🔁 Reproducibility

- `_quarto.yml` sets `execute.freeze: auto` — already-rendered chunks won't re-run unless their source changes.
- The wrangled `analytical_sandbox.rds` is cached on first render; downstream chunks read from it.
- `sessionInfo()` is captured at the end of the report.

---

## 📝 Notes

- The raw CSV uses **semicolons** (`;`) as delimiter — typical for European exports — and decimal **periods**. The import code handles both.
- The cookbook lists `age_driving_licence` as the *year of licence issuance*, but the observed range (0–80) reveals it is actually the *number of years held*. This is renamed to `years_licence_held` in the sandbox.
- 49 records with `total_premium = 0` **and** `total_exposure = 0` are removed as inactive shells.

---

*ISSS608 Visual Analytics & Applications · Take-home Exercise 1 · 2026*
