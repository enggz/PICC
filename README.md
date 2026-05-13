---
title: "Tesss"
output:
  flexdashboard::flex_dashboard:
    vertical_layout: scroll
    theme: yeti
---


```{r, echo=FALSE, results="asis"}
htmltools::HTML('<link rel="stylesheet" href="assets/styles.css">')
```

```{r, echo=FALSE, results="asis"}
htmltools::HTML('<script src="assets/scripts.js"></script>')
```

```{r setup, include=FALSE}
packages <- c(
  "flexdashboard",
  "tidyverse",
  "highcharter",
  "viridis",
  "DT",
  "gapminder",
  "jsonlite"
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load library
library(flexdashboard)
library(tidyverse)
library(highcharter)
library(viridis)
library(DT)
library(gapminder)
library(jsonlite)
library(plm)
library(dplyr)
library(tidyr)
library(purrr)
library(htmltools)
library(GGally)
```

Members {data-orientation=rows}
=======================================================================


Summary of Basic Statistics {data-orientation=rows}
=======================================================================
  
## Column {.tabset .tabset-fade data-height=520}
-----------------------------------------------------------------------

### Dataset

```{r message=FALSE, warning=FALSE}
# Pemanggilan data mentah dengan penanganan kolom duplikat
df_raw <- readr::read_csv(
  "https://raw.githubusercontent.com/enggz/PICC/refs/heads/main/PAS_JAMBORE.csv",
  skip = 1, 
  show_col_types = FALSE
) 

# Memberikan Nama Kolom yang Unik 
names(df_raw)[1:7]   <- c("Provinsi", "Kab_Kota", paste0("IPM_", 2020:2024))
names(df_raw)[8:12]  <- paste0("RLS_", 2020:2024)
names(df_raw)[23:27] <- paste0("AHH_", 2020:2024)
names(df_raw)[43:47] <- paste0("IPG_", 2020:2024)
names(df_raw)[48:52] <- paste0("PPP_", 2020:2024)

# Cleaning: Fill Provinsi, Ganti Koma ke Titik, & Konversi Numerik
df_clean <- df_raw %>%
  tidyr::fill(Provinsi, .direction = "down") %>%
  mutate(across(3:ncol(.), ~as.numeric(str_replace_all(as.character(.), ",", ".")))) %>%
  filter(!is.na(IPM_2024))


# Ini penting agar Tahun menjadi satu kolom untuk regresi & grafik tren
df_panel <- df_clean %>%
  pivot_longer(
    cols = matches("2020|2021|2022|2023|2024"),
    names_to = c(".value", "Tahun"),
    names_sep = "_"
  ) %>%
  mutate(Tahun = as.numeric(Tahun)) %>%
  select(Provinsi, Kab_Kota, Tahun, IPM, RLS, AHH, IPG, PPP)

# Tampilkan Tabel Interaktif (Seluruh Tahun 2020-2024)
datatable(
  df_panel,
  rownames = FALSE,
  filter = 'top',
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#393940', 'color': 'white'});",
      "}"
    )
  ),
  caption = htmltools::tags$caption(
    style = "caption-side: top; text-align: center; font-weight: bold; font-size: 16px; color: #2c3e50;",
    "Dataset Penuh Determinan IPM Indonesia (2020-2024)"
  )
) %>%
  formatRound(columns = c('IPM', 'RLS', 'AHH', 'IPG'), digits = 2) %>%
  formatCurrency('PPP', currency = "Rp ", interval = 3, mark = ".", digits = 0)
```

### Data yang digunakan

```{r}
# Mengambil data tahun terbaru (2024) untuk melihat korelasi terkini
df_2024 <- df_panel %>% filter(Tahun == 2024) %>% 
  select(IPM, RLS, AHH, IPG, PPP)

# Visualisasi Korelasi
ggpairs(df_2024, 
        title = "Matriks Korelasi Variabel Determinan IPM (2024)",
        columnLabels = c("IPM", "Pendidikan (RLS)", "Kesehatan (AHH)", "Gender (IPG)", "Ekonomi (PPP)")) +
  theme_minimal()
```

### PROGRESS MAP 

```{r}
```

```{r}
```

### PROGRESS tes  {data-width=1200}

```{r echo=FALSE}
HTML('
<div class="content-tab-wrapper">
  <div class="content-tab-container">
    <button class="tab-btn active" data-target="chapter9-tab1">Basic<br>Concepts</button>
    <button class="tab-btn" data-target="chapter9-tab2">Hypothesis Tests</button>
    <button class="tab-btn" data-target="chapter9-tab3">Decision Making</button>
  </div>
</div>

<div class="content-tab-contents-wrapper">
  
  <!-- TAB 1: Basic Concepts -->
  <section id="chapter9-tab1" class="content-tab-section text-only active">
    <div class="content-tab-left">
      <center><h1>Basic Concepts of Statistical Inference</h1></center>
      
      <div style="font-family: Arial, sans-serif; line-height: 1.3; text-align: justify;">
        <p style="text-align: justify; text-indent: 40px; margin-bottom: 15px;">
          Statistical inference is the process of drawing conclusions about a population based on information obtained from a sample. This allows researchers and analysts to make generalizations, predictions, and decisions under conditions of uncertainty. Highlights its three main components: <strong>Statistical hypothesis</strong>, <strong>hypothesis testing methods</strong>, and <strong>decision-making</strong>.
        </p>
      </div>

      <h4>Hypothesis Fundamentals</h4>
      <table>
        <thead>
          <tr>
            <th>Term</th>
            <th>Symbol</th>
            <th>Definition</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td><strong>Null Hypothesis</strong></td>
            <td>\\(H_0\\)</td>
            <td>Represents the assumption that there is <strong>no effect, no difference, or no relationship</strong> in the population</td>
          </tr>
          <tr>
            <td><strong>Alternative Hypothesis</strong></td>
            <td>\\(H_1\\) or \\(H_a\\)</td>
            <td>Statement we want to prove; indicates a difference, relationship, or effect</td>
          </tr>
          <tr>
            <td><strong>P-value</strong></td>
            <td>\\(p\\)</td>
            <td>Probability that observed results happened by chance if \\(H_0\\) is true</td>
          </tr>
          <tr>
            <td><strong>Significance Level</strong></td>
            <td>\\(\\alpha\\)</td>
            <td>"Threshold" for evidence (usually 0.05)</td>
          </tr>
        </tbody>
      </table>
    </div>
  </section>

  <!-- TAB 2: Hypothesis Tests -->
  <section id="chapter9-tab2" class="content-tab-section text-only">
    <div class="content-tab-left">
      <center><h1>Hypothesis Testing Methods</h1></center>
      
      <div style="font-family: Arial, sans-serif; line-height: 1.3; text-align: justify;">
        <p style="text-align: justify; text-indent: 40px; margin-bottom: 15px;">
          Hypothesis testing methods are used to determine whether the evidence form a sample is strong enough to reject the null hypothesis in favor of the alternative hypothesis. The choice of test depends on the <strong>type of data</strong>, <strong>sample size</strong>, and <strong>population characteristic</strong>.
        </p>
      </div>

      <h3>Comparison of Statistical Tests</h3>
      <table>
        <thead>
          <tr>
            <th><strong>Z-Test</strong></th>
            <th><strong>T-Test</strong></th>
            <th><strong>Chi-Square Test</strong></th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>
              <strong>Function:</strong> Compare means with known \\(\\sigma\\) or large \\(n\\)<br>
              <strong>Formula:</strong> \\(z = \\frac{\\bar{x} - \\mu}{\\sigma/\\sqrt{n}}\\)<br>
              <strong>Example:</strong> Test if new teaching method changes scores
            </td>
            <td>
              <strong>Function:</strong> Compare means with unknown \\(\\sigma\\) and small \\(n\\)<br>
              <strong>Formula:</strong> \\(t = \\frac{\\bar{x} - \\mu}{s/\\sqrt{n}}\\)<br>
              <strong>Example:</strong> Test if average score differs from 80
            </td>
            <td>
              <strong>Function:</strong> Test categorical data distributions<br>
              <strong>Formula:</strong> \\(\\chi^2 = \\sum\\frac{(O-E)^2}{E}\\)<br>
              <strong>Example:</strong> Test if gender is independent of learning preference
            </td>
          </tr>
        </tbody>
      </table>
    </div>
  </section>

  <!-- TAB 3: Decision Making -->
  <section id="chapter9-tab3" class="content-tab-section text-only">
    <div class="content-tab-left">
      <center><h1>Statistical Decision Making</h1></center>
      <table>
        <thead>
          <tr>
            <th>Error Type</th>
            <th>Definition</th>
            <th>Probability</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td><strong>Type I Error</strong></td>
            <td>Rejecting \\(H_0\\) when it\'s true (False Positive)</td>
            <td>\\(\\alpha\\), commonly 0.05</td>
          </tr>
          <tr>
            <td><strong>Type II Error</strong></td>
            <td>Failing to reject \\(H_0\\) when it\'s false (False Negative)</td>
            <td>\\(\\beta\\); power \\(=1-\\beta\\)</td>
          </tr>
        </tbody>
      </table>
      
      <!-- Simple Decision Rules using your div style -->
      <div style="border: 1px solid #ddd; padding: 15px; border-radius: 3px; background: #f9f9f9; margin: 15px 0;">
        <div style="font-weight: bold; color: #333; margin-bottom: 10px;">Decision Rules Based on P-value:</div>
        
        <div style="display: flex; gap: 15px; margin-top: 10px;">
          <div style="flex: 1; padding: 10px; background: #e8f5e8; border-radius: 3px; border-left: 4px solid #28a745;">
            <div style="font-weight: bold; color: #155724;">If P-value < α</div>
            <div style="margin-top: 5px; font-size: 16px; font-weight: bold; color: #155724;">→ Reject H₀</div>
            <div style="margin-top: 5px; font-size: 14px; color: #155724;">(Statistically Significant)</div>
          </div>
          
          <div style="flex: 1; padding: 10px; background: #fdeaea; border-radius: 3px; border-left: 4px solid #dc3545;">
            <div style="font-weight: bold; color: #721c24;">If P-value ≥ α</div>
            <div style="margin-top: 5px; font-size: 16px; font-weight: bold; color: #721c24;">→ Fail to reject H₀</div>
            <div style="margin-top: 5px; font-size: 14px; color: #721c24;">(Not Significant)</div>
          </div>
        </div>
      </div>
  </section>
</div>
')
```

Testing visual {data-orientation=rows}
=======================================================================

## Column {.tabset .tabset-fade data-height=520}
-----------------------------------------------------------------------
