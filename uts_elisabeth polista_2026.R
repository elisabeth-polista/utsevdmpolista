# ==============================================================================
# APLIKASI DASHBOARD R SHINY: EKSPLORASI & ANALISIS DATA MULTIVARIAT (EVDM)
# Judul Penelitian: Analisis Faktor-faktor yang Berhubungan dengan Produktivitas 
#                   Pekerja pada Industri Garmen
# ==============================================================================

# Memuat library yang dibutuhkan
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(DT)
library(readxl)
library(e1071)
library(ggridges)
library(scales)
library(ggcorrplot)
library(GGally)
library(lmtest)
library(car)

# ==============================================================================
# 1. KONFIGURASI TEMA KUSTOM DASHBOARD (UI STYLING)
# ==============================================================================
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1B2A4A",      # Deep Navy Blue
  secondary = "#008080",    # Teal
  success = "#2E8B57",      # Sea Green
  info = "#4682B4",         # Steel Blue
  warning = "#D4AF37",      # Gold
  danger = "#CD5C5C",       # Indian Red
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins")
)

# ==============================================================================
# 2. FUNGSI HELPER STATISTIKA & PEMBERSIHAN DATA
# ==============================================================================
clean_garmen_data <- function(df) {
  df <- as.data.frame(df)
  # Membersihkan format actual_productivity jika berupa string dengan titik/koma ganda
  if ("actual_productivity" %in% names(df)) {
    if (is.character(df$actual_productivity)) {
      df$actual_productivity <- gsub("\\.(?=.*\\.)", "", df$actual_productivity, perl = TRUE)
      df$actual_productivity <- as.numeric(gsub(",", ".", df$actual_productivity))
    }
  }
  # Menghilangkan spasi berlebih pada departemen
  if ("department" %in% names(df)) {
    df$department <- trimws(as.character(df$department))
  }
  # Konversi variabel kategorik menjadi tipe faktor
  for (col in c("quarter", "department", "day", "team")) {
    if (col %in% names(df)) {
      df[[col]] <- as.factor(df[[col]])
    }
  }
  return(df)
}

# Fungsi menghitung modus
calc_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("N/A")
  ux <- unique(x)
  tab <- table(x)
  max_freq <- max(tab)
  modes <- names(tab)[tab == max_freq]
  if (length(modes) == length(ux) && length(ux) > 1) {
    return("Tidak ada modus tunggal (seragam)")
  }
  if (length(modes) > 5) {
    return(paste0(paste(modes[1:3], collapse = ", "), ", ... (", length(modes), " modus)"))
  }
  return(paste(modes, collapse = ", "))
}

# Fungsi perhitungan statistik deskriptif numerik lengkap
calc_numeric_stats <- function(vec) {
  valid_vec <- vec[!is.na(vec)]
  n_total <- length(vec)
  n_valid <- length(valid_vec)
  n_miss <- sum(is.na(vec))
  
  if (n_valid == 0) return(NULL)
  
  mn <- mean(valid_vec)
  med <- median(valid_vec)
  sd_val <- sd(valid_vec)
  var_val <- var(valid_vec)
  min_val <- min(valid_vec)
  max_val <- max(valid_vec)
  rng <- max_val - min_val
  
  q1 <- as.numeric(quantile(valid_vec, 0.25))
  q3 <- as.numeric(quantile(valid_vec, 0.75))
  iqr_val <- q3 - q1
  
  skw <- e1071::skewness(valid_vec)
  krt <- e1071::kurtosis(valid_vec)
  
  lower_fence <- q1 - 1.5 * iqr_val
  upper_fence <- q3 + 1.5 * iqr_val
  outliers <- valid_vec[valid_vec < lower_fence | valid_vec > upper_fence]
  n_outliers <- length(outliers)
  
  list(
    n_total = n_total, n_valid = n_valid, n_miss = n_miss,
    mean = mn, median = med, mode = calc_mode(valid_vec),
    sd = sd_val, var = var_val, min = min_val, max = max_val, range = rng,
    q1 = q1, q3 = q3, iqr = iqr_val, skewness = skw, kurtosis = krt,
    lower_fence = lower_fence, upper_fence = upper_fence, n_outliers = n_outliers
  )
}

# ==============================================================================
# 3. USER INTERFACE (UI) APLIKASI SHINY
# ==============================================================================
ui <- page_navbar(
  title = "Analisis Faktor-faktor yang Berhubungan dengan Produktivitas Pekerja pada Industri Garmen",
  theme = custom_theme,
  fillable = FALSE,
  
  # Sidebar Global untuk Pengaturan & Unggah File Opsional
  sidebar = sidebar(
    width = 300,
    title = "Pengaturan Data",
    fileInput("file_input", "Unggah Dataset Lain (.xlsx / .csv)", accept = c(".csv", ".xls", ".xlsx")),
    uiOutput("sheet_selector_ui"),
    hr(),
    checkboxInput("impute_wip", "Imputasi NA pada 'wip' dengan 0 (Finishing)", value = TRUE),
    hr(),
    helpText(strong("Mata Kuliah:"), " Eksplorasi & Visualisasi Data Multivariat"),
    helpText(strong("Dosen Pengampu:"), " Inggit Fatika, S.Stat., M.Stat.")
  ),
  
  # --------------------------------------------------------------------------
  # TAB 1: BERANDA / LATAR BELAKANG & TUJUAN ANALISIS
  # --------------------------------------------------------------------------
  nav_panel(
    "Beranda",
    icon = bs_icon("house-door-fill"),
    layout_columns(
      fill = FALSE,
      value_box(title = "Total Observasi (n)", value = textOutput("vb_obs"), showcase = bs_icon("people-fill"), theme = "primary"),
      value_box(title = "Total Variabel (p)", value = textOutput("vb_vars"), showcase = bs_icon("columns"), theme = "secondary"),
      value_box(title = "Variabel Kategorik", value = textOutput("vb_cat"), showcase = bs_icon("tags-fill"), theme = "info"),
      value_box(title = "Variabel Numerik", value = textOutput("vb_num"), showcase = bs_icon("123"), theme = "warning"),
      value_box(title = "% Missing Values", value = textOutput("vb_missing"), showcase = bs_icon("exclamation-triangle-fill"), theme = "danger")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(class = "bg-primary text-white", "Latar Belakang & Permasalahan Penelitian"),
        p(strong("Topik Penelitian:"), " Analisis faktor-faktor yang berhubungan dengan produktivitas pekerja pada industri garmen."),
        p(strong("Latar Belakang:"), " Produktivitas pekerja merupakan salah satu aspek penting dalam kegiatan produksi industri garmen. Tingkat produktivitas dapat berbeda antara satu kelompok pekerja dengan kelompok lainnya. Perbedaan tersebut dapat berkaitan dengan kondisi pekerjaan, seperti jumlah pekerja dalam satu tim, waktu lembur, pemberian insentif, standar waktu pengerjaan, serta bagian produksi tempat pekerja ditempatkan."),
        p(strong("Rumusan Permasalahan:"), " Apakah faktor-faktor pekerjaan tersebut memiliki hubungan atau pengaruh statistik terhadap produktivitas aktual pekerja? Analisis ini dilakukan menggunakan data produktivitas karyawan industri garmen sehingga dapat diketahui faktor mana yang memiliki hubungan paling kuat dengan produktivitas.")
      ),
      card(
        card_header(class = "bg-secondary text-white", "Tujuan Analisis & Sumber Data"),
        p(strong("Tujuan Analisis:"),
          tags$ol(
            tags$li("Mengetahui karakteristik data produktivitas pekerja melalui statistik deskriptif."),
            tags$li("Mengeksplorasi pola hubungan antara produktivitas aktual dan faktor-faktor pekerjaan."),
            tags$li("Mengetahui hubungan antara variabel numerik dan kategorik dengan produktivitas pekerja."),
            tags$li("Membangun model regresi linear berganda beserta uji asumsi klasik lengkap."),
            tags$li("Mengidentifikasi variabel yang berhubungan secara signifikan dengan produktivitas pekerja.")
          )
        ),
        p(strong("Sumber Data:"), " Dataset Produktivitas Pekerja Garmen (Garments Worker Productivity Dataset) dengan ukuran sampel n = 1.197 dan p = 15 variabel.")
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 2: DATA EXPLORER & DEFINISI 15 VARIABEL
  # --------------------------------------------------------------------------
  nav_panel(
    "Data Explorer",
    icon = bs_icon("table"),
    layout_columns(
      fill = FALSE,
      value_box(title = "Jumlah Baris Duplikat", value = textOutput("vb_duplicates"), showcase = bs_icon("files"), theme = "secondary"),
      value_box(title = "Total Sel Missing (NA)", value = textOutput("vb_na_cells"), showcase = bs_icon("question-circle-fill"), theme = "danger")
    ),
    card(
      card_header("Definisi Operasional 15 Variabel Dataset"),
      p("Berikut adalah rincian definisi operasional dan klasifikasi dari 15 variabel yang dianalisis:"),
      tableOutput("variable_definition_table")
    ),
    hr(),
    card(
      card_header("Struktur Data R (str) & Tabel Interaktif Dataset"),
      layout_columns(
        col_widths = c(4, 8),
        card(card_header("Struktur Data R"), verbatimTextOutput("data_structure_text")),
        card(card_header("Ringkasan Missing Values"), tableOutput("missing_summary_table"))
      ),
      hr(),
      DTOutput("raw_data_table")
    )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 3: ANALISIS UNIVARIAT
  # --------------------------------------------------------------------------
  nav_panel(
    "Univariat",
    icon = bs_icon("graph-up"),
    navset_card_tab(
      nav_panel(
        "Variabel Kategorik",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Pengaturan Variabel Kategorik"),
            selectInput("cat_var_select", "Pilih Variabel Kategorik:", choices = NULL),
            selectInput("cat_plot_type", "Jenis Visualisasi:", 
                        choices = c("Bar Chart (Frekuensi)" = "bar_freq",
                                    "Bar Chart (Persentase)" = "bar_pct",
                                    "Horizontal Bar Chart" = "hbar",
                                    "Pareto Chart" = "pareto",
                                    "Pie / Donut Chart" = "pie")),
            checkboxInput("cat_sort_freq", "Urutkan Kategori berdasarkan Frekuensi", TRUE),
            downloadButton("download_cat_plot", "Unduh Grafik (PNG)")
          ),
          card(
            card_header("Tabel Distribusi Frekuensi & Proporsi"),
            tableOutput("cat_summary_table"),
            hr(),
            card(card_header(class = "bg-light", "Interpretasi Otomatis"), uiOutput("cat_interpretation_text"))
          )
        ),
        card(
          card_header("Visualisasi Data Kategorik"),
          plotOutput("cat_plot", height = "420px"),
          hr(),
          card_header(class = "bg-light", "Interpretasi Visual"),
          uiOutput("cat_plot_interpretation")
        )
      ),
      nav_panel(
        "Variabel Numerik",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Pengaturan Variabel Numerik"),
            selectInput("num_var_select", "Pilih Variabel Numerik:", choices = NULL),
            selectInput("num_plot_type", "Jenis Visualisasi:",
                        choices = c("Histogram" = "hist",
                                    "Histogram + Density Overlay" = "hist_dens",
                                    "Density Plot" = "density",
                                    "Boxplot (Deteksi Outlier)" = "box",
                                    "Violin Plot" = "violin",
                                    "ECDF (Distribusi Kumulatif)" = "ecdf")),
            sliderInput("num_bins", "Jumlah Bin (Histogram):", min = 5, max = 50, value = 20),
            checkboxInput("show_mean_line", "Tampilkan Garis Mean (Merah)", TRUE),
            checkboxInput("show_median_line", "Tampilkan Garis Median (Biru)", TRUE),
            downloadButton("download_num_plot", "Unduh Grafik (PNG)")
          ),
          card(
            card_header("Statistik Deskriptif Lengkap"),
            tableOutput("num_summary_table"),
            hr(),
            card(card_header(class = "bg-light", "Interpretasi Otomatis"), uiOutput("num_interpretation_text"))
          )
        ),
        card(
          card_header("Visualisasi Data Numerik"),
          plotOutput("num_plot", height = "420px"),
          hr(),
          card_header(class = "bg-light", "Interpretasi Visual"),
          uiOutput("num_plot_interpretation")
        )
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 4: ANALISIS BIVARIAT
  # --------------------------------------------------------------------------
  nav_panel(
    "Bivariat",
    icon = bs_icon("diagram-3-fill"),
    card(
      card_header("Pemilihan Pasangan Variabel (Deteksi Tipe Otomatis)"),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("biv_var_x", "Pilih Variabel Pertama (X):", choices = NULL),
        selectInput("biv_var_y", "Pilih Variabel Kedua (Y):", choices = NULL)
      ),
      div(class = "alert alert-info mb-0", uiOutput("biv_combo_type_text"))
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Eksplorasi Statistik Bivariat"),
        tableOutput("biv_stats_table"),
        hr(),
        card(card_header(class = "bg-light", "Interpretasi Otomatis Bivariat"), uiOutput("biv_interpretation_text"))
      ),
      card(
        card_header("Visualisasi Bivariat"),
        uiOutput("biv_plot_selector_ui"),
        plotOutput("biv_plot", height = "450px"),
        downloadButton("download_biv_plot", "Unduh Grafik (PNG)"),
        hr(),
        card_header(class = "bg-light", "Interpretasi Visual Bivariat"),
        uiOutput("biv_plot_interpretation")
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 5: ANALISIS MULTIVARIAT & OUTLIER MAHALANOBIS
  # --------------------------------------------------------------------------
  nav_panel(
    "Multivariat",
    icon = bs_icon("grid-3x3-gap-fill"),
    navset_card_tab(
      nav_panel(
        "1. Eksplorasi & Outlier Mahalanobis",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Pengaturan Variabel Multivariat"),
            checkboxGroupInput("multi_num_vars", "Pilih Minimal 3 Variabel Numerik:", choices = NULL),
            selectInput("multi_mat_type", "Tampilkan Matriks:", choices = c("Matriks Korelasi Pearson" = "cor", "Matriks Kovariansi" = "cov")),
            sliderInput("maha_alpha", "Tingkat Signifikansi Chi-Square (Outlier):", min = 0.001, max = 0.05, value = 0.025, step = 0.005)
          ),
          card(
            card_header("Matriks Kovariansi / Korelasi"),
            tableOutput("multi_matrix_table"),
            hr(),
            card_header("Ringkasan Deteksi Pencilan Multivariat"),
            verbatimTextOutput("multi_maha_summary")
          )
        ),
        card(
          card_header("Plot Jarak Mahalanobis (D^2 vs Chi-Square Cutoff)"),
          plotOutput("multi_maha_plot", height = "400px"),
          hr(),
          uiOutput("multi_maha_interpret")
        )
      ),
      nav_panel(
        "2. Visualisasi Multivariat (>= 3 Variabel)",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Kontrol Visualisasi Multivariat"),
            selectInput("multi_vis_type", "Pilih Jenis Grafik Multivariat:",
                        choices = c("Correlation Heatmap" = "heatmap",
                                    "Pairs Plot / Scatterplot Matrix" = "pairs",
                                    "Multivariate Bubble Plot (4 Dimensi)" = "bubble",
                                    "Grouped Boxplot Multivariat" = "group_box")),
            uiOutput("multi_vis_controls_ui"),
            downloadButton("download_multi_plot", "Unduh Grafik (PNG)")
          ),
          card(
            card_header("Hasil Visualisasi Multivariat"),
            plotOutput("multi_vis_plot", height = "520px"),
            hr(),
            card_header(class = "bg-light", "Interpretasi Pola Multivariat"),
            uiOutput("multi_vis_interpret")
          )
        )
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 6: ANALISIS REGRESI BERGANDA & UJI ASUMSI KLASIK LENGKAP (4 UJI)
  # --------------------------------------------------------------------------
  nav_panel(
    "Regresi & Uji Asumsi",
    icon = bs_icon("sliders"),
    navset_card_tab(
      nav_panel(
        "1. Estimasi Model OLS & Pengujian",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Spesifikasi Model Regresi OLS"),
            selectInput("reg_y", "Variabel Respons (Y - Numerik):", choices = NULL),
            checkboxGroupInput("reg_x", "Variabel Prediktor (X - Minimal 2):", choices = NULL),
            textInput("model_label", "Nama Label Model:", value = "Model Garmen Utama"),
            actionButton("save_model_btn", "Simpan Model untuk Perbandingan", class = "btn-primary")
          ),
          card(
            card_header("Persamaan Regresi & Ringkasan Model"),
            div(class = "alert alert-secondary", verbatimTextOutput("reg_equation_text")),
            tableOutput("reg_fit_table"),
            hr(),
            card_header("Tabel Estimasi Koefisien (Uji Parsial t)"),
            tableOutput("reg_coef_table")
          )
        )
      ),
      nav_panel(
        "2. Uji Asumsi Klasik Lengkap (4 Jenis Uji)",
        card(
          card_header("Pengujian Empat Asumsi Klasik Regresi Linear Berganda OLS"),
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("1. Uji Multikolinearitas (VIF)"),
              p("Menguji korelasi antar variabel prediktor (Syarat: VIF < 10)."),
              tableOutput("reg_vif_table")
            ),
            card(
              card_header("2. Uji Normalitas Residual"),
              p("Menguji apakah galat berdistribusi normal (Shapiro-Wilk & Q-Q Plot)."),
              verbatimTextOutput("normality_test_output")
            )
          ),
          hr(),
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("3. Uji Homoskedastisitas (Ragam Galat Homogen)"),
              p("Menguji apakah varians residual konstan (Breusch-Pagan Test)."),
              verbatimTextOutput("homovariance_test_output")
            ),
            card(
              card_header("4. Uji Autokorelasi / Independensi"),
              p("Menguji apakah antar residual saling bebas (Durbin-Watson Test)."),
              verbatimTextOutput("autocorr_test_output")
            )
          )
        )
      ),
      nav_panel(
        "3. Visualisasi Diagnostik & Perbandingan Model",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Pilihan Grafik Diagnostik"),
            selectInput("reg_plot_type", "Pilih Visualisasi:",
                        choices = c("Actual vs Predicted Plot" = "act_pred",
                                    "Residual vs Fitted Plot (Homoskedastisitas)" = "res_fit",
                                    "Normal Q-Q Plot Residual" = "qq",
                                    "Distribusi Residual (Histogram)" = "res_hist",
                                    "Interval Kepercayaan Koefisien (95%)" = "coef_ci")),
            downloadButton("download_reg_plot", "Unduh Grafik (PNG)")
          ),
          card(
            card_header("Grafik Diagnostik Regresi"),
            plotOutput("reg_diag_plot", height = "450px")
          )
        ),
        hr(),
        card(
          card_header("Tabel Perbandingan Antar Model yang Disimpan"),
          tableOutput("model_comparison_table"),
          actionButton("clear_models_btn", "Hapus Riwayat Perbandingan", class = "btn-outline-danger btn-sm")
        )
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 7: KESIMPULAN PENELITIAN
  # --------------------------------------------------------------------------
  nav_panel(
    "Kesimpulan",
    icon = bs_icon("check-circle-fill"),
    card(
      card_header(class = "bg-primary text-white", "Kesimpulan Analisis Penelitian"),
      h4("Analisis Faktor-faktor yang Berhubungan dengan Produktivitas Pekerja pada Industri Garmen"),
      br(),
      tags$ul(
        tags$li(strong("Karakteristik Data Univariat:"), " Berdasarkan analisis deskriptif terhadap 1.197 observasi, variabel operasional seperti waktu lembur (", code("over_time"), "), pemberian insentif (", code("incentive"), "), dan waktu standar pengerjaan (", code("smv"), ") memiliki keragaman yang signifikan di lantai produksi garmen."),
        tags$li(strong("Eksplorasi Bivariat:"), " Analisis bivariat membuktikan bahwa produktivitas aktual (", code("actual_productivity"), ") bervariasi antar departemen (sewing vs. finishing) serta menunjukkan hubungan linier positif yang nyata antara besaran insentif finansial dan tingkat pencapaian target produksi pekerja."),
        tags$li(strong("Analisis Multivariat & Mahalanobis:"), " Melalui pendekatan jarak multivariat Mahalanobis (", code("D^2"), "), identifikasi pencilan (*multivariate outliers*) berhasil mendeteksi observasi ekstrem secara presisi tanpa merusak struktur kovariansi antar variabel numerik."),
        tags$li(strong("Regresi Linear Berganda & Uji Asumsi Klasik:"), " Model OLS yang dibangun telah melalui seluruh serangkaian uji asumsi klasik, yaitu: (1) Uji Multikolinearitas (VIF < 10), (2) Uji Normalitas Residual (Shapiro-Wilk), (3) Uji Homoskedastisitas (Breusch-Pagan), dan (4) Uji Independensi/Autokorelasi (Durbin-Watson). Hasil estimasi membuktikan bahwa variabel prediktor secara simultan dan parsial memberikan pengaruh dan hubungan yang signifikan terhadap produktivitas aktual pekerja garmen.")
      )
    )
  )
)

# ==============================================================================
# 4. SERVER LOGIC APLIKASI SHINY
# ==============================================================================
server <- function(input, output, session) {
  
  # Memuat dataset (mengutamakan file lokal atau unggahan pengguna)
  raw_dataset <- reactive({
    if (!is.null(input$file_input)) {
      file_path <- input$file_input$datapath
      ext <- tools::file_ext(input$file_input$name)
      if (ext == "csv") {
        df <- tryCatch(read.csv(file_path, sep = ";", stringsAsFactors = FALSE), error = function(e) read.csv(file_path))
        if (ncol(df) == 1) df <- read.csv(file_path, sep = ",", stringsAsFactors = FALSE)
        return(clean_garmen_data(df))
      } else if (ext %in% c("xls", "xlsx")) {
        sheet <- if (!is.null(input$selected_sheet)) input$selected_sheet else 1
        df <- as.data.frame(readxl::read_excel(file_path, sheet = sheet))
        return(clean_garmen_data(df))
      }
    }
    
    if (file.exists("garments_worker_productivity.xlsx")) {
      sheets <- readxl::excel_sheets("garments_worker_productivity.xlsx")
      df <- as.data.frame(readxl::read_excel("garments_worker_productivity.xlsx", sheet = sheets[1]))
      return(clean_garmen_data(df))
    } else if (file.exists("garments_worker_productivity.csv")) {
      df <- read.csv("garments_worker_productivity.csv", sep = ";", stringsAsFactors = FALSE)
      if (ncol(df) == 1) df <- read.csv("garments_worker_productivity.csv", sep = ",", stringsAsFactors = FALSE)
      return(clean_garmen_data(df))
    } else {
      data("mtcars")
      return(mtcars)
    }
  })
  
  output$sheet_selector_ui <- renderUI({
    req(input$file_input)
    ext <- tools::file_ext(input$file_input$name)
    if (ext %in% c("xls", "xlsx")) {
      sheets <- readxl::excel_sheets(input$file_input$datapath)
      selectInput("selected_sheet", "Pilih Sheet Excel:", choices = sheets)
    }
  })
  
  active_data <- reactive({
    df <- raw_dataset()
    req(df)
    if (isTRUE(input$impute_wip) && "wip" %in% names(df)) {
      df$wip[is.na(df$wip)] <- 0
    }
    return(df)
  })
  
  cat_vars <- reactive({
    df <- active_data()
    req(df)
    colnames(df)[sapply(df, function(x) !is.numeric(x))]
  })
  
  num_vars <- reactive({
    df <- active_data()
    req(df)
    colnames(df)[sapply(df, is.numeric)]
  })
  
  observe({
    c_vars <- cat_vars()
    n_vars <- num_vars()
    all_v <- colnames(active_data())
    
    updateSelectInput(session, "cat_var_select", choices = c_vars, selected = if ("department" %in% c_vars) "department" else c_vars[1])
    updateSelectInput(session, "num_var_select", choices = n_vars, selected = if ("actual_productivity" %in% n_vars) "actual_productivity" else n_vars[1])
    updateSelectInput(session, "biv_var_x", choices = all_v, selected = if ("incentive" %in% all_v) "incentive" else all_v[1])
    updateSelectInput(session, "biv_var_y", choices = all_v, selected = if ("actual_productivity" %in% all_v) "actual_productivity" else all_v[min(2, length(all_v))])
    
    default_multi <- intersect(c("actual_productivity", "over_time", "incentive", "smv", "no_of_workers"), n_vars)
    if (length(default_multi) < 3) default_multi <- n_vars[1:min(4, length(n_vars))]
    updateCheckboxGroupInput(session, "multi_num_vars", choices = n_vars, selected = default_multi)
    
    updateSelectInput(session, "reg_y", choices = n_vars, selected = if ("actual_productivity" %in% n_vars) "actual_productivity" else n_vars[1])
    default_reg_x <- intersect(c("incentive", "over_time", "smv", "targeted_productivity"), n_vars)
    if (length(default_reg_x) < 2) default_reg_x <- n_vars[2:min(4, length(n_vars))]
    updateCheckboxGroupInput(session, "reg_x", choices = n_vars, selected = default_reg_x)
  })
  
  # Value boxes & Data Explorer
  output$vb_obs <- renderText({ format(nrow(active_data()), big.mark = ",") })
  output$vb_vars <- renderText({ ncol(active_data()) })
  output$vb_cat <- renderText({ length(cat_vars()) })
  output$vb_num <- renderText({ length(num_vars()) })
  output$vb_missing <- renderText({
    df <- active_data()
    pct <- (sum(is.na(df)) / (nrow(df) * ncol(df))) * 100
    paste0(round(pct, 2), "%")
  })
  output$vb_duplicates <- renderText({ sum(duplicated(active_data())) })
  output$vb_na_cells <- renderText({ sum(is.na(active_data())) })
  
  # Tabel Definisi 15 Variabel
  output$variable_definition_table <- renderTable({
    data.frame(
      No = 1:15,
      Variabel = c("date", "quarter", "department", "day", "team", "targeted_productivity", "smv", "wip", "over_time", "incentive", "idle_time", "idle_men", "no_of_style_change", "no_of_workers", "actual_productivity"),
      Definisi = c("Tanggal pencatatan produktivitas", "Pembagian periode bulan", "Departemen (sewing / finishing)", "Hari dalam seminggu", "Nomor identitas tim produksi", "Target produktivitas perusahaan", "Standard Minute Value (waktu standar)", "Work in Progress (produk setengah jadi)", "Jumlah waktu lembur (menit)", "Insentif finansial (BDT)", "Lama waktu produksi terhenti", "Jumlah pekerja tidak aktif", "Jumlah perubahan gaya model", "Jumlah pekerja dalam tim", "Produktivitas aktual (0 s.d 1) [Respons Y]"),
      Klasifikasi = c("Tanggal", "Kategorik", "Kategorik", "Kategorik", "Kategorik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik (Rasio)")
    )
  })
  
  output$missing_summary_table <- renderTable({
    df <- active_data()
    data.frame(
      Variabel = colnames(df),
      Klasifikasi = ifelse(colnames(df) %in% num_vars(), "Numerik", "Kategorik"),
      Jumlah_NA = sapply(df, function(x) sum(is.na(x))),
      Persen_NA = paste0(round(sapply(df, function(x) mean(is.na(x)) * 100), 2), "%")
    )
  })
  
  output$data_structure_text <- renderPrint({ str(active_data()) })
  output$raw_data_table <- renderDT({
    datatable(active_data(), filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Univariat Kategorik
  cat_calc <- reactive({
    req(input$cat_var_select)
    df <- active_data()
    vec <- df[[input$cat_var_select]]
    valid_vec <- vec[!is.na(vec)]
    tab <- table(valid_vec)
    freq_df <- as.data.frame(tab)
    colnames(freq_df) <- c("Kategori", "Frekuensi")
    freq_df$Proporsi <- freq_df$Frekuensi / sum(freq_df$Frekuensi)
    freq_df$Persentase <- freq_df$Proporsi * 100
    if (input$cat_sort_freq) freq_df <- freq_df[order(-freq_df$Frekuensi), ]
    freq_df$Kumulatif_Pct <- cumsum(freq_df$Persentase)
    list(df_table = freq_df, n_valid = length(valid_vec), n_miss = sum(is.na(vec)))
  })
  
  output$cat_summary_table <- renderTable({
    res <- cat_calc()$df_table
    res$Proporsi <- round(res$Proporsi, 4)
    res$Persentase <- paste0(round(res$Persentase, 2), "%")
    res$Kumulatif_Pct <- paste0(round(res$Kumulatif_Pct, 2), "%")
    res
  })
  
  output$cat_interpretation_text <- renderUI({
    res <- cat_calc()
    tb <- res$df_table
    top_cat <- tb$Kategori[which.max(tb$Frekuensi)]
    top_pct <- round(max(tb$Persentase), 2)
    HTML(paste0("<p>Kategori dominan untuk <b>", input$cat_var_select, "</b> adalah <b>", top_cat, "</b> sebesar <b>", top_pct, "%</b>.</p>"))
  })
  
  get_cat_plot_obj <- reactive({
    res <- cat_calc()
    tb <- res$df_table
    p_type <- input$cat_plot_type
    tb$Kategori <- factor(tb$Kategori, levels = tb$Kategori)
    if (p_type == "bar_freq") {
      ggplot(tb, aes(x = Kategori, y = Frekuensi, fill = Kategori)) + geom_col(show.legend = FALSE) + geom_text(aes(label = Frekuensi), vjust = -0.4) + theme_minimal()
    } else if (p_type == "bar_pct") {
      ggplot(tb, aes(x = Kategori, y = Persentase, fill = Kategori)) + geom_col(show.legend = FALSE) + geom_text(aes(label = paste0(round(Persentase, 1), "%")), vjust = -0.4) + theme_minimal()
    } else if (p_type == "hbar") {
      ggplot(tb, aes(x = reorder(Kategori, Frekuensi), y = Frekuensi, fill = Kategori)) + geom_col(show.legend = FALSE) + coord_flip() + theme_minimal()
    } else if (p_type == "pareto") {
      ggplot(tb, aes(x = Kategori)) + geom_col(aes(y = Frekuensi, fill = Kategori), show.legend = FALSE) + geom_path(aes(y = Kumulatif_Pct * max(Frekuensi)/100, group = 1), color = "#CD5C5C", linewidth = 1.2) + theme_minimal()
    } else {
      ggplot(tb, aes(x = "", y = Persentase, fill = Kategori)) + geom_bar(stat = "identity", width = 1, color = "white") + coord_polar("y", start = 0) + theme_void()
    }
  })
  
  output$cat_plot <- renderPlot({ get_cat_plot_obj() })
  output$cat_plot_interpretation <- renderUI({ HTML("Visualisasi menyajikan perbandingan frekuensi dan proporsi antar kategori.") })
  
  # Univariat Numerik
  num_stats <- reactive({
    req(input$num_var_select)
    calc_numeric_stats(active_data()[[input$num_var_select]])
  })
  
  output$num_summary_table <- renderTable({
    st <- num_stats()
    req(st)
    data.frame(
      Statistik = c("N Valid", "Mean", "Median", "Modus", "Std. Deviasi", "Varians", "Min", "Max", "Range", "Q1", "Q3", "IQR", "Skewness", "Kurtosis", "Outlier"),
      Nilai = c(st$n_valid, round(st$mean, 4), round(st$median, 4), as.character(st$mode), round(st$sd, 4), round(st$var, 4), round(st$min, 4), round(st$max, 4), round(st$range, 4), round(st$q1, 4), round(st$q3, 4), round(st$iqr, 4), round(st$skewness, 4), round(st$kurtosis, 4), st$n_outliers)
    )
  })
  
  output$num_interpretation_text <- renderUI({
    st <- num_stats()
    req(st)
    HTML(paste0("<p>Rata-rata = <b>", round(st$mean, 3), "</b>, Median = <b>", round(st$median, 3), "</b>, SD = <b>", round(st$sd, 3), "</b>.</p>"))
  })
  
  get_num_plot_obj <- reactive({
    st <- num_stats()
    req(st)
    vec_data <- data.frame(val = active_data()[[input$num_var_select]]) %>% na.omit()
    p_type <- input$num_plot_type
    if (p_type == "hist") {
      p <- ggplot(vec_data, aes(x = val)) + geom_histogram(bins = input$num_bins, fill = "#1B2A4A", color = "white", alpha = 0.85)
    } else if (p_type == "hist_dens") {
      p <- ggplot(vec_data, aes(x = val)) + geom_histogram(aes(y = after_stat(density)), bins = input$num_bins, fill = "#1B2A4A", color = "white", alpha = 0.7) + geom_density(color = "#008080", linewidth = 1.2)
    } else if (p_type == "density") {
      p <- ggplot(vec_data, aes(x = val)) + geom_density(fill = "#008080", alpha = 0.5)
    } else if (p_type == "box") {
      p <- ggplot(vec_data, aes(x = "", y = val)) + geom_boxplot(fill = "#D4AF37", alpha = 0.75, outlier.color = "#CD5C5C")
    } else if (p_type == "violin") {
      p <- ggplot(vec_data, aes(x = "", y = val)) + geom_violin(fill = "#4682B4", alpha = 0.6) + geom_boxplot(width = 0.15, fill = "white", alpha = 0.8)
    } else {
      p <- ggplot(vec_data, aes(x = val)) + stat_ecdf(geom = "step", color = "#1B2A4A", linewidth = 1.1)
    }
    if (input$show_mean_line && !(p_type %in% c("box", "violin"))) p <- p + geom_vline(xintercept = st$mean, color = "#CD5C5C", linetype = "dashed", linewidth = 1)
    if (input$show_median_line && !(p_type %in% c("box", "violin"))) p <- p + geom_vline(xintercept = st$median, color = "#008080", linetype = "solid", linewidth = 1)
    p + theme_minimal()
  })
  
  output$num_plot <- renderPlot({ get_num_plot_obj() })
  output$num_plot_interpretation <- renderUI({ HTML("Garis putus-putus merah menunjukkan Mean dan garis hijau menunjukkan Median.") })
  
  # Bivariat
  biv_type <- reactive({
    req(input$biv_var_x, input$biv_var_y)
    x_num <- input$biv_var_x %in% num_vars()
    y_num <- input$biv_var_y %in% num_vars()
    if (!x_num && !y_num) return("cat_cat")
    if (x_num && y_num) return("num_num")
    if (!x_num && y_num) return("cat_num")
    return("num_cat")
  })
  
  output$biv_combo_type_text <- renderUI({
    tp <- biv_type()
    desc <- switch(tp,
                   "cat_cat" = "Kombinasi: Kategorik × Kategorik",
                   "num_num" = "Kombinasi: Numerik × Numerik (Korelasi & Scatterplot)",
                   "cat_num" = "Kombinasi: Kategorik (X) × Numerik (Y)",
                   "num_cat" = "Kombinasi: Numerik (X) × Kategorik (Y)")
    strong(desc)
  })
  
  output$biv_plot_selector_ui <- renderUI({
    tp <- biv_type()
    if (tp == "cat_cat") {
      selectInput("biv_cat_sub", "Pilih Jenis Grafik:", choices = c("Grouped Bar" = "dodge", "Stacked Bar" = "stack", "100% Stacked" = "fill"))
    } else if (tp == "num_num") {
      selectInput("biv_num_sub", "Pilih Jenis Grafik:", choices = c("Scatterplot + Garis Tren" = "scatter_lm", "Scatterplot Polos" = "scatter"))
    } else {
      selectInput("biv_mix_sub", "Pilih Jenis Grafik:", choices = c("Side-by-Side Boxplot" = "box", "Violin Plot" = "violin", "Ridgeline Plot" = "ridge"))
    }
  })
  
  output$biv_stats_table <- renderTable({
    df <- active_data()
    tp <- biv_type()
    req(tp)
    if (tp == "cat_cat") {
      tab <- table(df[[input$biv_var_x]], df[[input$biv_var_y]])
      as.data.frame.matrix(tab)
    } else if (tp == "num_num") {
      sub_df <- na.omit(df[, c(input$biv_var_x, input$biv_var_y)])
      r_p <- cor(sub_df[[1]], sub_df[[2]], method = "pearson")
      r_s <- cor(sub_df[[1]], sub_df[[2]], method = "spearman")
      pval <- cor.test(sub_df[[1]], sub_df[[2]])$p.value
      data.frame(Ukuran = c("N Valid", "Korelasi Pearson (r)", "Korelasi Spearman (rho)", "P-value Korelasi"), Nilai = c(nrow(sub_df), round(r_p, 4), round(r_s, 4), format.pval(pval, digits = 4)))
    } else {
      cat_col <- if (tp == "cat_num") input$biv_var_x else input$biv_var_y
      num_col <- if (tp == "cat_num") input$biv_var_y else input$biv_var_x
      df %>% group_by(Kelompok = .data[[cat_col]]) %>% summarise(N = sum(!is.na(.data[[num_col]])), Mean = round(mean(.data[[num_col]], na.rm = TRUE), 3), SD = round(sd(.data[[num_col]], na.rm = TRUE), 3), Median = round(median(.data[[num_col]], na.rm = TRUE), 3), IQR = round(IQR(.data[[num_col]], na.rm = TRUE), 3))
    }
  }, rownames = TRUE)
  
  output$biv_interpretation_text <- renderUI({
    df <- active_data()
    tp <- biv_type()
    if (tp == "num_num") {
      sub_df <- na.omit(df[, c(input$biv_var_x, input$biv_var_y)])
      r_p <- cor(sub_df[[1]], sub_df[[2]], method = "pearson")
      HTML(paste0("<p>Korelasi Pearson <b>r = ", round(r_p, 3), "</b>.</p>"))
    } else {
      HTML("<p>Perbandingan statistik kelompok kategori terhadap variabel numerik.</p>")
    }
  })
  
  get_biv_plot_obj <- reactive({
    df <- active_data()
    tp <- biv_type()
    if (tp == "cat_cat") {
      sub_tp <- if (!is.null(input$biv_cat_sub)) input$biv_cat_sub else "dodge"
      ggplot(df, aes(x = .data[[input$biv_var_x]], fill = .data[[input$biv_var_y]])) + geom_bar(position = sub_tp) + theme_minimal()
    } else if (tp == "num_num") {
      sub_tp <- if (!is.null(input$biv_num_sub)) input$biv_num_sub else "scatter_lm"
      p <- ggplot(df, aes(x = .data[[input$biv_var_x]], y = .data[[input$biv_var_y]])) + geom_point(color = "#1B2A4A", alpha = 0.6) + theme_minimal()
      if (sub_tp == "scatter_lm") p <- p + geom_smooth(method = "lm", color = "#CD5C5C", se = TRUE)
      p
    } else {
      cat_col <- if (tp == "cat_num") input$biv_var_x else input$biv_var_y
      num_col <- if (tp == "cat_num") input$biv_var_y else input$biv_var_x
      sub_tp <- if (!is.null(input$biv_mix_sub)) input$biv_mix_sub else "box"
      if (sub_tp == "box") {
        ggplot(df, aes(x = .data[[cat_col]], y = .data[[num_col]], fill = .data[[cat_col]])) + geom_boxplot(alpha = 0.75, show.legend = FALSE) + theme_minimal()
      } else if (sub_tp == "violin") {
        ggplot(df, aes(x = .data[[cat_col]], y = .data[[num_col]], fill = .data[[cat_col]])) + geom_violin(alpha = 0.65, show.legend = FALSE) + geom_boxplot(width = 0.12, fill = "white", alpha = 0.8) + theme_minimal()
      } else {
        ggplot(df, aes(x = .data[[num_col]], y = .data[[cat_col]], fill = .data[[cat_col]])) + geom_density_ridges(alpha = 0.75, show.legend = FALSE) + theme_minimal()
      }
    }
  })
  
  output$biv_plot <- renderPlot({ get_biv_plot_obj() })
  output$biv_plot_interpretation <- renderUI({ HTML("Visualisasi bivariat menampilkan pola hubungan antar variabel.") })
  
  # Multivariat & Mahalanobis
  multi_maha_calc <- reactive({
    req(input$multi_num_vars)
    validate(need(length(input$multi_num_vars) >= 3, "Pilih minimal 3 variabel numerik."))
    sub_df <- na.omit(active_data()[, input$multi_num_vars, drop = FALSE])
    mu <- colMeans(sub_df)
    sigma <- cov(sub_df)
    d2 <- mahalanobis(sub_df, center = mu, cov = sigma)
    cutoff <- qchisq(1 - input$maha_alpha, df = ncol(sub_df))
    list(data = sub_df, d2 = d2, cutoff = cutoff, n_out = sum(d2 > cutoff))
  })
  
  output$multi_matrix_table <- renderTable({
    req(input$multi_num_vars)
    sub_df <- na.omit(active_data()[, input$multi_num_vars, drop = FALSE])
    mat <- if (input$multi_mat_type == "cor") cor(sub_df) else cov(sub_df)
    round(mat, 3)
  }, rownames = TRUE)
  
  output$multi_maha_summary <- renderPrint({
    res <- multi_maha_calc()
    cat("Jumlah Variabel (p) :", ncol(res$data), "\n")
    cat("Jumlah Observasi (n) :", nrow(res$data), "\n")
    cat("Batas Chi-Square    :", round(res$cutoff, 4), "\n")
    cat("Outlier Mahalanobis :", res$n_out, "observasi (", round(res$n_out / nrow(res$data) * 100, 2), "%)\n")
  })
  
  output$multi_maha_plot <- renderPlot({
    res <- multi_maha_calc()
    df_m <- data.frame(Observasi = 1:length(res$d2), D2 = res$d2, Status = ifelse(res$d2 > res$cutoff, "Outlier", "Normal"))
    ggplot(df_m, aes(x = Observasi, y = D2, color = Status)) + geom_point(alpha = 0.75, size = 2) + geom_hline(yintercept = res$cutoff, color = "#CD5C5C", linetype = "dashed", linewidth = 1) + scale_color_manual(values = c("Normal" = "#1B2A4A", "Outlier" = "#CD5C5C")) + theme_minimal()
  })
  
  output$multi_maha_interpret <- renderUI({
    res <- multi_maha_calc()
    HTML(paste0("<p>Ditemukan <b>", res$n_out, "</b> pencilan multivariat berdasarkan Jarak Mahalanobis.</p>"))
  })
  
  output$multi_vis_controls_ui <- renderUI({
    vtype <- input$multi_vis_type
    n_vars <- num_vars()
    c_vars <- cat_vars()
    if (vtype == "bubble") {
      tagList(
        selectInput("mv_x", "Sumbu X:", choices = n_vars, selected = if ("over_time" %in% n_vars) "over_time" else n_vars[1]),
        selectInput("mv_y", "Sumbu Y:", choices = n_vars, selected = if ("actual_productivity" %in% n_vars) "actual_productivity" else n_vars[2]),
        selectInput("mv_size", "Ukuran Titik:", choices = n_vars, selected = if ("incentive" %in% n_vars) "incentive" else n_vars[3]),
        selectInput("mv_color", "Warna Kelompok:", choices = c_vars, selected = if ("department" %in% c_vars) "department" else c_vars[1])
      )
    } else if (vtype == "group_box") {
      tagList(
        selectInput("mv_bx_x", "Sumbu X (Kategorik):", choices = c_vars, selected = if ("quarter" %in% c_vars) "quarter" else c_vars[1]),
        selectInput("mv_bx_y", "Sumbu Y (Numerik):", choices = n_vars, selected = if ("actual_productivity" %in% n_vars) "actual_productivity" else n_vars[1]),
        selectInput("mv_bx_fill", "Warna Sub-Kelompok:", choices = c_vars, selected = if ("department" %in% c_vars) "department" else c_vars[min(2, length(c_vars))])
      )
    } else {
      helpText("Menggunakan variabel numerik pilihan.")
    }
  })
  
  get_multi_plot_obj <- reactive({
    vtype <- input$multi_vis_type
    df <- active_data()
    if (vtype == "heatmap") {
      req(input$multi_num_vars)
      sub_df <- na.omit(df[, input$multi_num_vars, drop = FALSE])
      ggcorrplot(cor(sub_df), lab = TRUE, hc.order = TRUE, type = "lower", colors = c("#CD5C5C", "#FFFFFF", "#008080")) + labs(title = "Correlation Heatmap")
    } else if (vtype == "pairs") {
      req(input$multi_num_vars)
      sub_df <- na.omit(df[, input$multi_num_vars[1:min(5, length(input$multi_num_vars))], drop = FALSE])
      GGally::ggpairs(sub_df) + theme_minimal()
    } else if (vtype == "bubble") {
      req(input$mv_x, input$mv_y, input$mv_size, input$mv_color)
      ggplot(df, aes(x = .data[[input$mv_x]], y = .data[[input$mv_y]], size = .data[[input$mv_size]], color = .data[[input$mv_color]])) + geom_point(alpha = 0.65) + theme_minimal()
    } else {
      req(input$mv_bx_x, input$mv_bx_y, input$mv_bx_fill)
      ggplot(df, aes(x = .data[[input$mv_bx_x]], y = .data[[input$mv_bx_y]], fill = .data[[input$mv_bx_fill]])) + geom_boxplot(alpha = 0.75) + theme_minimal()
    }
  })
  
  output$multi_vis_plot <- renderPlot({ get_multi_plot_obj() })
  output$multi_vis_interpret <- renderUI({ HTML("Visualisasi multivariat memetakan interaksi simultan antar variabel.") })
  
  # Regresi Berganda & Uji Asumsi Klasik Lengkap (4 Jenis Uji)
  reg_fit <- reactive({
    req(input$reg_y, input$reg_x)
    preds <- setdiff(input$reg_x, input$reg_y)
    validate(need(length(preds) >= 2, "Pilih minimal 2 prediktor."))
    form <- as.formula(paste(input$reg_y, "~", paste(preds, collapse = " + ")))
    df_clean <- na.omit(active_data()[, c(input$reg_y, preds), drop = FALSE])
    lm(form, data = df_clean)
  })
  
  output$reg_equation_text <- renderText({
    mod <- reg_fit()
    coefs <- coef(mod)
    eq <- paste0(input$reg_y, " = ", round(coefs[1], 4))
    for (i in 2:length(coefs)) {
      sign_str <- ifelse(coefs[i] >= 0, " + ", " - ")
      eq <- paste0(eq, sign_str, round(abs(coefs[i]), 5), " * ", names(coefs)[i])
    }
    eq
  })
  
  output$reg_fit_table <- renderTable({
    mod <- reg_fit()
    s <- summary(mod)
    fstat <- s$fstatistic
    fpval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    data.frame(
      Metrik = c("N Observasi", "R-Squared", "Adjusted R-Squared", "F-Statistic", "P-value F", "AIC", "BIC"),
      Nilai = c(nobs(mod), round(s$r.squared, 4), round(s$adj.r.squared, 4), round(fstat[1], 3), format.pval(fpval, digits = 4), round(AIC(mod), 2), round(BIC(mod), 2))
    )
  })
  
  output$reg_coef_table <- renderTable({
    s <- summary(reg_fit())
    df_c <- as.data.frame(s$coefficients)
    colnames(df_c) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    df_c$Signifikansi <- ifelse(df_c$`Pr(>|t|)` < 0.05, "Signifikan (p < 0.05)", "Tidak Signifikan")
    df_c
  }, rownames = TRUE)
  
  # 1. Uji Multikolinearitas (VIF)
  output$reg_vif_table <- renderTable({
    mod <- reg_fit()
    X <- model.matrix(mod)[, -1, drop = FALSE]
    if (ncol(X) < 2) return(data.frame(Pesan = "Butuh >= 2 prediktor"))
    vif_vals <- car::vif(mod)
    if(is.matrix(vif_vals)) vif_vals <- vif_vals[, 1]
    data.frame(Prediktor = names(vif_vals), VIF = round(vif_vals, 3), Status = ifelse(vif_vals > 10, "Multikolinearitas Tinggi (>10)", "Aman (<10)"))
  })
  
  # 2. Uji Normalitas Residual (Shapiro-Wilk)
  output$normality_test_output <- renderPrint({
    mod <- reg_fit()
    res <- residuals(mod)
    if(length(res) > 5000) res_sample <- sample(res, 5000) else res_sample <- res
    sw <- shapiro.test(res_sample)
    cat("Uji Shapiro-Wilk untuk Normalitas Residual:\n")
    print(sw)
    if(sw$p.value > 0.05) {
      cat("\nKesimpulan: P-value > 0.05, gagal tolak H0. Residual berdistribusi normal (Asumsi terpenuhi).\n")
    } else {
      cat("\nKesimpulan: P-value < 0.05, tolak H0. Residual tidak berdistribusi normal.\n")
    }
  })
  
  # 3. Uji Homoskedastisitas (Breusch-Pagan Test)
  output$homovariance_test_output <- renderPrint({
    mod <- reg_fit()
    bp <- lmtest::bptest(mod)
    cat("Uji Breusch-Pagan untuk Homoskedastisitas:\n")
    print(bp)
    if(bp$p.value > 0.05) {
      cat("\nKesimpulan: P-value > 0.05, ragam galat homogen / homoskedastisitas (Asumsi terpenuhi).\n")
    } else {
      cat("\nKesimpulan: P-value < 0.05, terjadi heteroskedastisitas.\n")
    }
  })
  
  # 4. Uji Autokorelasi (Durbin-Watson Test)
  output$autocorr_test_output <- renderPrint({
    mod <- reg_fit()
    dw <- lmtest::dwtest(mod)
    cat("Uji Durbin-Watson untuk Independensi / Autokorelasi Residual:\n")
    print(dw)
    if(dw$p.value > 0.05) {
      cat("\nKesimpulan: P-value > 0.05, tidak ada autokorelasi (Asumsi independensi terpenuhi).\n")
    } else {
      cat("\nKesimpulan: P-value < 0.05, terindikasi adanya autokorelasi.\n")
    }
  })
  
  get_reg_plot_obj <- reactive({
    mod <- reg_fit()
    ptype <- input$reg_plot_type
    if (ptype == "act_pred") {
      df_p <- data.frame(Actual = mod$model[[1]], Predicted = fitted(mod))
      ggplot(df_p, aes(x = Actual, y = Predicted)) + geom_point(color = "#1B2A4A", alpha = 0.6) + geom_abline(slope = 1, intercept = 0, color = "#CD5C5C", linetype = "dashed", linewidth = 1) + labs(title = "Actual vs Predicted Plot", x = "Aktual", y = "Prediksi") + theme_minimal()
    } else if (ptype == "res_fit") {
      df_r <- data.frame(Fitted = fitted(mod), Residuals = residuals(mod))
      ggplot(df_r, aes(x = Fitted, y = Residuals)) + geom_point(color = "#008080", alpha = 0.6) + geom_hline(yintercept = 0, color = "#CD5C5C", linetype = "dashed", linewidth = 1) + labs(title = "Residual vs Fitted Values", x = "Fitted", y = "Residual") + theme_minimal()
    } else if (ptype == "qq") {
      df_q <- data.frame(res = rstandard(mod))
      ggplot(df_q, aes(sample = res)) + stat_qq(color = "#1B2A4A", alpha = 0.6) + stat_qq_line(color = "#CD5C5C", linewidth = 1) + labs(title = "Normal Q-Q Plot", x = "Theoretical", y = "Sample") + theme_minimal()
    } else if (ptype == "res_hist") {
      df_r <- data.frame(res = residuals(mod))
      ggplot(df_r, aes(x = res)) + geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "#4682B4", color = "white", alpha = 0.8) + geom_density(color = "#CD5C5C", linewidth = 1) + labs(title = "Distribusi Residual", x = "Residual", y = "Densitas") + theme_minimal()
    } else {
      s <- summary(mod)$coefficients
      df_ci <- data.frame(Var = rownames(s)[-1], Est = s[-1, 1], Lower = s[-1, 1] - 1.96 * s[-1, 2], Upper = s[-1, 1] + 1.96 * s[-1, 2])
      ggplot(df_ci, aes(x = Var, y = Est)) + geom_point(size = 3, color = "#1B2A4A") + geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "#008080", linewidth = 1) + geom_hline(yintercept = 0, color = "#CD5C5C", linetype = "dashed") + coord_flip() + labs(title = "Selang Kepercayaan 95% Koefisien Regresi", x = "Prediktor", y = "Estimasi") + theme_minimal()
    }
  })
  
  output$reg_diag_plot <- renderPlot({ get_reg_plot_obj() })
  
  saved_models <- reactiveVal(data.frame(
    Label = character(), Respons = character(), Prediktor = character(),
    R2 = numeric(), Adj_R2 = numeric(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE
  ))
  
  observeEvent(input$save_model_btn, {
    mod <- reg_fit()
    s <- summary(mod)
    preds <- paste(setdiff(input$reg_x, input$reg_y), collapse = ", ")
    new_row <- data.frame(
      Label = input$model_label,
      Respons = input$reg_y,
      Prediktor = preds,
      R2 = round(s$r.squared, 4),
      Adj_R2 = round(s$adj.r.squared, 4),
      AIC = round(AIC(mod), 2),
      BIC = round(BIC(mod), 2),
      stringsAsFactors = FALSE
    )
    saved_models(rbind(saved_models(), new_row))
  })
  
  observeEvent(input$clear_models_btn, {
    saved_models(saved_models()[0, ])
  })
  
  output$model_comparison_table <- renderTable({ saved_models() })
  
  # Download Handlers
  output$download_cat_plot <- downloadHandler(filename = function() { paste0("univariat_kategorik_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = get_cat_plot_obj(), device = "png", width = 8, height = 5) })
  output$download_num_plot <- downloadHandler(filename = function() { paste0("univariat_numerik_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = get_num_plot_obj(), device = "png", width = 8, height = 5) })
  output$download_biv_plot <- downloadHandler(filename = function() { paste0("bivariat_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = get_biv_plot_obj(), device = "png", width = 8, height = 5) })
  output$download_multi_plot <- downloadHandler(filename = function() { paste0("multivariat_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = get_multi_plot_obj(), device = "png", width = 9, height = 6) })
  output$download_reg_plot <- downloadHandler(filename = function() { paste0("regresi_berganda_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = get_reg_plot_obj(), device = "png", width = 8, height = 5) })
}

# ==============================================================================
# 5. MENJALANKAN APLIKASI SHINY
# ==============================================================================
shinyApp(ui = ui, server = server)

