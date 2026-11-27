for (pkg in c("ggplot2", "patchwork", "viridisLite", "openxlsx")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(ggplot2)
library(patchwork)
library(openxlsx)

# ---- 1) Settings ------------------------------------------------------------
START_DATE <- as.Date("2021-05-10")
END_DATE   <- as.Date("2026-10-31")
# Table 1 order: price-file column -> name in the table -> ticker
COINS <- data.frame(
    col  = c("BTCUSD", "ETH", "BNB", "XRP", "Solana", "Dogecoin", "Cardano",
             "TRON", "Aval", "ShibaInu", "Chainlink", "BitCash", "Polkadot",
             "Litecoin", "NEAR", "Uniswap", "UNUS"),
    name = c("Bitcoin", "Ethereum", "BNB", "XRP", "Solana", "Dogecoin", "Cardano",
             "TRON", "Avalanche", "Shiba Inu", "Chainlink", "Bitcoin Cash",
             "Polkadot", "Litecoin", "NEAR Protocol", "Uniswap", "UNUS SED LEO"),
    tick = c("BTC", "ETH", "BNB", "XRP", "SOL", "DOGE", "ADA", "TRX", "AVAX",
             "SHIB", "LINK", "BCH", "DOT", "LTC", "NEAR", "UNI", "LEO"),
    stringsAsFactors = FALSE)
# Portfolio order (Figure 1 stacks the first coin, ETH, at the top of each bar)
assets <- c("ETH", "Aval", "BitCash", "BNB", "BTCUSD", "Cardano", "Chainlink",
            "Dogecoin", "Litecoin", "NEAR", "Polkadot", "ShibaInu", "Solana",
            "TRON", "Uniswap", "UNUS", "XRP")
LOOKBACK   <- 180       # rolling estimation window in days
COST       <- 0.001     # 0.1% of the value traded
DAYS_YEAR  <- 365
FREQS      <- c("Half Week" = 3.5, "One Week" = 7, "Two Weeks" = 14,   # days; 3.5 = twice
                "Four Weeks" = 28, "Eight Weeks" = 56)                  # a week (3 + 4 days)
STRATS     <- c("Naive", "Sharpe", "Kurtosis")
LABELS     <- c(Naive = "Naive (1/N)", Sharpe = "Sharpe maximization",
                Kurtosis = "Kurtosis minimization")
USE_WEIGHT_CONTROLS <- TRUE   # 45% blend with 1/N and 12% cap; FALSE = unconstrained
BLEND <- 0.45; CAP <- 0.12; FLOOR <- 0.005
B_BOOT <- 9999          # bootstrap resamples (Table 3)
BLOCK  <- NULL          # bootstrap block length in days; NULL = T^(1/3) rounded
SEED   <- 2026          # same resamples for every test; results reproducible
LEGEND_KEY_CM <- 1.0; LEGEND_TEXT_PT <- 15; LEGEND_TITLE_PT <- 16   # Figure 1 legend
SUFFIX <- if (USE_WEIGHT_CONTROLS) "" else "_Unconstrained"
fmt_date <- function(d) sub("^0", "", format(d, "%d %b %Y"))      # e.g. 7 Nov 2021

# ---- 2) Data: daily simple returns and daily risk-free rate -------------------
cat("Select Crypto_Prices_Daily_2020-2026.csv ...\n")
price_file <- file.choose()
OUT_DIR <- file.path(dirname(price_file), "Main_Results")   # all outputs go here
dir.create(OUT_DIR, showWarnings = FALSE)
out <- function(f) file.path(OUT_DIR, f)
px <- read.csv(price_file, stringsAsFactors = FALSE, check.names = FALSE)
px$Date <- as.Date(px$Date)
px <- px[px$Date >= START_DATE & px$Date <= END_DATE, ]
px <- px[order(px$Date), ]
P  <- sapply(assets, function(a) as.numeric(gsub("[,$% ]", "", px[[a]])))
if (anyNA(P) || any(P <= 0)) stop("Missing or invalid prices in the sample window.")
R     <- P[-1, ] / P[-nrow(P), ] - 1          # R_t = P_t / P_(t-1) - 1
dates <- px$Date[-1]
n     <- nrow(R)

rf_file <- file.path(dirname(price_file), "DTB3.csv")
if (!file.exists(rf_file)) { cat("Select DTB3.csv ...\n"); rf_file <- file.choose() }
rf_raw <- read.csv(rf_file, stringsAsFactors = FALSE)
rf_d   <- as.Date(rf_raw[[1]]); rf_v <- suppressWarnings(as.numeric(rf_raw[[2]]))
ok     <- !is.na(rf_d) & is.finite(rf_v)
rf     <- approx(as.numeric(rf_d[ok]), rf_v[ok], xout = as.numeric(dates - 1),
                 method = "constant", f = 0, rule = 2)$y / 100 / DAYS_YEAR

# ---- 3) Table 1: descriptive statistics of daily returns ----------------------
# Skewness S = m3 / m2^1.5, excess kurtosis K = m4 / m2^2 - 3 (central moments),
# Jarque-Bera = n/6 (S^2 + K^2/4) ~ chi-squared(2) under normality
describe <- function(x) {
    d <- x - mean(x); m2 <- mean(d^2)
    S <- mean(d^3) / m2^1.5; K <- mean(d^4) / m2^2 - 3; JB <- length(x) / 6 * (S^2 + K^2 / 4)
    c(Mean = mean(x), SD = sd(x), Mean_ann = DAYS_YEAR * mean(x), Vol_ann = sqrt(DAYS_YEAR) * sd(x),
      Min = min(x), Max = max(x), Skew = S, ExKurt = K, JB = JB,
      JB_p = pchisq(JB, df = 2, lower.tail = FALSE))
}
R1    <- R[, COINS$col]
st1   <- rbind(t(apply(R1, 2, describe)), describe(rowMeans(R1)))   # 1/N rebalanced daily
cm    <- cor(R1)
avg_c <- c(sapply(seq_len(ncol(cm)), function(j) mean(cm[-j, j])), NA)
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
f2    <- function(x, d) formatC(x, format = "f", digits = d, big.mark = ",")
table1 <- data.frame(
    Asset                 = c(paste0(COINS$name, " (", COINS$tick, ")"), "Equally weighted portfolio (1/N)"),
    `Mean (%)`            = f2(100 * st1[, "Mean"], 3),
    `Std. dev. (%)`       = f2(100 * st1[, "SD"], 2),
    `Ann. mean (%)`       = f2(100 * st1[, "Mean_ann"], 1),
    `Ann. volatility (%)` = f2(100 * st1[, "Vol_ann"], 1),
    `Min (%)`             = f2(100 * st1[, "Min"], 1),
    `Max (%)`             = f2(100 * st1[, "Max"], 1),
    Skewness              = f2(st1[, "Skew"], 2),
    `Excess kurtosis`     = f2(st1[, "ExKurt"], 2),
    `Jarque-Bera`         = paste0(f2(st1[, "JB"], 0), stars(st1[, "JB_p"])),
    `Avg. correlation`    = ifelse(is.na(avg_c), "", f2(avg_c, 2)),
    check.names = FALSE, stringsAsFactors = FALSE)

# ---- 4) Walk-forward backtest -------------------------------------------------------
norm_w <- function(w) { w <- pmax(w, 0)
    if (sum(w) == 0 || any(!is.finite(w))) rep(1 / length(w), length(w)) else w / sum(w) }
controls <- function(w) {
    k <- length(w); w <- norm_w(w)
    w <- (1 - BLEND) * w + BLEND / k
    w <- pmax(w, FLOOR); w <- w / sum(w)
    repeat {
        over <- w > CAP; if (!any(over)) break
        excess <- sum(w[over] - CAP); w[over] <- CAP
        if (all(over)) break
        w[!over] <- w[!over] + excess * w[!over] / sum(w[!over])
        if (max(w) <= CAP + 1e-8) break
    }
    norm_w(w)
}
neg_sharpe <- function(w, X, rfv) { p <- as.numeric(X %*% norm_w(w)) - rfv
    s <- sd(p); if (!is.finite(s) || s == 0) 1e6 else -mean(p) / s }
kurt <- function(w, X, rfv) { p <- as.numeric(X %*% norm_w(w))
    m <- mean(p); s <- sd(p); if (!is.finite(s) || s == 0) 1e6 else mean(((p - m) / s)^4) }
optimise <- function(fn, X, rfv) {
    k <- ncol(X)
    res <- tryCatch(optim(rep(1 / k, k), fn, X = X, rfv = rfv, method = "L-BFGS-B",
                          lower = rep(0, k), upper = rep(1, k), control = list(maxit = 1000)),
                    error = function(e) NULL)
    w <- if (is.null(res)) rep(1 / k, k) else res$par
    if (USE_WEIGHT_CONTROLS) controls(w) else norm_w(w)
}
cache <- new.env()                      # weights at day i use days i-LOOKBACK .. i-1
weights_at <- function(strat, i) {
    key <- paste(strat, i)
    if (!is.null(cache[[key]])) return(cache[[key]])
    rows <- (i - LOOKBACK):(i - 1)
    w <- switch(strat,
        Naive    = rep(1 / length(assets), length(assets)),
        Sharpe   = optimise(neg_sharpe, R[rows, ], rf[rows]),
        Kurtosis = optimise(kurt,       R[rows, ], rf[rows]))
    cache[[key]] <- w; w
}
# Rebalancing days: every h days, or twice a week (gaps of 3 and 4) if h = 3.5
schedule <- function(h) {
    gaps <- if (h == 3.5) rep(c(3, 4), length.out = n) else rep(h, n)
    pts  <- (LOOKBACK + 1) + c(0, cumsum(gaps))
    list(start = pts[pts <= n], next_start = pts[-1][pts[-length(pts)] <= n])
}
# Daily net returns of a buy-and-hold portfolio rebalanced on the schedule
backtest_daily <- function(strat, h) {
    sc <- schedule(h); w_drift <- rep(0, length(assets))   # first period buys from cash
    daily <- vector("list", length(sc$start))
    for (k in seq_along(sc$start)) {
        i    <- sc$start[k]
        rows <- i:min(sc$next_start[k] - 1, n)   # last period cut short at 31 Oct 2026
        w    <- weights_at(strat, i)
        to   <- sum(abs(w - w_drift))            # turnover
        G    <- apply(1 + R[rows, , drop = FALSE], 2, cumprod)
        if (!is.matrix(G)) G <- matrix(G, nrow = 1)
        V    <- (1 - COST * to) * as.numeric(G %*% w)   # value after costs (start = 1)
        daily[[k]] <- data.frame(Date = dates[rows], Net = V / c(1, V[-length(V)]) - 1, RF = rf[rows])
        w_drift <- w * G[nrow(G), ] / sum(w * G[nrow(G), ])
    }
    do.call(rbind, daily)
}
cat("Running walk-forward backtests (about 1 minute)...\n")
res <- list()
for (f in names(FREQS)) for (s in STRATS) res[[paste(f, s)]] <- backtest_daily(s, FREQS[[f]])
OOS_START <- dates[LOOKBACK + 1]; OOS_END <- max(dates)
T_obs <- unique(sapply(res, nrow))
if (length(T_obs) != 1) stop("Portfolios do not cover the same days.")
WINDOW_TXT <- paste(fmt_date(OOS_START), "-", fmt_date(OOS_END))
WEIGHT_TXT <- if (USE_WEIGHT_CONTROLS) {
    "Optimized weights are blended 45% with 1/N and capped at 12% per coin."
} else "Optimized weights are long-only and unconstrained (no blend with 1/N, no cap)."

# ---- 5) Table 2: performance net of costs ---------------------------------------------
perf <- function(r, rfv) {
    ex <- r - rfv; W <- cumprod(1 + r); peak <- cummax(c(1, W))[-1]
    c(Mean    = DAYS_YEAR * mean(r),
      Vol     = sqrt(DAYS_YEAR) * sd(r),
      Sharpe  = sqrt(DAYS_YEAR) * mean(ex) / sd(ex),
      Sortino = sqrt(DAYS_YEAR) * mean(ex) / sqrt(mean(pmin(ex, 0)^2)),
      MaxDD   = min(W / peak - 1),
      TW      = W[length(W)])
}
pf <- lapply(res, function(d) perf(d$Net, d$RF))
PANELS2 <- list(
    A = list(col = "Mean",    title = "Annualized mean return",     fmt = "pct", best = max),
    B = list(col = "Vol",     title = "Annualized volatility",      fmt = "pct", best = min),
    C = list(col = "Sharpe",  title = "Sharpe ratio (annualized)",  fmt = "num", best = max),
    D = list(col = "Sortino", title = "Sortino ratio (annualized)", fmt = "num", best = max),
    E = list(col = "MaxDD",   title = "Maximum drawdown",           fmt = "pct", best = max),
    F = list(col = "TW",      title = paste0("Terminal wealth of $1 invested on ", fmt_date(OOS_START),
                                             " (cumulative return in brackets)"), fmt = "tw", best = max))
table2_panel <- function(col) {
    tab <- data.frame(Frequency = names(FREQS), check.names = FALSE)
    for (s in STRATS) tab[[LABELS[[s]]]] <- sapply(names(FREQS), function(f) pf[[paste(f, s)]][[col]])
    tab
}
fmt_val <- function(x, fmt) switch(fmt,
    pct = sprintf("%.2f%%", 100 * x), num = sprintf("%.3f", x),
    tw  = sprintf("%.3f (%+.1f%%)", x, 100 * (x - 1)))

# ---- 6) Table 3: bootstrap tests of Sharpe and Sortino differences against 1/N ------
# Studentized circular block bootstrap (Ledoit and Wolf, 2008). Each ratio is a
# function of m1 = E[x] and m2 = E[g(x)] of daily excess returns x; standard
# errors by the delta method with a block (batch-means) long-run covariance.
b <- if (is.null(BLOCK)) round(T_obs^(1 / 3)) else BLOCK
n_blocks <- ceiling(T_obs / b)
RATIOS <- list(
    Sharpe  = list(g  = function(x) x^2,  f = function(m1, m2) m1 / sqrt(m2 - m1^2),
                   d1 = function(m1, m2) m2 / (m2 - m1^2)^1.5,
                   d2 = function(m1, m2) -m1 / (2 * (m2 - m1^2)^1.5)),
    Sortino = list(g  = function(x) pmin(x, 0)^2, f = function(m1, m2) m1 / sqrt(m2),
                   d1 = function(m1, m2) 1 / sqrt(m2),
                   d2 = function(m1, m2) -m1 / (2 * m2^1.5)))
psi_hat <- function(Y) {
    l  <- floor(nrow(Y) / b)
    Yc <- sweep(Y[seq_len(l * b), , drop = FALSE], 2, colMeans(Y))
    Z  <- rowsum(Yc, rep(seq_len(l), each = b)) / sqrt(b)
    crossprod(Z) / l
}
diff_se <- function(Y, rt) {
    m    <- colMeans(Y)
    d    <- rt$f(m[1], m[3]) - rt$f(m[2], m[4])
    grad <- c(rt$d1(m[1], m[3]), -rt$d1(m[2], m[4]), rt$d2(m[1], m[3]), -rt$d2(m[2], m[4]))
    unname(c(d, sqrt(drop(grad %*% psi_hat(Y) %*% grad) / nrow(Y))))
}
lw_test <- function(x1, x2, ratio) {             # x1 = strategy, x2 = 1/N
    rt <- RATIOS[[ratio]]
    Y  <- cbind(x1, x2, rt$g(x1), rt$g(x2))
    s0 <- diff_se(Y, rt)
    set.seed(SEED)                               # identical resamples in every test
    t_star <- replicate(B_BOOT, {
        starts <- sample.int(T_obs, n_blocks, replace = TRUE)
        idx    <- (as.vector(outer(0:(b - 1), starts, "+")) - 1) %% T_obs + 1
        s      <- diff_se(Y[idx[seq_len(T_obs)], ], rt)
        abs(s[1] - s0[1]) / s[2]
    })
    c(Diff = sqrt(DAYS_YEAR) * s0[1],            # annualized difference
      p    = (1 + sum(t_star >= abs(s0[1]) / s0[2])) / (B_BOOT + 1))
}
cat(sprintf("Bootstrap tests: %d resamples, %d-day blocks (about 1-2 minutes)...\n", B_BOOT, b))
tests <- do.call(rbind, lapply(names(RATIOS), function(ratio)
    do.call(rbind, lapply(names(FREQS), function(f) do.call(rbind, lapply(c("Sharpe", "Kurtosis"), function(s) {
        x1 <- res[[paste(f, s)]]; x2 <- res[[paste(f, "Naive")]]
        data.frame(Ratio = ratio, Frequency = f, Strategy = s,
                   t(lw_test(x1$Net - x1$RF, x2$Net - x2$RF, ratio)))
    }))))))
tests$p_Holm <- ave(tests$p, tests$Ratio, FUN = function(p) p.adjust(p, method = "holm"))
PANELS3 <- c(Sharpe  = "Panel A: Difference in annualized Sharpe ratios",
             Sortino = "Panel B: Difference in annualized Sortino ratios")
table3_panel <- function(ratio) {
    d <- tests[tests$Ratio == ratio, ]
    tab <- data.frame(Frequency = names(FREQS))
    for (s in c("Sharpe", "Kurtosis")) {
        tab[[paste(s, "Diff")]] <- d$Diff[d$Strategy == s]
        tab[[paste(s, "p")]]    <- d$p[d$Strategy == s]
    }
    names(tab) <- c("Frequency", "Sharpe max. - 1/N", "p-value", "Kurtosis min. - 1/N", "p-value")
    tab
}

# ---- 7) Figure 1: Sharpe-maximization weights at every rebalance ------------------
wts <- do.call(rbind, lapply(names(FREQS), function(f) {
    starts <- schedule(FREQS[[f]])$start
    W <- t(sapply(starts, function(i) weights_at("Sharpe", i)))   # already estimated
    data.frame(Frequency = f, Holding_Period = rep(seq_along(starts), times = length(assets)),
               Asset = rep(assets, each = length(starts)), Weight = as.numeric(W))
}))
wts$Asset <- factor(wts$Asset, levels = assets)      # first level stacked on top
COLOURS <- setNames(viridisLite::viridis(length(assets), option = "D", direction = -1), assets)
weight_bar <- function(freq) {
    d <- wts[wts$Frequency == freq, ]; np <- max(d$Holding_Period)
    ggplot(d, aes(x = Holding_Period, y = Weight, fill = Asset)) +
        geom_col(width = if (np <= 70) 0.85 else 1) +
        scale_fill_manual(values = COLOURS, breaks = assets, drop = FALSE, name = "Asset") +
        scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                           limits = c(0, 1.0001), breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
        scale_x_continuous(expand = c(0.005, 0), breaks = pretty(c(1, np), n = 6)) +
        labs(title = paste(freq, "Strategy"), x = "Holding period", y = "Portfolio weight") +
        theme_classic(base_size = 11) +
        theme(plot.title      = element_text(face = "bold", hjust = 0.5),
              axis.text       = element_text(colour = "black"),
              legend.key.size = unit(LEGEND_KEY_CM, "cm"),
              legend.text     = element_text(size = LEGEND_TEXT_PT),
              legend.title    = element_text(size = LEGEND_TITLE_PT, face = "bold")) +
        guides(fill = guide_legend(ncol = 1))
}
p <- lapply(names(FREQS), weight_bar)
fig1 <- wrap_plots(A = p[[1]], B = p[[2]], C = p[[3]], D = p[[4]], E = p[[5]],
                   design = "AABB\nCCDD\n#EE#") +
    plot_layout(guides = "collect") & theme(legend.position = "right")
print(fig1)
ggsave(out(paste0("Figure1_Sharpe_Weights", SUFFIX, ".png")), fig1,
       width = 15, height = 12.5, dpi = 300, bg = "white")
ggsave(out(paste0("Figure1_Sharpe_Weights", SUFFIX, ".pdf")), fig1, width = 15, height = 12.5)

# ---- 8) Print the tables ---------------------------------------------------------------
old_width <- options(width = 220)
cat("\nTABLE 1: Descriptive statistics of daily simple returns,",
    fmt_date(min(dates)), "-", fmt_date(max(dates)), "\n")
print(table1, row.names = FALSE)
cat("\nTABLE 2: Out-of-sample performance, ", WINDOW_TXT, ", net of 0.1% transaction costs\n", sep = "")
for (k in names(PANELS2)) {
    pn <- PANELS2[[k]]; tab <- table2_panel(pn$col)
    for (j in 2:4) tab[[j]] <- fmt_val(tab[[j]], pn$fmt)
    cat("Panel ", k, ": ", pn$title, "\n", sep = ""); print(tab, row.names = FALSE); cat("\n")
}
cat("TABLE 3: Bootstrap tests against Naive (1/N), ", WINDOW_TXT, ", net of costs\n", sep = "")
for (ratio in names(PANELS3)) {
    tab <- table3_panel(ratio)
    for (j in c(2, 4)) tab[[j]] <- sprintf("%+.3f", tab[[j]])
    for (j in c(3, 5)) tab[[j]] <- sprintf("%.3f", tab[[j]])
    cat(PANELS3[[ratio]], "\n"); print(tab, row.names = FALSE); cat("\n")
}
cat(sprintf("Smallest Holm-adjusted p-value: %.3f (Sharpe), %.3f (Sortino)\n",
            min(tests$p_Holm[tests$Ratio == "Sharpe"]), min(tests$p_Holm[tests$Ratio == "Sortino"])))
options(old_width)

# ---- 9) Excel workbook: Tables 1-3 with notes ----------------------------------------
st_title <- createStyle(textDecoration = "bold", fontSize = 12)
st_panel <- createStyle(textDecoration = c("bold", "italic"))
st_head  <- createStyle(textDecoration = "bold", border = "TopBottom", halign = "center", wrapText = TRUE)
st_cell  <- createStyle(halign = "center")
st_note  <- createStyle(fontSize = 9, wrapText = TRUE, valign = "top")
NUMFMT   <- c(pct = "0.00%", num = "0.000", tw = "0.000")
new_sheet <- function(wb, sh, title) {
    addWorksheet(wb, sh, gridLines = FALSE)
    writeData(wb, sh, title, startRow = 1); addStyle(wb, sh, st_title, 1, 1)
}
add_notes <- function(wb, sh, text, row, ncol, nrows = 5) {
    writeData(wb, sh, text, startRow = row)
    mergeCells(wb, sh, cols = 1:ncol, rows = row:(row + nrows))
    addStyle(wb, sh, st_note, row, 1)
}
wb <- createWorkbook()

# Table 1
new_sheet(wb, "Table 1", "Table 1. Descriptive statistics of daily returns")
writeData(wb, "Table 1", table1, startRow = 3, headerStyle = st_head)
addStyle(wb, "Table 1", st_cell, 3 + seq_len(nrow(table1)), 2:ncol(table1), gridExpand = TRUE)
add_notes(wb, "Table 1", paste0(
    "Notes: Daily simple returns R_t = P_t / P_(t-1) - 1, ", fmt_date(min(dates)), " - ", fmt_date(max(dates)),
    " (", n, " daily observations per coin). Mean, standard deviation, minimum and maximum are daily, in percent. ",
    "Annualized mean = daily mean x 365; annualized volatility = daily standard deviation x sqrt(365). ",
    "Skewness = m3 / m2^(3/2) and excess kurtosis = m4 / m2^2 - 3, where mk is the k-th central sample moment. ",
    "Jarque-Bera = n/6 x (S^2 + K^2/4), chi-squared with 2 degrees of freedom under normality; ",
    "*** p < 0.01, ** p < 0.05, * p < 0.10. Avg. correlation is the mean Pearson correlation of daily returns ",
    "with the other 16 coins. The equally weighted portfolio holds 1/17 in each coin, rebalanced daily."),
    row = 5 + nrow(table1), ncol = ncol(table1), nrows = 3)
setColWidths(wb, "Table 1", 1:ncol(table1), c(32, rep(11, ncol(table1) - 1)))
setRowHeights(wb, "Table 1", 3, 32)
pageSetup(wb, "Table 1", orientation = "landscape", fitToWidth = TRUE)

# Table 2
new_sheet(wb, "Table 2", paste0("Table 2. Out-of-sample performance, ", WINDOW_TXT, ", net of transaction costs"))
r <- 3
for (k in names(PANELS2)) {
    pn <- PANELS2[[k]]; tab <- table2_panel(pn$col); shown <- tab
    if (pn$fmt == "tw") for (j in 2:4) shown[[j]] <- fmt_val(tab[[j]], "tw")
    writeData(wb, "Table 2", paste0("Panel ", k, ": ", pn$title), startRow = r)
    addStyle(wb, "Table 2", st_panel, r, 1)
    writeData(wb, "Table 2", shown, startRow = r + 1, headerStyle = st_head)
    addStyle(wb, "Table 2", createStyle(numFmt = NUMFMT[[pn$fmt]], halign = "center"),
             r + 1 + seq_len(nrow(tab)), 2:4, gridExpand = TRUE)
    for (i in seq_len(nrow(tab))) {                       # best value per row in bold
        v <- as.numeric(tab[i, 2:4])
        addStyle(wb, "Table 2", createStyle(numFmt = NUMFMT[[pn$fmt]], halign = "center",
                                            textDecoration = "bold"), r + 1 + i, 1 + which(v == pn$best(v)))
    }
    r <- r + nrow(tab) + 3
}
add_notes(wb, "Table 2", paste0(
    "Notes: Walk-forward out-of-sample results over the common window ", WINDOW_TXT, " (", T_obs,
    " daily returns for every portfolio). Weights are estimated on the previous ", LOOKBACK,
    " daily returns and held buy-and-hold to the next rebalance. ", WEIGHT_TXT,
    " Returns are net of a 0.1% cost on turnover. Annualized mean = 365 x mean daily return; ",
    "volatility = sqrt(365) x daily standard deviation. Sharpe and Sortino ratios use daily excess returns over ",
    "the 3-month T-bill rate (DTB3), annualized by sqrt(365); the Sortino downside deviation uses the risk-free ",
    "rate as target. Maximum drawdown is the largest peak-to-trough fall in wealth. Terminal wealth is the value on ",
    fmt_date(OOS_END), " of $1 invested on ", fmt_date(OOS_START), "; cumulative return = terminal wealth - 1. ",
    "Best value in each row in bold."), row = r, ncol = 4, nrows = 6)
setColWidths(wb, "Table 2", 1:4, c(20, 20, 22, 24))

# Table 3
new_sheet(wb, "Table 3", "Table 3. Bootstrap tests of Sharpe and Sortino ratio differences against Naive (1/N)")
r <- 3
for (ratio in names(PANELS3)) {
    tab <- table3_panel(ratio)
    writeData(wb, "Table 3", PANELS3[[ratio]], startRow = r); addStyle(wb, "Table 3", st_panel, r, 1)
    writeData(wb, "Table 3", tab, startRow = r + 1, headerStyle = st_head)
    rows <- r + 1 + seq_len(nrow(tab))
    addStyle(wb, "Table 3", createStyle(numFmt = "+0.000;-0.000;0.000", halign = "center"), rows, c(2, 4), gridExpand = TRUE)
    addStyle(wb, "Table 3", createStyle(numFmt = "0.000", halign = "center"), rows, c(3, 5), gridExpand = TRUE)
    for (j in c(3, 5)) for (i in which(tab[[j]] < 0.05))          # p < 0.05 in bold
        addStyle(wb, "Table 3", createStyle(numFmt = "0.000", halign = "center", textDecoration = "bold"), r + 1 + i, j)
    r <- r + nrow(tab) + 3
}
add_notes(wb, "Table 3", paste0(
    "Notes: Differences are annualized Sharpe or Sortino ratios of the optimized strategy minus those of the Naive (1/N) ",
    "portfolio at the same rebalancing frequency, from daily excess returns net of costs (", T_obs, " observations, ",
    WINDOW_TXT, "). Two-sided p-values from the studentized circular block bootstrap of Ledoit and Wolf (2008), ",
    "with ", B_BOOT, " resamples and a block length of ", b, " days; the two return series are resampled jointly. ",
    if (min(tests$p_Holm) > 0.9995) "Holm-adjusted p-values for the 10 comparisons of each ratio all equal 1.000. " else
        sprintf("Holm-adjusted p-values for the 10 comparisons of each ratio are at least %.3f (Sharpe) and %.3f (Sortino). ",
                min(tests$p_Holm[tests$Ratio == "Sharpe"]), min(tests$p_Holm[tests$Ratio == "Sortino"])),
    "p-values below 0.05 in bold."), row = r, ncol = 5, nrows = 5)
setColWidths(wb, "Table 3", 1:5, c(14, 19, 10, 21, 10))

saveWorkbook(wb, out(paste0("Main_Results_Tables", SUFFIX, ".xlsx")), overwrite = TRUE)
cat("\nTables 1-3 and Figure 1 saved in:\n  ", normalizePath(OUT_DIR), "\n")
