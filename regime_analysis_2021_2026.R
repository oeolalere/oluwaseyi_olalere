# =============================================================================
# MARKET-REGIME ANALYSIS — WEIGHTS AND HOLDING-PERIOD RETURNS BY REGIME
# Naive (1/N), Sharpe maximization and Kurtosis minimization, 17 coins
# =============================================================================
# Regimes   : dated at Bitcoin's turning points in the out-of-sample period
#   Regime 1  Bear market 2021-2022 : 7 Nov 2021 - 21 Nov 2022  (peak 8 Nov 2021, low 21 Nov 2022)
#   Regime 2  Bull market 2022-2025 : 22 Nov 2022 - 6 Oct 2025  (low 21 Nov 2022, peak 6 Oct 2025)
#   Regime 3  Bear market 2025-2026 : 7 Oct 2025 - 30 Jun 2026  (peak 6 Oct 2025, low 30 Jun 2026)
# Design    : one walk-forward backtest over the whole out-of-sample period,
#             identical to Tables 3-4 and Figures 2-4 (weights estimated on the
#             previous 180 daily returns, buy-and-hold to the next rebalance,
#             0.1% cost on turnover). The results are then split by regime:
#             - weight charts: holding periods whose rebalancing date falls in
#               the regime (each bar = weights set at the start of the period)
#             - return charts: holding periods that start and end in the regime
#             - summary table: daily net returns within the regime dates
# Calendar  : Half Week = twice a week (alternating gaps of 3 and 4 days);
#             One Week = 7 days; Two Weeks = 14; Four Weeks = 28; Eight Weeks = 56.
# Outputs   : saved in a "Regime_Charts" folder next to the price file
#             Regime<k>_<name>_Sharpe_Weights.png / .pdf     (3 regimes)
#             Regime<k>_<name>_Kurtosis_Weights.png / .pdf   (3 regimes)
#             Regime<k>_<name>_Returns.png / .pdf            (3 regimes)
#             Regime_Performance.csv  (cumulative return, Sharpe, volatility, ...)
#             Regime_Weights.csv      (distance from 1/N and change per rebalance)
# Run time  : about 2 minutes. Select the price file when the dialog opens.
# =============================================================================

rm(list = ls())   # start clean, so no earlier setting is reused

for (pkg in c("ggplot2", "patchwork", "viridisLite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(ggplot2)
library(patchwork)

# ---- 1) Settings ------------------------------------------------------------
START_DATE <- as.Date("2021-05-10")
END_DATE   <- as.Date("2026-10-31")
assets <- c("ETH", "Aval", "BitCash", "BNB", "BTCUSD", "Cardano", "Chainlink",
            "Dogecoin", "Litecoin", "NEAR", "Polkadot", "ShibaInu", "Solana",
            "TRON", "Uniswap", "UNUS", "XRP")      # ETH stacked at the top of each bar
LOOKBACK   <- 180       # rolling estimation window in days (same as the main results)
COST       <- 0.001     # 0.1% of the value traded
DAYS_YEAR  <- 365
FREQS      <- c("Half Week" = 3.5, "One Week" = 7, "Two Weeks" = 14,   # days; 3.5 = twice
                "Four Weeks" = 28, "Eight Weeks" = 56)                  # a week (3 + 4 days)
STRATS     <- c("Naive", "Sharpe", "Kurtosis")
LABELS     <- c(Naive = "Naive (1/N)", Sharpe = "Sharpe maximization",
                Kurtosis = "Kurtosis minimization")
LINE_COLOURS <- c("Naive (1/N)" = "#CC0000", "Sharpe maximization" = "#0000CC",
                  "Kurtosis minimization" = "#007700")
USE_WEIGHT_CONTROLS <- TRUE   # 45% blend with 1/N and 12% cap, as in the main results
BLEND <- 0.45; CAP <- 0.12; FLOOR <- 0.005
LEGEND_KEY_CM <- 1.0; LEGEND_TEXT_PT <- 15; LEGEND_TITLE_PT <- 16

REGIMES <- list(                                   # start, end (inclusive)
    "Bear market 2021-2022" = c("2021-11-07", "2022-11-21"),
    "Bull market 2022-2025" = c("2022-11-22", "2025-10-06"),
    "Bear market 2025-2026" = c("2025-10-07", "2026-06-30"))
REGIME_FILE <- c("Regime1_Bear_2021_2022", "Regime2_Bull_2022_2025", "Regime3_Bear_2025_2026")
names(REGIME_FILE) <- names(REGIMES)
fmt_date <- function(d) sub("^0", "", format(as.Date(d), "%d %b %Y"))      # e.g. 7 Nov 2021
regime_title <- function(g) paste0(g, " (", fmt_date(REGIMES[[g]][1]), " - ", fmt_date(REGIMES[[g]][2]), ")")

# ---- 2) Data: daily simple returns and daily risk-free rate -------------------
cat("Select Crypto_Prices_Daily_2020-2026.csv ...\n")
price_file <- file.choose()
OUT_DIR <- file.path(dirname(price_file), "Regime_Charts")   # all outputs go here
dir.create(OUT_DIR, showWarnings = FALSE)
out <- function(f) file.path(OUT_DIR, f)
px <- read.csv(price_file, stringsAsFactors = FALSE, check.names = FALSE)
px$Date <- as.Date(px$Date)
px <- px[px$Date >= START_DATE & px$Date <= END_DATE, ]
px <- px[order(px$Date), ]
P  <- sapply(assets, function(a) as.numeric(gsub("[,$% ]", "", px[[a]])))
if (anyNA(P) || any(P <= 0)) stop("Missing or invalid prices in the sample window.")
R     <- P[-1, ] / P[-nrow(P), ] - 1
dates <- px$Date[-1]
n     <- nrow(R)

rf_file <- file.path(dirname(price_file), "DTB3.csv")
if (!file.exists(rf_file)) { cat("Select DTB3.csv ...\n"); rf_file <- file.choose() }
rf_raw <- read.csv(rf_file, stringsAsFactors = FALSE)
rf_d   <- as.Date(rf_raw[[1]]); rf_v <- suppressWarnings(as.numeric(rf_raw[[2]]))
ok     <- !is.na(rf_d) & is.finite(rf_v)
rf     <- approx(as.numeric(rf_d[ok]), rf_v[ok], xout = as.numeric(dates - 1),
                 method = "constant", f = 0, rule = 2)$y / 100 / DAYS_YEAR

# ---- 3) Walk-forward backtest (same engine as the main results) -------------------
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
# Returns both the daily net returns and one row per holding period
backtest <- function(strat, h) {
    sc <- schedule(h); w_drift <- rep(0, length(assets))   # first period buys from cash
    daily <- periods <- vector("list", length(sc$start))
    for (k in seq_along(sc$start)) {
        i    <- sc$start[k]
        rows <- i:min(sc$next_start[k] - 1, n)
        w    <- weights_at(strat, i)
        to   <- sum(abs(w - w_drift))
        G    <- apply(1 + R[rows, , drop = FALSE], 2, cumprod)
        if (!is.matrix(G)) G <- matrix(G, nrow = 1)
        V    <- (1 - COST * to) * as.numeric(G %*% w)       # value after costs (start = 1)
        daily[[k]]   <- data.frame(Date = dates[rows], Net = V / c(1, V[-length(V)]) - 1, RF = rf[rows])
        periods[[k]] <- data.frame(Start = dates[i], End = dates[max(rows)],
                                   Complete = sc$next_start[k] - 1 <= n, Return = V[length(V)] - 1)
        w_drift <- w * G[nrow(G), ] / sum(w * G[nrow(G), ])
    }
    list(daily = do.call(rbind, daily), periods = do.call(rbind, periods))
}
cat("Running walk-forward backtests (about 1 minute)...\n")
res <- list()
for (f in names(FREQS)) for (s in STRATS) res[[paste(f, s)]] <- backtest(s, FREQS[[f]])
cat(sprintf("Out-of-sample period: %s to %s (lookback %d days)\n\n",
            fmt_date(dates[LOOKBACK + 1]), fmt_date(max(dates)), LOOKBACK))
in_regime <- function(d, g) d >= as.Date(REGIMES[[g]][1]) & d <= as.Date(REGIMES[[g]][2])

# ---- 4) Regime summary: performance and weights -----------------------------------
perf <- do.call(rbind, lapply(names(REGIMES), function(g)
    do.call(rbind, lapply(names(FREQS), function(f) do.call(rbind, lapply(STRATS, function(s) {
        d  <- res[[paste(f, s)]]$daily;   d <- d[in_regime(d$Date, g), ]
        hp <- res[[paste(f, s)]]$periods
        hp <- hp[hp$Complete & in_regime(hp$Start, g) & in_regime(hp$End, g), ]
        ex <- d$Net - d$RF
        data.frame(Regime = g, Frequency = f, Strategy = LABELS[[s]], Days = nrow(d),
                   Cumulative_return = prod(1 + d$Net) - 1,
                   Ann_volatility    = sqrt(DAYS_YEAR) * sd(d$Net),
                   Sharpe_ratio      = sqrt(DAYS_YEAR) * mean(ex) / sd(ex),
                   Holding_periods   = nrow(hp),
                   Mean_HPR          = mean(hp$Return),
                   SD_HPR            = sd(hp$Return))
    }))))))
write.csv(perf, out("Regime_Performance.csv"), row.names = FALSE)

wsum <- do.call(rbind, lapply(c("Sharpe", "Kurtosis"), function(s)
    do.call(rbind, lapply(names(FREQS), function(f) {
        st  <- schedule(FREQS[[f]])$start
        W   <- t(sapply(st, function(i) weights_at(s, i)))
        dist <- 0.5 * rowSums(abs(W - 1 / ncol(W)))         # distance from 1/N (share of portfolio)
        chg  <- c(NA, 0.5 * rowSums(abs(diff(W))))           # change since previous rebalance
        do.call(rbind, lapply(names(REGIMES), function(g) {
            k <- in_regime(dates[st], g)
            data.frame(Regime = g, Frequency = f, Strategy = LABELS[[s]], Rebalances = sum(k),
                       Distance_from_1N = mean(dist[k]), Change_per_rebalance = mean(chg[k], na.rm = TRUE),
                       Mean_max_weight = mean(apply(W[k, , drop = FALSE], 1, max)),
                       Mean_min_weight = mean(apply(W[k, , drop = FALSE], 1, min)))
        }))
    }))))
write.csv(wsum, out("Regime_Weights.csv"), row.names = FALSE)

# Console summary: rows = frequency, columns = strategy, for each regime
wide <- function(df, col, fmt) {
    tab <- data.frame(Frequency = names(FREQS), check.names = FALSE)
    for (s in LABELS[STRATS]) if (s %in% df$Strategy)
        tab[[s]] <- sapply(names(FREQS), function(f) fmt(df[[col]][df$Frequency == f & df$Strategy == s]))
    tab
}
pct <- function(x) sprintf("%.1f%%", 100 * x); num <- function(x) sprintf("%.3f", x)
old_width <- options(width = 200)
for (g in names(REGIMES)) {
    cat("\n=====", regime_title(g), "=====\n")
    p <- perf[perf$Regime == g, ]; w <- wsum[wsum$Regime == g, ]
    cat("Cumulative return (net of costs)\n");   print(wide(p, "Cumulative_return", pct), row.names = FALSE)
    cat("Sharpe ratio (annualized)\n");          print(wide(p, "Sharpe_ratio", num), row.names = FALSE)
    cat("Annualized volatility\n");              print(wide(p, "Ann_volatility", pct), row.names = FALSE)
    cat("Distance of weights from 1/N\n");       print(wide(w, "Distance_from_1N", pct), row.names = FALSE)
    cat("Weight change per rebalance\n");        print(wide(w, "Change_per_rebalance", pct), row.names = FALSE)
}
options(old_width)

# ---- 5) Charts ----------------------------------------------------------------------
W_COLOURS <- setNames(viridisLite::viridis(length(assets), option = "D", direction = -1), assets)

weight_bar <- function(strat, g, freq) {
    st <- schedule(FREQS[[freq]])$start
    st <- st[in_regime(dates[st], g)]                 # rebalances inside the regime
    W  <- t(sapply(st, function(i) weights_at(strat, i)))
    d  <- data.frame(Holding_Period = rep(seq_along(st), times = length(assets)),
                     Asset = factor(rep(assets, each = length(st)), levels = assets),
                     Weight = as.numeric(W))
    np <- length(st)
    ggplot(d, aes(x = Holding_Period, y = Weight, fill = Asset)) +
        geom_col(width = if (np <= 70) 0.85 else 1) +
        scale_fill_manual(values = W_COLOURS, breaks = assets, drop = FALSE, name = "Asset") +
        scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                           limits = c(0, 1.0001), breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
        scale_x_continuous(expand = c(0.005, 0), breaks = unique(pmax(1, pretty(c(1, np), n = 6)))) +
        labs(title = paste(freq, "Strategy"), x = "Holding period", y = "Portfolio weight") +
        theme_classic(base_size = 11) +
        theme(plot.title      = element_text(face = "bold", hjust = 0.5),
              axis.text       = element_text(colour = "black"),
              legend.key.size = unit(LEGEND_KEY_CM, "cm"),
              legend.text     = element_text(size = LEGEND_TEXT_PT),
              legend.title    = element_text(size = LEGEND_TITLE_PT, face = "bold")) +
        guides(fill = guide_legend(ncol = 1))
}

return_panel <- function(g, freq) {
    d <- do.call(rbind, lapply(STRATS, function(s) {
        hp <- res[[paste(freq, s)]]$periods
        hp <- hp[hp$Complete & in_regime(hp$Start, g) & in_regime(hp$End, g), ]
        data.frame(Start = hp$Start, Return = hp$Return, Strategy = LABELS[[s]])
    }))
    d$Strategy <- factor(d$Strategy, levels = LABELS)
    ylim <- c(-1, 1) * max(abs(d$Return)) * 1.05                 # symmetric around zero
    ggplot(d, aes(x = Start, y = Return, colour = Strategy)) +
        geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
        geom_line(linewidth = 0.45, alpha = 0.9) +
        geom_point(size = if (length(unique(d$Start)) <= 12) 1.2 else 0,   # points only when few periods
                   show.legend = FALSE) +
        scale_colour_manual(values = LINE_COLOURS, name = NULL) +
        scale_y_continuous(labels = function(x) paste0(formatC(100 * x, format = "fg", digits = 3), "%"),
                           limits = ylim) +
        scale_x_date(date_labels = "%b %Y", expand = expansion(mult = c(0.02, 0.06))) +
        labs(title = paste(freq, "Strategy Returns"), x = "Start of holding period",
             y = "Holding-period return") +
        theme_bw(base_size = 12) +
        theme(plot.title       = element_text(face = "bold", hjust = 0.5),
              axis.text        = element_text(colour = "black"),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(colour = "grey90", linewidth = 0.25),
              legend.position  = "bottom",
              legend.text      = element_text(size = 14),
              legend.key.width = unit(1.6, "cm")) +
        guides(colour = guide_legend(override.aes = list(linewidth = 1.4, alpha = 1)))
}

layout_221 <- function(panels, legend_pos, title) {
    wrap_plots(A = panels[[1]], B = panels[[2]], C = panels[[3]], D = panels[[4]], E = panels[[5]],
               design = "AABB\nCCDD\n#EE#") +
        plot_layout(guides = "collect") +
        plot_annotation(title = title,
                        theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) &
        theme(legend.position = legend_pos)
}
save_fig <- function(fig, name, w, h) {
    ggsave(out(paste0(name, ".png")), fig, width = w, height = h, dpi = 300, bg = "white")
    ggsave(out(paste0(name, ".pdf")), fig, width = w, height = h)
}

cat("\nDrawing 9 figures (3 regimes x Sharpe weights, Kurtosis weights, returns)...\n")
for (g in names(REGIMES)) {
    for (s in c("Sharpe", "Kurtosis")) {
        fig <- layout_221(lapply(names(FREQS), function(f) weight_bar(s, g, f)), "right",
                          paste0(LABELS[[s]], " weights: ", g))
        save_fig(fig, paste0(REGIME_FILE[[g]], "_", s, "_Weights"), 15, 13)
    }
    fig <- layout_221(lapply(names(FREQS), function(f) return_panel(g, f)), "bottom",
                      paste0("Holding-period returns: ", g))
    save_fig(fig, paste0(REGIME_FILE[[g]], "_Returns"), 14, 13.5)
    print(fig)
}
cat("\nAll regime charts and tables are saved in:\n  ", normalizePath(OUT_DIR), "\n")
