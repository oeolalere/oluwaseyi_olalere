# -----------------------------------------------------------------------------
# 0) Packages
# -----------------------------------------------------------------------------
required_pkgs <- c("ggplot2", "dplyr", "tidyr", "scales", "viridisLite", "patchwork")
for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(viridisLite)
library(patchwork)

# =============================================================================
# STUDY SETUP AND BACKTEST ENGINE  (this block is identical in every script)
# =============================================================================
# Sample    : 17 cryptocurrencies, daily prices 10 May 2021 - 31 October 2026
# Returns   : daily simple returns, R_t = P_t / P_(t-1) - 1, on every calendar
#             day (crypto trades every day, so there are no non-trading days)
# Year      : 365 days for annualisation, the risk-free rate and the window
# Risk-free : daily 3-month US T-bill rate (FRED series DTB3), carried forward
#             over weekends and holidays; daily rate = DTB3 / 100 / 365.
#             The optimiser uses the daily rate; holding-period results use
#             the daily rates compounded over each holding period.
# Method    : rolling walk-forward. At each rebalance the weights are estimated
#             from the previous LOOKBACK daily returns and then held
#             buy-and-hold (no trading) until the next rebalance. The backtest
#             runs to the end of the sample (complete holding periods only).
# Calendar  : Half Week = twice a week on fixed weekdays (gaps of 3 and 4 days,
#             i.e. Wednesdays and Saturdays, since the first out-of-sample day,
#             11 May 2022, is a Wednesday); One Week = every 7 days; Two Weeks
#             = 14; Four Weeks = 28; Eight Weeks = 56 days.
# Costs     : TRANSACTION_COST x turnover at each rebalance, where turnover =
#             sum |new weight - drifted weight| (the first period buys from cash)
# Speed     : optimised weights are cached in a small .rds file next to the
#             price file, so the slow step runs once and later scripts reuse it.
# =============================================================================

# ---- Settings ---------------------------------------------------------------
PRICE_FILE <- ""      # "" = choose Crypto_Prices_Daily_2020-2026.csv in a dialog
RF_FILE    <- ""      # "" = DTB3.csv in the same folder as the price file
START_DATE <- as.Date("2021-05-10")   # first day all 17 coins trade
END_DATE   <- as.Date("2026-10-31")

assets <- c("ETH", "Aval", "BitCash", "BNB", "BTCUSD", "Cardano", "Chainlink",
            "Dogecoin", "Litecoin", "NEAR", "Polkadot", "ShibaInu", "Solana",
            "TRON", "Uniswap", "UNUS", "XRP")

DAYS_PER_YEAR    <- 365
LOOKBACK         <- 365      # estimation window in daily returns (one year)
TRANSACTION_COST <- 0.001    # 0.1% of the value traded at each rebalance

# Holding period in calendar days. 3.5 = twice a week (alternating 3 and 4 days).
rebalance_periods <- c("Half Week" = 3.5, "One Week" = 7, "Two Weeks" = 14,
                       "Four Weeks" = 28, "Eight Weeks" = 56)
FREQ_ORDER     <- names(rebalance_periods)
ann_factors    <- DAYS_PER_YEAR / rebalance_periods    # holding periods per year
STRATEGY_ORDER <- c("Naive", "Sharpe", "Kurtosis")

# Weight controls applied to the optimised (Sharpe, Kurtosis) weights,
# as in the original code. Set USE_WEIGHT_CONTROLS <- FALSE for raw weights.
USE_WEIGHT_CONTROLS <- TRUE
blend_with_equal    <- 0.45    # share of 1/N blended into the optimised weights
max_weight          <- 0.12    # maximum weight per coin
min_visible_weight  <- 0.005   # minimum weight per coin

lookback <- LOOKBACK           # alias used further down in some scripts

# ---- Prices -> daily simple returns -----------------------------------------
if (PRICE_FILE == "") {
    cat("Select Crypto_Prices_Daily_2020-2026.csv ...\n")
    PRICE_FILE <- file.choose()
}
DATA_DIR <- dirname(PRICE_FILE)
raw_data <- read.csv(PRICE_FILE, stringsAsFactors = FALSE, check.names = FALSE)
missing_cols <- setdiff(c("Date", assets), colnames(raw_data))
if (length(missing_cols) > 0)
    stop("Columns not found in the price file: ", paste(missing_cols, collapse = ", "))

raw_data$Date <- as.Date(as.character(raw_data$Date),
                         tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d"))
raw_data <- raw_data[!is.na(raw_data$Date) &
                     raw_data$Date >= START_DATE & raw_data$Date <= END_DATE, ]
raw_data <- raw_data[order(raw_data$Date), ]

prices <- sapply(assets, function(a)
    suppressWarnings(as.numeric(gsub("[,$% ]", "", as.character(raw_data[[a]])))))
prices[!is.finite(prices) | prices <= 0] <- NA
if (anyNA(prices)) {
    bad <- colSums(is.na(prices)); bad <- bad[bad > 0]
    stop("Missing prices inside the sample window: ",
         paste(names(bad), bad, sep = " = ", collapse = ", "))
}
if (any(as.numeric(diff(raw_data$Date)) != 1))
    stop("The price file must have one row for every calendar day.")

ret_mat <- prices[-1, , drop = FALSE] / prices[-nrow(prices), , drop = FALSE] - 1
colnames(ret_mat) <- assets
ret_dates <- raw_data$Date[-1]
returns   <- as.data.frame(ret_mat)

# ---- Risk-free rate (DTB3) ----------------------------------------------------
if (RF_FILE == "") {
    RF_FILE <- file.path(DATA_DIR, "DTB3.csv")
    if (!file.exists(RF_FILE)) {
        cat("Select DTB3.csv (3-month T-bill rate from FRED) ...\n")
        RF_FILE <- file.choose()
    }
}
rf_raw  <- read.csv(RF_FILE, stringsAsFactors = FALSE)
rf_obs  <- as.Date(as.character(rf_raw[[1]]))
rf_rate <- suppressWarnings(as.numeric(rf_raw[[2]]))      # per cent per year
keep_rf <- !is.na(rf_obs) & is.finite(rf_rate)
rf_obs  <- rf_obs[keep_rf]; rf_rate <- rf_rate[keep_rf]
if (min(rf_obs) > min(ret_dates) - 1 || max(rf_obs) < max(ret_dates) - 7)
    stop("DTB3.csv must cover ", format(min(ret_dates) - 1), " to ", format(max(ret_dates)))
# Rate in effect at the start of each return day: the latest quote on or before
# the previous calendar day (carries Friday's rate over the weekend).
rf_annual_pct <- approx(as.numeric(rf_obs), rf_rate, xout = as.numeric(ret_dates - 1),
                        method = "constant", f = 0, rule = 2)$y
rf_daily <- rf_annual_pct / 100 / DAYS_PER_YEAR

cat(sprintf("Sample: %s to %s | %d daily returns | %d coins\n",
            format(min(ret_dates)), format(max(ret_dates)), nrow(ret_mat), ncol(ret_mat)))
cat(sprintf("Risk-free (DTB3): %.2f%% to %.2f%% a year, mean %.2f%%\n",
            min(rf_annual_pct), max(rf_annual_pct), mean(rf_annual_pct)))
cat(sprintf("Estimation window: %d days | out-of-sample from %s\n\n",
            LOOKBACK, format(ret_dates[LOOKBACK + 1])))

# ---- Optimisation -------------------------------------------------------------
normalize_weights <- function(w) {
    w <- pmax(w, 0)
    if (sum(w) == 0 || any(!is.finite(w))) return(rep(1 / length(w), length(w)))
    w / sum(w)
}

apply_weight_controls <- function(w) {
    n <- length(w); equal_w <- rep(1 / n, n)
    w <- normalize_weights(w)
    w <- (1 - blend_with_equal) * w + blend_with_equal * equal_w
    w <- pmax(w, min_visible_weight)
    w <- w / sum(w)
    repeat {
        over <- w > max_weight
        if (!any(over)) break
        excess  <- sum(w[over] - max_weight)
        w[over] <- max_weight
        under   <- !over
        if (sum(under) == 0) break
        w[under] <- w[under] + excess * w[under] / sum(w[under])
        if (max(w) <= max_weight + 1e-8) break
    }
    normalize_weights(w)
}

# Portfolio kurtosis of the daily portfolio returns in the window (minimised)
portfolio_kurtosis <- function(w, R, rf_vec = 0) {
    w <- normalize_weights(w)
    p <- as.numeric(R %*% w)
    m <- mean(p); s <- sd(p)
    if (!is.finite(s) || s == 0) return(1e6)
    mean(((p - m) / s)^4)
}

# Negative Sharpe ratio of daily excess returns (daily risk-free rate per day)
neg_sharpe <- function(w, R, rf_vec = 0) {
    w <- normalize_weights(w)
    p <- as.numeric(R %*% w) - rf_vec
    s <- sd(p)
    if (!is.finite(s) || s == 0) return(1e6)
    -mean(p) / s
}

optimise_portfolio <- function(R, obj_fn, rf_vec = 0) {
    n   <- ncol(R)
    ini <- rep(1 / n, n)
    res <- tryCatch(
        optim(ini, obj_fn, R = R, rf_vec = rf_vec, method = "L-BFGS-B",
              lower = rep(0, n), upper = rep(1, n), control = list(maxit = 1000)),
        error = function(e) NULL)
    w <- if (is.null(res)) ini else res$par
    if (USE_WEIGHT_CONTROLS) apply_weight_controls(w) else normalize_weights(w)
}

# ---- Weight cache (weights at row i use rows i-LOOKBACK .. i-1 only) ---------
CACHE_FILE <- file.path(DATA_DIR, "weights_cache_17coins.rds")
cache_signature <- list(assets, START_DATE, END_DATE, LOOKBACK, USE_WEIGHT_CONTROLS,
                        blend_with_equal, max_weight, min_visible_weight,
                        round(sum(ret_mat), 8), round(sum(rf_daily), 12))
.weight_cache <- new.env()
if (file.exists(CACHE_FILE)) {
    saved <- tryCatch(readRDS(CACHE_FILE), error = function(e) NULL)
    if (!is.null(saved) && identical(saved$signature, cache_signature))
        for (k in names(saved$weights)) assign(k, saved$weights[[k]], envir = .weight_cache)
}
.cache_changed <- FALSE

get_weights <- function(strategy, i) {
    key <- paste(strategy, i)
    if (exists(key, envir = .weight_cache, inherits = FALSE))
        return(get(key, envir = .weight_cache))
    rows <- (i - LOOKBACK):(i - 1)
    w <- switch(strategy,
        "Naive"    = rep(1 / length(assets), length(assets)),
        "Sharpe"   = optimise_portfolio(ret_mat[rows, , drop = FALSE], neg_sharpe, rf_daily[rows]),
        "Kurtosis" = optimise_portfolio(ret_mat[rows, , drop = FALSE], portfolio_kurtosis),
        stop("Unknown strategy: ", strategy))
    names(w) <- assets
    assign(key, w, envir = .weight_cache)
    .cache_changed <<- TRUE
    w
}

save_weight_cache <- function() {
    if (!.cache_changed) return(invisible(NULL))
    saveRDS(list(signature = cache_signature, weights = as.list(.weight_cache)), CACHE_FILE)
    .cache_changed <<- FALSE
    invisible(NULL)
}

# ---- Rebalancing calendar ---------------------------------------------------
# Start and end rows of the complete holding periods for a holding period h.
# A whole number h means every h days; h = 3.5 means twice a week on fixed
# weekdays (alternating gaps of 3 and 4 days).
rebalance_schedule <- function(h) {
    n     <- nrow(ret_mat)
    first <- LOOKBACK + 1
    gaps  <- if (h == 3.5) rep(c(3, 4), length.out = ceiling(n / 3) + 2)
             else rep(h, ceiling(n / h) + 2)
    pts   <- first + c(0, cumsum(gaps))
    ends  <- pts[-1] - 1
    pts   <- pts[-length(pts)]
    keep  <- ends <= n
    list(start = pts[keep], end = ends[keep])
}
cat(sprintf("Half Week rebalancing days: %ss and %ss\n\n",
            weekdays(ret_dates[LOOKBACK + 1]), weekdays(ret_dates[LOOKBACK + 4])))

# ---- Buy-and-hold backtest ---------------------------------------------------
# One row per complete holding period:
#   Gross    = holding-period return before costs
#   Net      = (1 - TRANSACTION_COST x Turnover) x (1 + Gross) - 1
#   RF       = risk-free return over the same days (daily rates compounded)
#   Turnover = sum |new weight - drifted weight|
# attr(, "weights") holds the target weights set at each rebalance;
# attr(, "daily") holds daily net returns when keep_daily = TRUE.
run_backtest <- function(strategy, h, tc = TRANSACTION_COST, keep_daily = FALSE) {
    sched  <- rebalance_schedule(h)
    starts <- sched$start
    K      <- length(starts)
    gross <- net <- rf <- turn <- numeric(K)
    W       <- matrix(NA_real_, K, length(assets), dimnames = list(NULL, assets))
    w_drift <- rep(0, length(assets))                  # start from cash
    daily   <- if (keep_daily) vector("list", K) else NULL
    for (k in seq_len(K)) {
        i    <- starts[k]
        rows <- i:sched$end[k]
        m    <- length(rows)
        w    <- get_weights(strategy, i)
        W[k, ]  <- w
        turn[k] <- sum(abs(w - w_drift))
        growth  <- apply(1 + ret_mat[rows, , drop = FALSE], 2, cumprod)
        if (!is.matrix(growth)) growth <- matrix(growth, nrow = 1)
        V        <- as.numeric(growth %*% w)           # value of 1 invested, no trading
        gross[k] <- V[m] - 1
        net[k]   <- (1 - tc * turn[k]) * V[m] - 1
        rf[k]    <- prod(1 + rf_daily[rows]) - 1
        w_drift  <- w * growth[m, ] / V[m]
        if (keep_daily) {
            Vn <- (1 - tc * turn[k]) * V
            daily[[k]] <- data.frame(Date = ret_dates[rows],
                                     Return = Vn / c(1, Vn[-m]) - 1,
                                     RF = rf_daily[rows])
        }
    }
    save_weight_cache()
    out <- data.frame(Period = seq_len(K), Start = ret_dates[starts],
                      End = ret_dates[sched$end], Gross = gross, Net = net,
                      RF = rf, Turnover = turn)
    attr(out, "weights") <- W
    if (keep_daily) attr(out, "daily") <- do.call(rbind, daily)
    out
}

# Net return for any cost level, from a backtest result
net_at_cost <- function(bt, cost) (1 - cost * bt$Turnover) * (1 + bt$Gross) - 1

# Fill the weight cache once for every rebalance date used by any frequency
prepare_weights <- function(strategies = c("Sharpe", "Kurtosis")) {
    idx <- sort(unique(unlist(lapply(rebalance_periods, function(h)
               rebalance_schedule(h)$start))))
    todo <- sum(sapply(strategies, function(s)
               sum(!sapply(paste(s, idx), exists, envir = .weight_cache, inherits = FALSE))))
    if (todo > 0) {
        cat(sprintf("Optimising %d weight vectors (first run only; saved to %s) ...\n",
                    todo, basename(CACHE_FILE)))
        done <- 0
        for (s in strategies) for (i in idx) {
            if (!exists(paste(s, i), envir = .weight_cache, inherits = FALSE)) {
                get_weights(s, i); done <- done + 1
                if (done %% 200 == 0) { cat(sprintf("  %d / %d\n", done, todo)); save_weight_cache() }
            }
        }
        save_weight_cache()
        cat("Done.\n\n")
    }
}
prepare_weights()
# =============================================================================
# END OF STUDY SETUP AND BACKTEST ENGINE
# =============================================================================

# -----------------------------------------------------------------------------
# 1) Asset colours and order (all 17 coins, never filtered)
# -----------------------------------------------------------------------------
ALL_ASSETS <- assets

# Viridis: ETH = yellow (first), XRP = dark purple (last)
N_ASSETS     <- length(ALL_ASSETS)
VIRIDIS_COLS <- viridis(N_ASSETS, option = "D", direction = -1)
ASSET_COLOUR <- setNames(VIRIDIS_COLS, ALL_ASSETS)

LEGEND_ORDER <- ALL_ASSETS          # ETH top -> XRP bottom in legend
# geom_col stacks the LAST factor level at the bottom and the FIRST at the
# top. Natural order ETH ... XRP puts ETH (yellow) at the TOP of each bar.
STACK_LEVELS <- ALL_ASSETS

REBAL_DAYS <- rebalance_periods

# -----------------------------------------------------------------------------
# Market regimes (out-of-sample period, dated at Bitcoin's turning points)
#   Bear (May-Nov 2022)      : 11 May 2022 - 21 Nov 2022  (Terra and FTX; BTC low 21 Nov 2022)
#   Bull (Nov 2022-Oct 2025) : 22 Nov 2022 - 06 Oct 2025  (BTC peak 6 Oct 2025)
#   Bear (Oct 2025-Jun 2026) : 07 Oct 2025 - 30 Jun 2026  (BTC low 30 Jun 2026)
# The walk-forward backtest is run once on the full sample; a holding period
# belongs to a regime if it starts and ends inside it, so the weights always
# use the full previous 365 days, whatever the regime.
# -----------------------------------------------------------------------------
REGIMES <- list(
    "Bear (May-Nov 2022)"      = c("2022-05-11", "2022-11-21"),
    "Bull (Nov 2022-Oct 2025)" = c("2022-11-22", "2025-10-06"),
    "Bear (Oct 2025-Jun 2026)" = c("2025-10-07", "2026-06-30")
)
REGIME_ORDER <- names(REGIMES)
REGIME_FILE  <- setNames(c("Bear_2022", "Bull_2022_2025", "Bear_2025_2026"), REGIME_ORDER)

in_regime <- function(bt, regime) {
    r <- as.Date(REGIMES[[regime]])
    bt$Start >= r[1] & bt$End <= r[2]
}
REGIME_TAG <- REGIME_FILE

# -----------------------------------------------------------------------------
# 5) Kurtosis weights for every regime x frequency
#    Target weights set at each rebalance in the full walk-forward backtest,
#    kept for the holding periods that fall inside each regime.
# -----------------------------------------------------------------------------
cat("Collecting Kurtosis weights for each regime...\n\n")

subperiod_weights <- data.frame()
for (freq in FREQ_ORDER) {
    bt <- run_backtest("Kurtosis", REBAL_DAYS[[freq]])
    W  <- attr(bt, "weights")
    for (regime in REGIME_ORDER) {
        sel <- which(in_regime(bt, regime))
        if (length(sel) == 0) next
        Wr  <- W[sel, , drop = FALSE]
        subperiod_weights <- rbind(subperiod_weights, data.frame(
            Holding_Period   = rep(seq_along(sel), times = ncol(Wr)),
            Asset            = rep(colnames(Wr), each = length(sel)),
            Weight           = as.numeric(Wr),
            Regime           = regime,
            Frequency        = freq,
            stringsAsFactors = FALSE
        ))
        cat(sprintf("  %-26s %-7s -> %d holding periods\n", regime, freq, length(sel)))
    }
}

# Factor ordering
subperiod_weights$Regime    <- factor(subperiod_weights$Regime,    levels = REGIME_ORDER)
subperiod_weights$Frequency <- factor(subperiod_weights$Frequency, levels = FREQ_ORDER)
subperiod_weights$Asset     <- factor(subperiod_weights$Asset,     levels = STACK_LEVELS)

write.csv(subperiod_weights, "Subperiod_Kurtosis_Weights.csv", row.names = FALSE)
cat("\nSaved: Subperiod_Kurtosis_Weights.csv\n\n")

# Integrity check — every period should have all coins summing to 1
chk <- subperiod_weights %>%
    group_by(Regime, Frequency, Holding_Period) %>%
    summarise(n_assets = n(), wsum = round(sum(Weight), 6), .groups = "drop")
cat("Rows per bar (should be", N_ASSETS, "):", paste(unique(chk$n_assets), collapse = ", "), "\n")
cat("Weight sums (should be 1):  ", paste(unique(chk$wsum),     collapse = ", "), "\n\n")
# -----------------------------------------------------------------------------
# 6) Theme — white background, grey horizontal gridlines, no strip background
# -----------------------------------------------------------------------------
theme_wt <- function(base_size = 12) {
    theme_grey(base_size = base_size) %+replace%
    theme(
        panel.background   = element_rect(fill = "white", colour = NA),
        panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.35),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.border       = element_blank(),
        plot.background    = element_rect(fill = "white", colour = NA),
        plot.margin        = margin(8, 12, 6, 8),
        plot.title         = element_text(face = "bold", size = base_size + 1,
                                          hjust = 0, margin = margin(b = 4)),
        plot.subtitle      = element_text(size = base_size - 2, colour = "grey35",
                                          hjust = 0, margin = margin(b = 6)),
        axis.title         = element_text(size = base_size - 1),
        axis.title.x       = element_text(margin = margin(t = 5)),
        axis.title.y       = element_text(margin = margin(r = 5), angle = 90),
        axis.text          = element_text(size = base_size - 2, colour = "grey25"),
        axis.ticks         = element_line(colour = "grey70", linewidth = 0.28),
        strip.background   = element_blank(),
        strip.text         = element_text(face = "bold", size = base_size - 1,
                                          margin = margin(t = 3, b = 3)),
        legend.position    = "right",
        legend.background  = element_rect(fill = "white", colour = NA),
        legend.key         = element_rect(fill = "white", colour = NA),
        legend.key.size    = unit(9, "pt"),
        legend.title       = element_text(face = "bold", size = base_size - 1),
        legend.text        = element_text(size = base_size - 4, colour = "grey15"),
        complete = TRUE
    )
}

# -----------------------------------------------------------------------------
# 7) Single stacked bar chart: one regime, one frequency
# -----------------------------------------------------------------------------
build_weight_bar <- function(regime_name, freq_name) {

    sub <- subperiod_weights %>%
        filter(Regime == regime_name, Frequency == freq_name)

    if (nrow(sub) == 0) {
        return(
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = paste0("No data\n", freq_name),
                         size = 3.4, colour = "grey50") +
                labs(title = freq_name) +
                theme_wt() +
                theme(plot.title = element_text(face = "bold", hjust = 0.5))
        )
    }

    n_periods <- n_distinct(sub$Holding_Period)
    x_brks <- if (n_periods <= 10) seq(1, n_periods) else
              pretty(1:n_periods, n = 6)
    x_brks <- x_brks[x_brks >= 1 & x_brks <= n_periods]

    ggplot(sub, aes(x = Holding_Period, y = Weight, fill = Asset)) +
        geom_col(position = position_stack(), width = 0.92, colour = NA) +
        scale_fill_manual(
            values = ASSET_COLOUR,
            name   = "Assets",
            breaks = LEGEND_ORDER,    # ETH top -> SUI bottom
            drop   = FALSE            # always show all coins
        ) +
        scale_x_continuous(breaks = x_brks, expand = expansion(add = 0.5)) +
        scale_y_continuous(
            breaks = c(0, 0.25, 0.50, 0.75, 1.00),
            labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
            expand = expansion(mult = c(0, 0.01))
        ) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(title = freq_name, x = "Holding Period", y = "Weight") +
        theme_wt(base_size = 12)
}

# -----------------------------------------------------------------------------
# 8) Save individual charts (one per regime x frequency)
# -----------------------------------------------------------------------------
cat("Saving individual regime/frequency charts...\n")

for (regime in REGIME_ORDER) {
    tag <- REGIME_TAG[[regime]]
    for (freq in FREQ_ORDER) {

        sub_n <- subperiod_weights %>%
            filter(Regime == regime, Frequency == freq)
        if (nrow(sub_n) == 0) next

        p     <- build_weight_bar(regime, freq)
        n_p   <- n_distinct(sub_n$Holding_Period)
        w_out <- max(7.0, min(15.0, 5.0 + n_p * 0.06))

        fname <- paste0(tag, "_", gsub("[^A-Za-z0-9]", "_", freq), "_Kurtosis_Weights")
        ggsave(paste0(fname, ".pdf"), p, width = w_out, height = 4.5,
               dpi = 300, device = cairo_pdf)
        ggsave(paste0(fname, ".png"), p, width = w_out, height = 4.5,
               dpi = 300, bg = "white")
        cat(sprintf("  Saved: %-46s (%d bars)\n", fname, n_p))
    }
}
cat("\n")

# -----------------------------------------------------------------------------
# 9) Combined per-regime chart (all five frequencies) in a 2-2-1 layout
#    Built with patchwork so the lone Eight Week panel is CENTRED on the
#    bottom row (facet_wrap would leave it stuck at the left).
# -----------------------------------------------------------------------------
cat("Saving combined per-regime charts...\n")

for (regime in REGIME_ORDER) {
    tag <- REGIME_TAG[[regime]]

    reg_dat <- subperiod_weights %>% filter(Regime == regime)
    if (nrow(reg_dat) == 0) {
        cat(sprintf("  %s — no data, skipped.\n", regime)); next
    }

    # One stacked-bar panel per frequency (build_weight_bar handles empties)
    p_half  <- build_weight_bar(regime, FREQ_ORDER[1])
    p_one   <- build_weight_bar(regime, FREQ_ORDER[2])
    p_two   <- build_weight_bar(regime, FREQ_ORDER[3])
    p_four  <- build_weight_bar(regime, FREQ_ORDER[4])
    p_eight <- build_weight_bar(regime, FREQ_ORDER[5])

    # 2-2-1 layout: lone Eight Week panel centred on the bottom row
    layout_design <- "
    AABB
    CCDD
    #EE#
    "

    p_comb <- p_half + p_one + p_two + p_four + p_eight +
        plot_layout(design = layout_design, heights = c(1, 1, 1),
                    guides = "collect") +
        plot_annotation(
            title    = paste0("Kurtosis Portfolio Weight Distribution — ", regime),
            subtitle = "Optimal asset weights per holding period across rebalancing frequencies",
            theme = theme(
                plot.title      = element_text(face = "bold", size = 14, hjust = 0),
                plot.subtitle   = element_text(size = 10, colour = "grey35", hjust = 0),
                plot.background = element_rect(fill = "white", colour = NA),
                plot.margin     = margin(10, 12, 10, 10)
            )
        ) &
        theme(legend.position = "right")

    ggsave(paste0(tag, "_Kurtosis_Weights_AllFreq.pdf"), p_comb,
           width = 14, height = 10, dpi = 300, device = cairo_pdf)
    ggsave(paste0(tag, "_Kurtosis_Weights_AllFreq.png"), p_comb,
           width = 14, height = 10, dpi = 300, bg = "white")
    cat(sprintf("  Saved: %s_Kurtosis_Weights_AllFreq (.pdf + .png)\n", tag))
}
cat("\n")

# -----------------------------------------------------------------------------
# 10) Final summary
# -----------------------------------------------------------------------------
cat("========================================\n")
cat("ALL OUTPUTS SAVED\n\n")
cat("Individual charts: <Regime>_<Frequency>_Kurtosis_Weights.pdf / .png\n")
cat("Combined charts  : <Regime>_Kurtosis_Weights_AllFreq.pdf / .png\n")
cat("  Regimes: Bull, Bear, Recovery\n\n")
cat("Data:\n")
cat("  Subperiod_Kurtosis_Weights.csv\n")
cat("========================================\n")
