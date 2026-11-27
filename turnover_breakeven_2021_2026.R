if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(openxlsx)

# ---- 1) Settings ------------------------------------------------------------
START_DATE <- as.Date("2021-05-10")
END_DATE   <- as.Date("2026-10-31")
assets <- c("ETH", "Aval", "BitCash", "BNB", "BTCUSD", "Cardano", "Chainlink",
            "Dogecoin", "Litecoin", "NEAR", "Polkadot", "ShibaInu", "Solana",
            "TRON", "Uniswap", "UNUS", "XRP")
LOOKBACK   <- 180       # rolling estimation window in days
BASE_COST  <- 0.001     # 10 bps = 0.1% of value traded (baseline in Tables 3-4)
BE_MAX     <- 0.10      # search break-even costs up to 1,000 bps
DAYS_YEAR  <- 365
FREQS      <- c("Half Week" = 3.5, "One Week" = 7, "Two Weeks" = 14,   # days; 3.5 = twice
                "Four Weeks" = 28, "Eight Weeks" = 56)                  # a week (3 + 4 days)
STRATS     <- c("Naive", "Sharpe", "Kurtosis")
LABELS     <- c(Naive = "Naive (1/N)", Sharpe = "Sharpe maximization",
                Kurtosis = "Kurtosis minimization")
USE_WEIGHT_CONTROLS <- TRUE   # 45% blend with 1/N and 12% cap; FALSE = unconstrained
BLEND <- 0.45; CAP <- 0.12; FLOOR <- 0.005
fmt_date <- function(d) sub("^0", "", format(d, "%d %b %Y"))      # e.g. 7 Nov 2021

# ---- 2) Data: daily simple returns and daily risk-free rate -------------------
cat("Select Crypto_Prices_Daily_2020-2026.csv ...\n")
price_file <- file.choose()
OUT_DIR <- file.path(dirname(price_file), "Main_Results")   # same folder as Tables 1-4
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


# ---- 3) Weights (same optimizer and weight limits as the main results) -----------
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

# ---- 4) Backtest: gross value path and turnover of every holding period -------
backtest <- function(strat, h) {
    sc <- schedule(h); w_drift <- rep(0, length(assets))     # first period buys from cash
    periods <- vector("list", length(sc$start))
    for (k in seq_along(sc$start)) {
        i    <- sc$start[k]
        rows <- i:min(sc$next_start[k] - 1, n)                # last period cut short at 31 Oct 2026
        w    <- weights_at(strat, i)
        G    <- apply(1 + R[rows, , drop = FALSE], 2, cumprod)
        if (!is.matrix(G)) G <- matrix(G, nrow = 1)
        periods[[k]] <- list(rows = rows, Vg = as.numeric(G %*% w),   # value before costs (start = 1)
                             TO = sum(abs(w - w_drift)))              # turnover at this rebalance
        w_drift <- w * G[nrow(G), ] / sum(w * G[nrow(G), ])
    }
    periods
}
# Daily net returns for any cost level c (cost paid on the rebalancing day)
daily_net <- function(bt, c) unlist(lapply(bt, function(p) {
    V <- (1 - c * p$TO) * p$Vg; V / c(1, V[-length(V)]) - 1 }))

cat("Running walk-forward backtests (about 1 minute)...\n")
res <- list()
for (f in names(FREQS)) for (s in STRATS) res[[paste(f, s)]] <- backtest(s, FREQS[[f]])
oos_rows  <- unlist(lapply(res[[1]], `[[`, "rows"))
rf_oos    <- rf[oos_rows]
OOS_START <- dates[min(oos_rows)]; OOS_END <- dates[max(oos_rows)]
YEARS     <- length(oos_rows) / DAYS_YEAR

# ---- 5) Performance at a given cost, and break-even costs ---------------------
sharpe_at <- function(bt, c) { ex <- daily_net(bt, c) - rf_oos; sqrt(DAYS_YEAR) * mean(ex) / sd(ex) }
wealth_at <- function(bt, c) prod(1 + daily_net(bt, c))
break_even <- function(active, naive, metric) {
    gap <- function(c) metric(active, c) - metric(naive, c)
    if (gap(0) <= 0) return(NA_real_)          # does not beat 1/N even at zero cost
    if (gap(BE_MAX) > 0) return(Inf)           # still ahead at the top of the search range
    uniroot(gap, c(0, BE_MAX), tol = 1e-8)$root
}

tab <- do.call(rbind, lapply(names(FREQS), function(f) {
    nv <- res[[paste(f, "Naive")]]
    do.call(rbind, lapply(STRATS, function(s) {
        bt <- res[[paste(f, s)]]; to <- sapply(bt, `[[`, "TO")[-1]   # skip the purchase from cash
        data.frame(Frequency = f, Strategy = LABELS[[s]], Rebalances = length(bt),
                   Turnover_per_rebalance = mean(to),
                   Turnover_per_year      = sum(to) / YEARS,
                   Sharpe_0bps   = sharpe_at(bt, 0), Sharpe_10bps = sharpe_at(bt, BASE_COST),
                   Wealth_0bps   = wealth_at(bt, 0), Wealth_10bps = wealth_at(bt, BASE_COST),
                   BE_Sharpe_bps = if (s == "Naive") NA else 1e4 * break_even(bt, nv, sharpe_at),
                   BE_Wealth_bps = if (s == "Naive") NA else 1e4 * break_even(bt, nv, wealth_at),
                   check.names = FALSE)
    }))
}))
rownames(tab) <- NULL
write.csv(tab, out("Table6_Turnover_BreakEven.csv"), row.names = FALSE)

# ---- 6) Table layout: rows = frequency ------------------------------------------
pick <- function(col, s) sapply(names(FREQS), function(f) tab[[col]][tab$Frequency == f & tab$Strategy == LABELS[[s]]])
fmt_be <- function(x) ifelse(is.na(x), "-", ifelse(is.infinite(x), paste0("> ", 1e4 * BE_MAX),
                                                   formatC(x, format = "f", digits = 0, big.mark = ",")))
table6 <- data.frame(Frequency = names(FREQS),
    TO_N = pick("Turnover_per_rebalance", "Naive"), TO_S = pick("Turnover_per_rebalance", "Sharpe"),
    TO_K = pick("Turnover_per_rebalance", "Kurtosis"),
    BS_S = pick("BE_Sharpe_bps", "Sharpe"), BS_K = pick("BE_Sharpe_bps", "Kurtosis"),
    BW_S = pick("BE_Wealth_bps", "Sharpe"), BW_K = pick("BE_Wealth_bps", "Kurtosis"))
SUBHEAD <- c("Frequency", "1/N", "Sharpe max.", "Kurtosis min.", "Sharpe max.", "Kurtosis min.",
             "Sharpe max.", "Kurtosis min.")
GROUPS  <- list(list("Average turnover per rebalance (%)", 2:4),
                list("Break-even cost: Sharpe ratio (bps)", 5:6),
                list("Break-even cost: terminal wealth (bps)", 7:8))

shown <- table6
for (j in 2:4) shown[[j]] <- sprintf("%.1f%%", 100 * table6[[j]])
for (j in 5:8) shown[[j]] <- fmt_be(table6[[j]])
names(shown) <- c("Frequency", "TO: 1/N", "TO: Sharpe", "TO: Kurtosis",
                  "BE Sharpe ratio: Sharpe", "BE Sharpe ratio: Kurtosis",
                  "BE wealth: Sharpe", "BE wealth: Kurtosis")
WINDOW_TXT <- paste(fmt_date(OOS_START), "-", fmt_date(OOS_END))
old_width <- options(width = 200)
cat("\nTABLE 6: Turnover and break-even transaction costs, ", WINDOW_TXT, " (lookback ", LOOKBACK, " days)\n", sep = "")
print(shown, row.names = FALSE)
cat("TO = average turnover per rebalance; BE = break-even cost in bps; '-' = does not beat 1/N even at zero cost.\n")
chk <- data.frame(Frequency = names(FREQS),
                  `1/N` = sprintf("%.3f", pick("Sharpe_10bps", "Naive")),
                  `Sharpe max.` = sprintf("%.3f", pick("Sharpe_10bps", "Sharpe")),
                  `Kurtosis min.` = sprintf("%.3f", pick("Sharpe_10bps", "Kurtosis")), check.names = FALSE)
cat("\nCheck: Sharpe ratios at 10 bps (should equal Table 3, Panel C)\n"); print(chk, row.names = FALSE)
options(old_width)

# ---- 7) Excel workbook ---------------------------------------------------------------
st_title <- createStyle(textDecoration = "bold", fontSize = 12)
st_group <- createStyle(textDecoration = "bold", halign = "center", border = "Top", wrapText = TRUE)
st_head  <- createStyle(textDecoration = "bold", halign = "center", border = "Bottom")
st_pct   <- createStyle(numFmt = "0.0%", halign = "center")
st_txt   <- createStyle(halign = "center")
st_note  <- createStyle(fontSize = 9, wrapText = TRUE, valign = "top")
st_last  <- createStyle(border = "Bottom")
sh <- "Table 6"
wb <- createWorkbook(); addWorksheet(wb, sh, gridLines = FALSE)
writeData(wb, sh, "Table 6. Turnover and break-even transaction costs", startRow = 1)
addStyle(wb, sh, st_title, 1, 1)
for (g in GROUPS) {                                            # group headers over their columns
    writeData(wb, sh, g[[1]], startRow = 3, startCol = g[[2]][1])
    mergeCells(wb, sh, cols = g[[2]], rows = 3)
    addStyle(wb, sh, st_group, 3, g[[2]], gridExpand = TRUE)
}
addStyle(wb, sh, st_group, 3, 1)
writeData(wb, sh, t(SUBHEAD), startRow = 4, colNames = FALSE)
addStyle(wb, sh, st_head, 4, 1:8, gridExpand = TRUE)
body <- table6
for (j in 5:8) body[[j]] <- fmt_be(table6[[j]])
writeData(wb, sh, body, startRow = 5, colNames = FALSE)
rows <- 4 + seq_len(nrow(body))
addStyle(wb, sh, st_pct, rows, 2:4, gridExpand = TRUE)
addStyle(wb, sh, st_txt, rows, 5:8, gridExpand = TRUE)
addStyle(wb, sh, st_last, max(rows), 1:8, gridExpand = TRUE, stack = TRUE)
notes <- paste0(
    "Notes: Walk-forward out-of-sample results, ", WINDOW_TXT, ". Weights are estimated on the previous ", LOOKBACK,
    " daily returns and held buy-and-hold to the next rebalance. ",
    if (USE_WEIGHT_CONTROLS) "Optimized weights are blended 45% with 1/N and capped at 12% per coin. " else
        "Optimized weights are long-only and unconstrained. ",
    "Turnover at a rebalance is the sum over assets of |w(i,t) - w(i,t-)|, where w(i,t-) is the weight after ",
    "drifting with returns since the previous rebalance (DeMiguel, Garlappi and Uppal, 2009); the initial purchase ",
    "from cash is excluded from the average. Net returns are (1 - c x turnover)(1 + gross return) - 1, with c the ",
    "proportional cost per unit traded; 1 bp = 0.01%. The break-even cost is the value of c at which the strategy's ",
    "net Sharpe ratio (or terminal wealth) equals that of the 1/N portfolio at the same rebalancing frequency, both ",
    "paying c. '-' indicates that the strategy does not outperform 1/N even at zero cost. The baseline cost in ",
    "Tables 3 and 4 is 10 bps.")
nr <- max(rows) + 2
writeData(wb, sh, notes, startRow = nr); mergeCells(wb, sh, cols = 1:8, rows = nr:(nr + 6))
addStyle(wb, sh, st_note, nr, 1)
setColWidths(wb, sh, 1:8, c(13, 9, 11, 12, 11, 12, 11, 12))
setRowHeights(wb, sh, 3, 30)
pageSetup(wb, sh, orientation = "landscape", fitToWidth = TRUE)
saveWorkbook(wb, out("Table6_Turnover_BreakEven.xlsx"), overwrite = TRUE)
cat("\nTable 6 saved in:\n  ", normalizePath(OUT_DIR), "\n")
