}
# Generate efficient frontier
efficient_frontier <- generate_efficient_frontier(crypto_data$cov_matrix, crypto_data$mean_returns)
# Minimum Variance Portfolio (MVP)
mvp_weights <- optimize_portfolio(crypto_data$cov_matrix, crypto_data$mean_returns, short_allowed = FALSE)
mvp_point <- portfolio_return_risk(mvp_weights, crypto_data$cov_matrix, crypto_data$mean_returns)
# Tangency Portfolio
tangency_weights <- optimize_portfolio(crypto_data$cov_matrix, crypto_data$mean_returns, risk_free_rate = risk_free_rate, short_allowed = FALSE)
tangency_point <- portfolio_return_risk(tangency_weights, crypto_data$cov_matrix, crypto_data$mean_returns)
# Add MVP and Tangency Portfolio to the frontier data
efficient_frontier$Type <- "Frontier"
mvp_df <- data.frame(Return = mvp_point["Return"], Risk = mvp_point["Risk"], Type = "MVP")
tangency_df <- data.frame(Return = tangency_point["Return"], Risk = tangency_point["Risk"], Type = "Tangency")
# Combine data for visualization
plot_data <- rbind(efficient_frontier, mvp_df, tangency_df)
# Plot the efficient frontier
ggplot(plot_data, aes(x = Risk, y = Return, color = Type)) +
geom_point(data = efficient_frontier, aes(x = Risk, y = Return), size = 1, alpha = 0.7) +
geom_point(data = mvp_df, aes(x = Risk, y = Return), color = "red", size = 3) +
geom_point(data = tangency_df, aes(x = Risk, y = Return), color = "blue", size = 3) +
labs(
title = "Efficient Frontier with Optimal Portfolios",
x = "Portfolio Risk (Standard Deviation)",
y = "Portfolio Return"
) +
theme_minimal() +
scale_color_manual(values = c("Frontier" = "gray", "MVP" = "red", "Tangency" = "blue"))
# Load necessary libraries
library(quadprog)
library(ggplot2)
library(dplyr)
# Specify cryptocurrency assets
cryptos <- c("ETH", "Aval", "BNB", "BitCash", "BTCUSD", "Cardano", "Chainlink",
"Dogecoin", "Litecoin", "NEAR", "Polkadot", "ShibaInu", "Solana",
"TetherUSDt", "Toncoin", "TRON", "Uniswap", "UNUS", "XRP", "SUI")
# Simulate data for cryptocurrencies
simulate_crypto_data <- function(cryptos) {
n_assets <- length(cryptos)
cov_matrix <- matrix(runif(n_assets^2, min = 0.0001, max = 0.002), n_assets, n_assets)
cov_matrix <- cov_matrix + t(cov_matrix)  # Make symmetric
diag(cov_matrix) <- diag(cov_matrix) + 0.001  # Ensure positive definiteness
cov_matrix <- as.matrix(nearPD(cov_matrix)$mat)  # Ensure positive definiteness
mean_returns <- runif(n_assets, 0.001, 0.02)  # Simulated mean returns
list(cov_matrix = cov_matrix, mean_returns = mean_returns)
}
crypto_data <- simulate_crypto_data(cryptos)
risk_free_rate <- 0.002
# Monte Carlo Simulation
simulate_portfolios <- function(cov_matrix, mean_returns, num_portfolios = 5000) {
n_assets <- ncol(cov_matrix)
results <- matrix(NA, nrow = num_portfolios, ncol = 3)  # Risk, Return, Sharpe Ratio
weights_matrix <- matrix(NA, nrow = num_portfolios, ncol = n_assets)
for (i in 1:num_portfolios) {
weights <- runif(n_assets)
weights <- weights / sum(weights)  # Normalize weights to sum to 1
portfolio_return <- sum(weights * mean_returns)
portfolio_risk <- sqrt(t(weights) %*% cov_matrix %*% weights)
sharpe_ratio <- (portfolio_return - risk_free_rate) / portfolio_risk
results[i, ] <- c(portfolio_risk, portfolio_return, sharpe_ratio)
weights_matrix[i, ] <- weights
}
list(results = results, weights = weights_matrix)
}
# Run the Monte Carlo simulation
mc_simulation <- simulate_portfolios(crypto_data$cov_matrix, crypto_data$mean_returns)
# Extract results
mc_results <- as.data.frame(mc_simulation$results)
colnames(mc_results) <- c("Risk", "Return", "SharpeRatio")
mc_weights <- mc_simulation$weights
# Find the optimal portfolios
min_risk_index <- which.min(mc_results$Risk)
max_sharpe_index <- which.max(mc_results$SharpeRatio)
min_risk_portfolio <- mc_results[min_risk_index, ]
max_sharpe_portfolio <- mc_results[max_sharpe_index, ]
# Visualize the Efficient Frontier
ggplot(mc_results, aes(x = Risk, y = Return)) +
geom_point(aes(color = SharpeRatio), alpha = 0.5) +
scale_color_gradient(low = "blue", high = "red") +
geom_point(data = min_risk_portfolio, aes(x = Risk, y = Return), color = "green", size = 4) +
geom_point(data = max_sharpe_portfolio, aes(x = Risk, y = Return), color = "orange", size = 4) +
labs(
title = "Robust Efficient Frontier with Optimal Portfolios",
x = "Portfolio Risk (Standard Deviation)",
y = "Portfolio Return",
color = "Sharpe Ratio"
) +
theme_minimal()
# Display the optimal portfolio weights
min_risk_weights <- mc_weights[min_risk_index, ]
max_sharpe_weights <- mc_weights[max_sharpe_index, ]
optimal_weights <- data.frame(
Asset = cryptos,
Min_Risk_Weights = min_risk_weights,
Max_Sharpe_Weights = max_sharpe_weights
)
print("Optimal Portfolio Weights:")
print(optimal_weights)
# Load necessary libraries
library(quadprog)
library(ggplot2)
library(dplyr)
# Specify cryptocurrency assets
cryptos <- c("ETH", "Aval", "BNB", "BitCash", "BTCUSD", "Cardano", "Chainlink",
"Dogecoin", "Litecoin", "NEAR", "Polkadot", "ShibaInu", "Solana",
"TetherUSDt", "Toncoin", "TRON", "Uniswap", "UNUS", "XRP", "SUI")
# Simulate data for cryptocurrencies
simulate_crypto_data <- function(cryptos) {
n_assets <- length(cryptos)
cov_matrix <- matrix(runif(n_assets^2, min = 0.0001, max = 0.002), n_assets, n_assets)
cov_matrix <- cov_matrix + t(cov_matrix)  # Make symmetric
diag(cov_matrix) <- diag(cov_matrix) + 0.001  # Ensure positive definiteness
cov_matrix <- as.matrix(nearPD(cov_matrix)$mat)  # Ensure positive definiteness
mean_returns <- runif(n_assets, 0.001, 0.02)  # Simulated mean returns
list(cov_matrix = cov_matrix, mean_returns = mean_returns)
}
crypto_data <- simulate_crypto_data(cryptos)
risk_free_rate <- 0.002
# Monte Carlo Simulation with Optimization
simulate_portfolios <- function(cov_matrix, mean_returns, num_portfolios = 10000) {
n_assets <- ncol(cov_matrix)
results <- matrix(NA, nrow = num_portfolios, ncol = 3)  # Risk, Return, Sharpe Ratio
weights_matrix <- matrix(NA, nrow = num_portfolios, ncol = n_assets)
for (i in 1:num_portfolios) {
weights <- runif(n_assets)
weights <- weights / sum(weights)  # Normalize weights to sum to 1
portfolio_return <- sum(weights * mean_returns)
portfolio_risk <- sqrt(t(weights) %*% cov_matrix %*% weights)
sharpe_ratio <- (portfolio_return - risk_free_rate) / portfolio_risk
results[i, ] <- c(portfolio_risk, portfolio_return, sharpe_ratio)
weights_matrix[i, ] <- weights
}
list(results = results, weights = weights_matrix)
}
# Generate the Efficient Frontier Curve
generate_efficient_frontier <- function(cov_matrix, mean_returns, num_points = 100) {
target_returns <- seq(min(mean_returns), max(mean_returns), length.out = num_points)
frontier <- sapply(target_returns, function(target) {
weights <- optimize_portfolio(cov_matrix, mean_returns, target_return = target, short_allowed = FALSE)
portfolio_return_risk(weights, cov_matrix, mean_returns)
})
as.data.frame(t(frontier))
}
# Optimize Portfolio Weights
optimize_portfolio <- function(cov_matrix, mean_returns, target_return = NULL, short_allowed = TRUE) {
n <- length(mean_returns)
Dmat <- cov_matrix
dvec <- rep(0, n)
# Constraints
Amat <- cbind(1, diag(n))
bvec <- c(1, rep(0, n))  # Constraint: sum of weights = 1, non-negativity
if (!short_allowed) {
Amat <- cbind(Amat, diag(n))  # Add non-negativity constraints
bvec <- c(bvec, rep(0, n))
}
if (!is.null(target_return)) {
Amat <- cbind(Amat, mean_returns)  # Add target return constraint
bvec <- c(bvec, target_return)
}
tryCatch({
weights <- solve.QP(Dmat, dvec, Amat, bvec, meq = ifelse(is.null(target_return), 1, 2))$solution
weights
}, error = function(e) {
rep(NA, n)  # Return NA if optimization fails
})
}
# Calculate Portfolio Risk and Return
portfolio_return_risk <- function(weights, cov_matrix, mean_returns) {
return_val <- sum(weights * mean_returns)
risk_val <- sqrt(t(weights) %*% cov_matrix %*% weights)
c(Return = return_val, Risk = risk_val)
}
# Monte Carlo Simulation
mc_simulation <- simulate_portfolios(crypto_data$cov_matrix, crypto_data$mean_returns, num_portfolios = 10000)
# Extract Results
mc_results <- as.data.frame(mc_simulation$results)
colnames(mc_results) <- c("Risk", "Return", "SharpeRatio")
# Find Optimal Portfolios
min_risk_index <- which.min(mc_results$Risk)
max_sharpe_index <- which.max(mc_results$SharpeRatio)
min_risk_portfolio <- mc_results[min_risk_index, ]
max_sharpe_portfolio <- mc_results[max_sharpe_index, ]
# Generate Efficient Frontier
efficient_frontier <- generate_efficient_frontier(crypto_data$cov_matrix, crypto_data$mean_returns)
# Add Portfolio Types for Visualization
efficient_frontier$Type <- "Efficient Frontier"
min_risk_portfolio$Type <- "Minimum Variance Portfolio"
max_sharpe_portfolio$Type <- "Tangency Portfolio"
# Visualization
ggplot(mc_results, aes(x = Risk, y = Return)) +
geom_point(aes(color = SharpeRatio), alpha = 0.4) +
geom_line(data = efficient_frontier, aes(x = Risk, y = Return, color = Type), size = 1.2) +
geom_point(data = min_risk_portfolio, aes(x = Risk, y = Return), color = "green", size = 4) +
geom_point(data = max_sharpe_portfolio, aes(x = Risk, y = Return), color = "orange", size = 4) +
labs(
title = "Efficient Frontier with Robust Portfolios",
x = "Portfolio Risk (Standard Deviation)",
y = "Portfolio Return",
color = "Sharpe Ratio"
) +
theme_minimal()
# Add Portfolio Types for Visualization
efficient_frontier$Type <- "Efficient Frontier"
min_risk_portfolio$Type <- "Minimum Variance Portfolio"
max_sharpe_portfolio$Type <- "Tangency Portfolio"
# Combine all data for visualization
optimal_portfolios <- rbind(
data.frame(Risk = min_risk_portfolio$Risk, Return = min_risk_portfolio$Return, Type = "Minimum Variance Portfolio"),
data.frame(Risk = max_sharpe_portfolio$Risk, Return = max_sharpe_portfolio$Return, Type = "Tangency Portfolio")
)
# Visualization
ggplot(mc_results, aes(x = Risk, y = Return)) +
geom_point(aes(color = SharpeRatio), alpha = 0.4) +  # Monte Carlo portfolios
geom_line(data = efficient_frontier, aes(x = Risk, y = Return, group = Type, color = Type), size = 1.2) +  # Efficient frontier
geom_point(data = optimal_portfolios, aes(x = Risk, y = Return, color = Type), size = 4) +  # Highlight optimal portfolios
labs(
title = "Efficient Frontier with Robust Portfolios",
x = "Portfolio Risk (Standard Deviation)",
y = "Portfolio Return",
color = "Legend"
) +
scale_color_manual(
values = c(
"Efficient Frontier" = "blue",
"Minimum Variance Portfolio" = "green",
"Tangency Portfolio" = "orange",
"SharpeRatio" = "red"
)
) +
theme_minimal()
# Add Portfolio Types for Visualization
efficient_frontier$Type <- "Efficient Frontier"
optimal_portfolios <- rbind(
data.frame(Risk = min_risk_portfolio$Risk, Return = min_risk_portfolio$Return, Type = "Minimum Variance Portfolio"),
data.frame(Risk = max_sharpe_portfolio$Risk, Return = max_sharpe_portfolio$Return, Type = "Tangency Portfolio")
)
# Visualization
ggplot() +
# Monte Carlo portfolios (continuous color scale for Sharpe Ratio)
geom_point(data = mc_results, aes(x = Risk, y = Return, color = SharpeRatio), alpha = 0.4) +
# Efficient Frontier (discrete color scale for "Type")
geom_line(data = efficient_frontier, aes(x = Risk, y = Return, group = Type), color = "blue", size = 1.2) +
# Highlight Optimal Portfolios (MVP and Tangency Portfolio)
geom_point(data = optimal_portfolios, aes(x = Risk, y = Return, color = Type), size = 4) +
labs(
title = "Efficient Frontier with Robust Portfolios",
x = "Portfolio Risk (Standard Deviation)",
y = "Portfolio Return",
color = "Legend"
) +
# Continuous color scale for Sharpe Ratio
scale_color_gradient(low = "blue", high = "red", name = "Sharpe Ratio") +
# Theme adjustments
theme_minimal()
# Add Portfolio Types for Visualization
efficient_frontier$Type <- "Efficient Frontier"
optimal_portfolios <- rbind(
data.frame(Risk = min_risk_portfolio$Risk, Return = min_risk_portfolio$Return, Type = "Minimum Variance Portfolio"),
data.frame(Risk = max_sharpe_portfolio$Risk, Return = max_sharpe_portfolio$Return, Type = "Tangency Portfolio")
)
# Combine all data
combined_data <- rbind(
data.frame(mc_results, Type = NA),  # Monte Carlo portfolios
efficient_frontier,                # Efficient frontier
optimal_portfolios                 # Optimal portfolios
)
# Add Portfolio Types for Visualization
optimal_portfolios <- rbind(
data.frame(Risk = min_risk_portfolio$Risk, Return = min_risk_portfolio$Return, SharpeRatio = NA, Type = "Minimum Variance Portfolio"),
data.frame(Risk = max_sharpe_portfolio$Risk, Return = max_sharpe_portfolio$Return, SharpeRatio = NA, Type = "Tangency Portfolio")
)
# Add Type column to efficient_frontier
efficient_frontier <- efficient_frontier %>%
mutate(SharpeRatio = NA, Type = "Efficient Frontier")
# Add Type column to Monte Carlo results
mc_results <- mc_results %>%
mutate(Type = "Monte Carlo Portfolios")
# Combine all data with consistent columns
combined_data <- rbind(
mc_results,  # Monte Carlo portfolios
efficient_frontier,  # Efficient frontier
optimal_portfolios  # Optimal portfolios
)
# Visualization
ggplot(combined_data, aes(x = Risk, y = Return)) +
# Monte Carlo portfolios with continuous color for Sharpe Ratio
geom_point(data = mc_results, aes(color = SharpeRatio), alpha = 0.5) +
# Efficient Frontier with a fixed color
geom_line(data = efficient_frontier, color = "blue", size = 1.2) +
# Highlight Optimal Portfolios (MVP and Tangency Portfolio) with a discrete shape
geom_point(data = optimal_portfolios, aes(shape = Type), size = 4, color = "black") +
labs(
title = "Efficient Frontier with Robust Portfolios",
x = "Portfolio Risk (Standard Deviation)",
y = "Portfolio Return",
color = "Sharpe Ratio",
shape = "Portfolio Type"
) +
# Continuous color scale for Sharpe Ratio
scale_color_gradient(low = "blue", high = "red") +
# Theme adjustments
theme_minimal()
# Load necessary libraries
library(quadprog)
library(ggplot2)
library(dplyr)
# Specify cryptocurrency assets
cryptos <- c("ETH", "Aval", "BNB", "BitCash", "BTCUSD", "Cardano", "Chainlink",
"Dogecoin", "Litecoin", "NEAR", "Polkadot", "ShibaInu", "Solana",
"TetherUSDt", "Toncoin", "TRON", "Uniswap", "UNUS", "XRP", "SUI")
# Simulate data for cryptocurrencies
simulate_crypto_data <- function(cryptos) {
n_assets <- length(cryptos)
cov_matrix <- matrix(runif(n_assets^2, min = 0.0001, max = 0.002), n_assets, n_assets)
cov_matrix <- cov_matrix + t(cov_matrix)  # Make symmetric
diag(cov_matrix) <- diag(cov_matrix) + 0.001  # Ensure positive definiteness
cov_matrix <- as.matrix(nearPD(cov_matrix)$mat)  # Ensure positive definiteness
mean_returns <- runif(n_assets, 0.001, 0.02)  # Simulated mean returns
list(cov_matrix = cov_matrix, mean_returns = mean_returns)
}
crypto_data <- simulate_crypto_data(cryptos)
risk_free_rate <- 0.002
# Monte Carlo Simulation with Optimization
simulate_portfolios <- function(cov_matrix, mean_returns, num_portfolios = 5000) {
n_assets <- ncol(cov_matrix)
results <- matrix(NA, nrow = num_portfolios, ncol = 3)  # Risk, Return, Sharpe Ratio
weights_matrix <- matrix(NA, nrow = num_portfolios, ncol = n_assets)
for (i in 1:num_portfolios) {
weights <- runif(n_assets)
weights <- weights / sum(weights)  # Normalize weights to sum to 1
portfolio_return <- sum(weights * mean_returns)
portfolio_risk <- sqrt(t(weights) %*% cov_matrix %*% weights)
sharpe_ratio <- (portfolio_return - risk_free_rate) / portfolio_risk
results[i, ] <- c(portfolio_risk, portfolio_return, sharpe_ratio)
weights_matrix[i, ] <- weights
}
list(results = results, weights = weights_matrix)
}
# Generate Efficient Frontier Curve
generate_efficient_frontier <- function(cov_matrix, mean_returns, num_points = 100) {
target_returns <- seq(min(mean_returns), max(mean_returns), length.out = num_points)
frontier <- sapply(target_returns, function(target) {
weights <- optimize_portfolio(cov_matrix, mean_returns, target_return = target, short_allowed = FALSE)
portfolio_return_risk(weights, cov_matrix, mean_returns)
})
as.data.frame(t(frontier))
}
# Optimize Portfolio Weights
optimize_portfolio <- function(cov_matrix, mean_returns, target_return = NULL, short_allowed = TRUE) {
n <- length(mean_returns)
Dmat <- cov_matrix
dvec <- rep(0, n)
# Constraints
Amat <- cbind(1, diag(n))
bvec <- c(1, rep(0, n))  # Constraint: sum of weights = 1, non-negativity
if (!short_allowed) {
Amat <- cbind(Amat, diag(n))  # Add non-negativity constraints
bvec <- c(bvec, rep(0, n))
}
if (!is.null(target_return)) {
Amat <- cbind(Amat, mean_returns)  # Add target return constraint
bvec <- c(bvec, target_return)
}
tryCatch({
weights <- solve.QP(Dmat, dvec, Amat, bvec, meq = ifelse(is.null(target_return), 1, 2))$solution
weights
}, error = function(e) {
rep(NA, n)  # Return NA if optimization fails
})
}
# Calculate Portfolio Risk and Return
portfolio_return_risk <- function(weights, cov_matrix, mean_returns) {
return_val <- sum(weights * mean_returns)
risk_val <- sqrt(t(weights) %*% cov_matrix %*% weights)
c(Return = return_val, Risk = risk_val)
}
# Monte Carlo Simulation
mc_simulation <- simulate_portfolios(crypto_data$cov_matrix, crypto_data$mean_returns, num_portfolios = 10000)
# Extract Results
mc_results <- as.data.frame(mc_simulation$results)
colnames(mc_results) <- c("Risk", "Return", "SharpeRatio")
# Generate Efficient Frontier
efficient_frontier <- generate_efficient_frontier(crypto_data$cov_matrix, crypto_data$mean_returns)
# Asset Positions on the Efficient Frontier
asset_positions <- data.frame(
Asset = cryptos,
Risk = sqrt(diag(crypto_data$cov_matrix)),
Return = crypto_data$mean_returns
)
# Visualization
ggplot() +
# Monte Carlo portfolios with Sharpe Ratio as gradient
geom_point(data = mc_results, aes(x = Risk, y = Return, color = SharpeRatio), alpha = 0.4) +
# Efficient Frontier Curve
geom_line(data = efficient_frontier, aes(x = Risk, y = Return), color = "blue", size = 1.2) +
# Asset Positions
geom_point(data = asset_positions, aes(x = Risk, y = Return), color = "red", size = 3) +
geom_text(data = asset_positions, aes(x = Risk, y = Return, label = Asset), hjust = -0.1, vjust = 0.5, size = 3) +
labs(
title = "Efficient Frontier with Asset Positions",
x = "Portfolio Risk (Standard Deviation)",
y = "Portfolio Return",
color = "Sharpe Ratio"
) +
scale_color_gradient(low = "blue", high = "red") +
theme_minimal()
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
# Define parameters
strategies <- c("EW", "MV", "RP", "MW", "MS", "MU")  # Portfolio strategies
transaction_costs <- seq(0, 0.1, by = 0.01)          # Transaction cost values
rebalancing_periods <- seq(1, 360, by = 10)          # Rebalancing periods
# Simulate Sharpe Ratios and Utility vs Transaction Costs
simulate_transaction_costs <- function(strategies, transaction_costs) {
data <- expand.grid(Strategy = strategies, TransactionCost = transaction_costs)
data$SharpeRatio <- with(data, 3.5 - TransactionCost * ifelse(Strategy == "EW", 0.4, 0.3))
data$Utility <- with(data, 3.5 - TransactionCost * ifelse(Strategy == "EW", 0.2, 0.15))
data
}
# Simulate Sharpe Ratios and Utility vs Rebalancing Periods
simulate_rebalancing_periods <- function(strategies, rebalancing_periods) {
data <- expand.grid(Strategy = strategies, RebalancingPeriod = rebalancing_periods)
data$SharpeRatio <- with(data, 3.5 - log(RebalancingPeriod) * ifelse(Strategy == "EW", 0.1, 0.05))
data$Utility <- with(data, 3.5 - log(RebalancingPeriod) * ifelse(Strategy == "EW", 0.07, 0.03))
data
}
# Generate data for transaction costs
tc_data <- simulate_transaction_costs(strategies, transaction_costs)
# Generate data for rebalancing periods
rp_data <- simulate_rebalancing_periods(strategies, rebalancing_periods)
# Plot: Sharpe Ratio vs Transaction Costs
p1 <- ggplot(tc_data, aes(x = TransactionCost, y = SharpeRatio, color = Strategy, shape = Strategy)) +
geom_line() +
geom_point() +
labs(
title = "Graph A: Sharpe Ratio vs Transaction Costs",
x = "Transaction Cost",
y = "Sharpe Ratio"
) +
theme_minimal()
# Plot: Utility vs Transaction Costs
p2 <- ggplot(tc_data, aes(x = TransactionCost, y = Utility, color = Strategy, shape = Strategy)) +
geom_line() +
geom_point() +
labs(
title = "Graph B: Utility vs Transaction Costs",
x = "Transaction Cost",
y = "Utility"
) +
theme_minimal()
# Plot: Sharpe Ratio vs Rebalancing Period
p3 <- ggplot(rp_data, aes(x = RebalancingPeriod, y = SharpeRatio, color = Strategy, shape = Strategy)) +
geom_line() +
geom_point() +
labs(
title = "Graph A: Sharpe Ratio vs Rebalancing Period",
x = "Rebalancing Period",
y = "Sharpe Ratio"
) +
theme_minimal()
# Plot: Utility vs Rebalancing Period
p4 <- ggplot(rp_data, aes(x = RebalancingPeriod, y = Utility, color = Strategy, shape = Strategy)) +
geom_line() +
geom_point() +
labs(
title = "Graph B: Utility vs Rebalancing Period",
x = "Rebalancing Period",
y = "Utility"
) +
theme_minimal()
# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)
# Assuming min_var and max_sr are data frames with columns "Risk" and "Return"
p <- portfolio_values %>%
ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
geom_point() +
theme_classic() +
scale_y_continuous(labels = scales::percent) +
scale_x_continuous(labels = scales::percent) +
labs(
x = 'Annualized Risk',
y = 'Annualized Returns',
title = "Portfolio Optimization & Efficient Frontier"
) +
# Highlight Minimum Variance Portfolio
geom_point(aes(x = Risk, y = Return), data = min_var, color = 'red', size = 3) +
# Highlight Tangency Portfolio
geom_point(aes(x = Risk, y = Return), data = max_sr, color = 'red', size = 3) +
# Annotate Minimum Variance Portfolio
annotate('text', x = min_var$Risk + 0.01, y = min_var$Return, label = "Minimum Variance Portfolio", hjust = 0) +
# Annotate Tangency Portfolio
annotate('text', x = max_sr$Risk - 0.01, y = max_sr$Return, label = "Tangency Portfolio", hjust = 1) +
# Arrows for annotations
annotate(
geom = 'segment',
x = min_var$Risk + 0.01, xend = min_var$Risk,
y = min_var$Return + 0.01, yend = min_var$Return,
color = 'red', arrow = arrow(type = "open")
) +
annotate(
geom = 'segment',
x = max_sr$Risk - 0.01, xend = max_sr$Risk,
y = max_sr$Return - 0.01, yend = max_sr$Return,
color = 'red', arrow = arrow(type = "open")
)
# Convert to an interactive plot
ggplotly(p)
savehistory("C:/Users/User/Desktop/Data Analysis (Cryptocurrency)/R code History.Rhistory")
