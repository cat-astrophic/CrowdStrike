# CrowdStrike paper code

# Installing missing libraries

list.of.packages <- c('modelsummary', 'strucchange', 'stargazer', 'sandwich', 'quantmod',
                      'tseries', 'ggplot2', 'ggrepel', 'lmtest', 'Synth', 'dplyr', 'MSwM')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]

if (length(new.packages) > 0) {
  
  install.packages(new.packages)
  
}

# Loading libraries

library(modelsummary)
library(strucchange)
library(stargazer)
library(sandwich)
library(quantmod)
library(tseries)
library(ggplot2)
library(ggrepel)
library(lmtest)
library(Synth)
library(dplyr)
library(MSwM)

# Get data with quantmod

#symbols.to.get <- c('CRWD', 'AAPL', 'NVDA', 'MSFT', 'GOOG', 'AMZN', 'META', 'TSM', 'AVGO', 'TSLA',
#                    'TCEHY', 'ORCL', '005930.KS', 'ASML', 'NFLX', 'SAP', 'CRM', 'ADBE', 'AMD', 'CSCO',
#                    'BABA', 'QCOM', 'TXN', 'IBM', 'INTU', 'NOW', 'AMAT', 'UBER', 'SU.PA', 'ARM',
#                    'PDD', 'BKNG', '6861.T', 'SONY', 'ADI', 'PANW', 'ADP', 'KLAC', 'ANET', 'MU',
#                    'LRCX', 'MELI', 'FI', 'SHOP', '000660.KS', 'INTC', '8035.T', '3690.HK', 'SNPS', 'DELL',
#                    '2317.TW', 'EQIX', 'ABNB', 'CDNS', 'PYPL', 'WDAY', 'PLTR', 'SPOT', 'CSU.TO', '7974.T',
#                    'NXPI', 'XIACF', '2454.TW', 'MRVL', 'ROP', 'FTNT', 'ADSK', 'NTES', 'DASH', 'TTD',
#                    'DSY.PA', 'COIN', 'SE', 'IFX.DE', 'TEL', 'MPWR', 'ADYEN.AS', 'IQV', 'FIS', 'MCHP',
#                    'TEAM', 'JD', 'FICO', 'SQ', 'CPNG', 'WKL.AS', 'EA', 'DDOG', 'SNOW', 'DELTA.BK',
#                    '6981.T', 'HPQ', 'GRMN', '7751.T', '2382.TW', '2308.TW', 'ON', 'ASM.AS', 'VEEV', 'SMCI')

symbols.to.get <- c('CRWD', 'AAPL', 'NVDA', 'MSFT', 'GOOG', 'AMZN', 'META', 'TSM', 'AVGO', 'TSLA',
                    'TCEHY', 'ORCL', 'ASML', 'NFLX', 'SAP', 'CRM', 'ADBE', 'AMD', 'CSCO', 'BABA',
                    'QCOM', 'TXN', 'IBM', 'INTU', 'NOW', 'AMAT', 'UBER', 'ARM', 'PDD', 'BKNG',
                    'SONY', 'ADI', 'PANW', 'ADP', 'KLAC', 'ANET', 'MU', 'MELI', 'FI',
                    'SHOP', 'INTC', 'SNPS', 'DELL', 'EQIX', 'ABNB', 'CDNS', 'PYPL', 'WDAY',
                    'PLTR', 'SPOT', 'NXPI', 'XIACF', 'MRVL', 'ROP', 'FTNT', 'ADSK', 'NTES',
                    'DASH', 'TTD', 'COIN', 'SE', 'TEL', 'MPWR', 'IQV', 'FIS', 'MCHP', 'TEAM', 'JD',
                    'FICO', 'SQ', 'CPNG', 'EA', 'DDOG', 'SNOW', 'HPQ', 'GRMN', 'ON', 'VEEV', 'SMCI')

getSymbols(symbols.to.get, src = 'yahoo', from = as.Date('2024-01-01'), to = as.Date('2024-11-30'))

# Create a single dataframe

indx <- rep(1:230, 79)

unit <- c()
symbol <- c()

for (s in symbols.to.get) {
  
  unit <- c(unit, rep(which(symbols.to.get == s), 230))
  symbol <- c(symbol, rep(s, 230))
  
}

price <- c(coredata(CRWD[2:231,4]), coredata(AAPL[2:231,4]), coredata(NVDA[2:231,4]), coredata(MSFT[2:231,4]), coredata(GOOG[2:231,4]),
           coredata(AMZN[2:231,4]), coredata(META[2:231,4]), coredata(TSM[2:231,4]), coredata(AVGO[2:231,4]), coredata(TSLA[2:231,4]),
           coredata(TCEHY[2:231,4]), coredata(ORCL[2:231,4]), coredata(ASML[2:231,4]), coredata(NFLX[2:231,4]),
           coredata(SAP[2:231,4]), coredata(CRM[2:231,4]), coredata(ADBE[2:231,4]), coredata(AMD[2:231,4]), coredata(CSCO[2:231,4]),
           coredata(BABA[2:231,4]), coredata(QCOM[2:231,4]), coredata(TXN[2:231,4]), coredata(IBM[2:231,4]), coredata(INTU[2:231,4]),
           coredata(NOW[2:231,4]), coredata(AMAT[2:231,4]), coredata(UBER[2:231,4]), coredata(ARM[2:231,4]),
           coredata(PDD[2:231,4]), coredata(BKNG[2:231,4]), coredata(SONY[2:231,4]), coredata(ADI[2:231,4]), coredata(PANW[2:231,4]),
           coredata(ADP[2:231,4]), coredata(KLAC[2:231,4]), coredata(ANET[2:231,4]), coredata(MU[2:231,4]),
           coredata(MELI[2:231,4]), coredata(FI[2:231,4]), coredata(SHOP[2:231,4]), coredata(INTC[2:231,4]), coredata(SNPS[2:231,4]),
           coredata(DELL[2:231,4]), coredata(EQIX[2:231,4]), coredata(ABNB[2:231,4]), coredata(CDNS[2:231,4]), coredata(PYPL[2:231,4]),
           coredata(WDAY[2:231,4]), coredata(PLTR[2:231,4]), coredata(SPOT[2:231,4]), coredata(NXPI[2:231,4]),
           coredata(XIACF[2:231,4]), coredata(MRVL[2:231,4]), coredata(ROP[2:231,4]), coredata(FTNT[2:231,4]), coredata(ADSK[2:231,4]),
           coredata(NTES[2:231,4]), coredata(DASH[2:231,4]), coredata(TTD[2:231,4]), coredata(COIN[2:231,4]),
           coredata(SE[2:231,4]), coredata(TEL[2:231,4]), coredata(MPWR[2:231,4]),
           coredata(IQV[2:231,4]), coredata(FIS[2:231,4]), coredata(MCHP[2:231,4]), coredata(TEAM[2:231,4]), coredata(JD[2:231,4]),
           coredata(FICO[2:231,4]), coredata(SQ[2:231,4]), coredata(CPNG[2:231,4]), coredata(EA[2:231,4]),
           coredata(DDOG[2:231,4]), coredata(SNOW[2:231,4]), coredata(HPQ[2:231,4]), coredata(GRMN[2:231,4]),
           coredata(ON[2:231,4]), coredata(VEEV[2:231,4]), coredata(SMCI[2:231,4]))

lag.price <- c(coredata(CRWD[1:230,4]), coredata(AAPL[1:230,4]), coredata(NVDA[1:230,4]), coredata(MSFT[1:230,4]), coredata(GOOG[1:230,4]),
               coredata(AMZN[1:230,4]), coredata(META[1:230,4]), coredata(TSM[1:230,4]), coredata(AVGO[1:230,4]), coredata(TSLA[1:230,4]),
               coredata(TCEHY[1:230,4]), coredata(ORCL[1:230,4]), coredata(ASML[1:230,4]), coredata(NFLX[1:230,4]),
               coredata(SAP[1:230,4]), coredata(CRM[1:230,4]), coredata(ADBE[1:230,4]), coredata(AMD[1:230,4]), coredata(CSCO[1:230,4]),
               coredata(BABA[1:230,4]), coredata(QCOM[1:230,4]), coredata(TXN[1:230,4]), coredata(IBM[1:230,4]), coredata(INTU[1:230,4]),
               coredata(NOW[1:230,4]), coredata(AMAT[1:230,4]), coredata(UBER[1:230,4]), coredata(ARM[1:230,4]),
               coredata(PDD[1:230,4]), coredata(BKNG[1:230,4]), coredata(SONY[1:230,4]), coredata(ADI[1:230,4]), coredata(PANW[1:230,4]),
               coredata(ADP[1:230,4]), coredata(KLAC[1:230,4]), coredata(ANET[1:230,4]), coredata(MU[1:230,4]),
               coredata(MELI[1:230,4]), coredata(FI[1:230,4]), coredata(SHOP[1:230,4]), coredata(INTC[1:230,4]), coredata(SNPS[1:230,4]),
               coredata(DELL[1:230,4]), coredata(EQIX[1:230,4]), coredata(ABNB[1:230,4]), coredata(CDNS[1:230,4]), coredata(PYPL[1:230,4]),
               coredata(WDAY[1:230,4]), coredata(PLTR[1:230,4]), coredata(SPOT[1:230,4]), coredata(NXPI[1:230,4]),
               coredata(XIACF[1:230,4]), coredata(MRVL[1:230,4]), coredata(ROP[1:230,4]), coredata(FTNT[1:230,4]), coredata(ADSK[1:230,4]),
               coredata(NTES[1:230,4]), coredata(DASH[1:230,4]), coredata(TTD[1:230,4]), coredata(COIN[1:230,4]),
               coredata(SE[1:230,4]), coredata(TEL[1:230,4]), coredata(MPWR[1:230,4]),
               coredata(IQV[1:230,4]), coredata(FIS[1:230,4]), coredata(MCHP[1:230,4]), coredata(TEAM[1:230,4]), coredata(JD[1:230,4]),
               coredata(FICO[1:230,4]), coredata(SQ[1:230,4]), coredata(CPNG[1:230,4]), coredata(EA[1:230,4]),
               coredata(DDOG[1:230,4]), coredata(SNOW[1:230,4]), coredata(HPQ[1:230,4]), coredata(GRMN[1:230,4]),
               coredata(ON[1:230,4]), coredata(VEEV[1:230,4]), coredata(SMCI[1:230,4]))

volume <- c(coredata(CRWD[2:231,5]), coredata(AAPL[2:231,5]), coredata(NVDA[2:231,5]), coredata(MSFT[2:231,5]), coredata(GOOG[2:231,5]),
            coredata(AMZN[2:231,5]), coredata(META[2:231,5]), coredata(TSM[2:231,5]), coredata(AVGO[2:231,5]), coredata(TSLA[2:231,5]),
            coredata(TCEHY[2:231,5]), coredata(ORCL[2:231,5]), coredata(ASML[2:231,5]), coredata(NFLX[2:231,5]),
            coredata(SAP[2:231,5]), coredata(CRM[2:231,5]), coredata(ADBE[2:231,5]), coredata(AMD[2:231,5]), coredata(CSCO[2:231,5]),
            coredata(BABA[2:231,5]), coredata(QCOM[2:231,5]), coredata(TXN[2:231,5]), coredata(IBM[2:231,5]), coredata(INTU[2:231,5]),
            coredata(NOW[2:231,5]), coredata(AMAT[2:231,5]), coredata(UBER[2:231,5]), coredata(ARM[2:231,5]),
            coredata(PDD[2:231,5]), coredata(BKNG[2:231,5]), coredata(SONY[2:231,5]), coredata(ADI[2:231,5]), coredata(PANW[2:231,5]),
            coredata(ADP[2:231,5]), coredata(KLAC[2:231,5]), coredata(ANET[2:231,5]), coredata(MU[2:231,5]),
            coredata(MELI[2:231,5]), coredata(FI[2:231,5]), coredata(SHOP[2:231,5]), coredata(INTC[2:231,5]), coredata(SNPS[2:231,5]),
            coredata(DELL[2:231,5]), coredata(EQIX[2:231,5]), coredata(ABNB[2:231,5]), coredata(CDNS[2:231,5]), coredata(PYPL[2:231,5]),
            coredata(WDAY[2:231,5]), coredata(PLTR[2:231,5]), coredata(SPOT[2:231,5]), coredata(NXPI[2:231,5]),
            coredata(XIACF[2:231,5]), coredata(MRVL[2:231,5]), coredata(ROP[2:231,5]), coredata(FTNT[2:231,5]), coredata(ADSK[2:231,5]),
            coredata(NTES[2:231,5]), coredata(DASH[2:231,5]), coredata(TTD[2:231,5]), coredata(COIN[2:231,5]),
            coredata(SE[2:231,5]), coredata(TEL[2:231,5]), coredata(MPWR[2:231,5]), 
            coredata(IQV[2:231,5]), coredata(FIS[2:231,5]), coredata(MCHP[2:231,5]), coredata(TEAM[2:231,5]), coredata(JD[2:231,5]),
            coredata(FICO[2:231,5]), coredata(SQ[2:231,5]), coredata(CPNG[2:231,5]), coredata(EA[2:231,5]),
            coredata(DDOG[2:231,5]), coredata(SNOW[2:231,5]), coredata(HPQ[2:231,5]), coredata(GRMN[2:231,5]),
            coredata(ON[2:231,5]), coredata(VEEV[2:231,5]), coredata(SMCI[2:231,5]))

lag.volume <- c(coredata(CRWD[1:230,5]), coredata(AAPL[1:230,5]), coredata(NVDA[1:230,5]), coredata(MSFT[1:230,5]), coredata(GOOG[1:230,5]),
                coredata(AMZN[1:230,5]), coredata(META[1:230,5]), coredata(TSM[1:230,5]), coredata(AVGO[1:230,5]), coredata(TSLA[1:230,5]),
                coredata(TCEHY[1:230,5]), coredata(ORCL[1:230,5]), coredata(ASML[1:230,5]), coredata(NFLX[1:230,5]),
                coredata(SAP[1:230,5]), coredata(CRM[1:230,5]), coredata(ADBE[1:230,5]), coredata(AMD[1:230,5]), coredata(CSCO[1:230,5]),
                coredata(BABA[1:230,5]), coredata(QCOM[1:230,5]), coredata(TXN[1:230,5]), coredata(IBM[1:230,5]), coredata(INTU[1:230,5]),
                coredata(NOW[1:230,5]), coredata(AMAT[1:230,5]), coredata(UBER[1:230,5]), coredata(ARM[1:230,5]),
                coredata(PDD[1:230,5]), coredata(BKNG[1:230,5]), coredata(SONY[1:230,5]), coredata(ADI[1:230,5]), coredata(PANW[1:230,5]),
                coredata(ADP[1:230,5]), coredata(KLAC[1:230,5]), coredata(ANET[1:230,5]), coredata(MU[1:230,5]),
                coredata(MELI[1:230,5]), coredata(FI[1:230,5]), coredata(SHOP[1:230,5]), coredata(INTC[1:230,5]), coredata(SNPS[1:230,5]),
                coredata(DELL[1:230,5]), coredata(EQIX[1:230,5]), coredata(ABNB[1:230,5]), coredata(CDNS[1:230,5]), coredata(PYPL[1:230,5]),
                coredata(WDAY[1:230,5]), coredata(PLTR[1:230,5]), coredata(SPOT[1:230,5]), coredata(NXPI[1:230,5]),
                coredata(XIACF[1:230,5]), coredata(MRVL[1:230,5]), coredata(ROP[1:230,5]), coredata(FTNT[1:230,5]), coredata(ADSK[1:230,5]),
                coredata(NTES[1:230,5]), coredata(DASH[1:230,5]), coredata(TTD[1:230,5]), coredata(COIN[1:230,5]),
                coredata(SE[1:230,5]), coredata(TEL[1:230,5]), coredata(MPWR[1:230,5]), 
                coredata(IQV[1:230,5]), coredata(FIS[1:230,5]), coredata(MCHP[1:230,5]), coredata(TEAM[1:230,5]), coredata(JD[1:230,5]),
                coredata(FICO[1:230,5]), coredata(SQ[1:230,5]), coredata(CPNG[1:230,5]), coredata(EA[1:230,5]),
                coredata(DDOG[1:230,5]), coredata(SNOW[1:230,5]), coredata(HPQ[1:230,5]), coredata(GRMN[1:230,5]),
                coredata(ON[1:230,5]), coredata(VEEV[1:230,5]), coredata(SMCI[1:230,5]))

df <- as.data.frame(cbind(indx, unit, price, lag.price, volume, lag.volume))
df$symbol <- symbol
df$Rate <- (df$price - df$lag.price) / df$lag.price
df$vRate <- (df$volume - df$lag.volume) / df$lag.volume

cr <- c()
crv <- c()

for (i in 1:nrow(df)) {
  
  cr <- c(cr, (df$price[i] - df$lag.price[(floor((i-1)/230)*230) + 1]) / df$lag.price[(floor((i-1)/230)*230) + 1])
  crv <- c(crv, (df$volume[i] - df$lag.volume[(floor((i-1)/230)*230) + 1]) / df$lag.volume[(floor((i-1)/230)*230) + 1])
  
}

df$Cum_Rate <- cr
df$Cum_vRate <- crv

# Scaling / transofrming data

df$volume <- log(df$volume)
df$lag.volume <- log(df$lag.volume)

df$Rate <- 100 * df$Rate
df$vRate <- 100 * df$vRate
df$Cum_Rate <- 100 * df$Cum_Rate
df$Cum_vRate <- 100 * df$Cum_vRate

# Creating two nice data visualizations for CRWD

chartSeries(CRWD)

# Data prep for running Synth

sob <- dataprep(foo = df, predictors = c('Rate', 'vRate', 'Cum_Rate', 'Cum_vRate', 'lag.price', 'volume', 'lag.volume'), predictors.op = c('mean'),
                dependent = c('price'), unit.variable = c('unit'), time.variable = c('indx'),
                treatment.identifier = 1, controls.identifier = c(2:79), time.predictors.prior = c(1:136),
                time.optimize.ssr = c(1:136), unit.names.variable = 'symbol', time.plot = c(1:230))

# Running Synth

mod <- synth(data.prep.obj = sob, optimxmethod = 'All')

path.plot(synth.res = mod, dataprep.res = sob, Ylab = 'Stock Price ($USD)', Main = 'CrowdStrike Synthetic Control Model Results',
          Xlab = 'Date', Legend = c('CrowdStrike', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 136)

# RMSPE function

rmspe.fx <- function(dp.obj, res.obj) {
  
  pred.vals <- dp.obj$Y0plot %*% res.obj$solution.w
  
  errors <- dp.obj$Y1plot - pred.vals
  
  pre.err <- sqrt(sum(errors[1:136]^2) / 136)
  
  post.err.160 <- sqrt(sum(errors[137:160]^2) / 24)
  post.err.168 <- sqrt(sum(errors[137:168]^2) / 32)
  post.err.211 <- sqrt(sum(errors[137:211]^2) / 75)
  post.err.230 <- sqrt(sum(errors[137:230]^2) / 94)
  
  res.vec <- c(pre.err, post.err.160, post.err.168, post.err.211, post.err.230)
  
  return(res.vec)
  
}

# Quantifying the pre-treatment fit

main.rm <- rmspe.fx(sob, mod)

crwd.pre.fit <- main.rm[1] / max(df[2:137,]$price)

crwd.pre.fit

# Placebo testing - "in place"

rmspe.pre <- c(rmspe.fx(sob, mod)[1])
rmspe.post.160 <- c(rmspe.fx(sob,mod)[2])
rmspe.post.168 <- c(rmspe.fx(sob,mod)[3])
rmspe.post.211 <- c(rmspe.fx(sob,mod)[4])
rmspe.post.230 <- c(rmspe.fx(sob,mod)[5])

store <- matrix(NA, 230, 78)
crow <- sob$Y1plot - (sob$Y0plot %*% mod$solution.w)
store <- cbind(crow, store)
colnames(store) <- symbols.to.get

for (j in 2:79) {
  
  print(j)
  
  cons <- which(1:79 != j)
  
  sob <- dataprep(foo = df, predictors = c('Rate', 'vRate', 'Cum_Rate', 'Cum_vRate', 'lag.price', 'volume', 'lag.volume'), predictors.op = c('mean'),
                  dependent = c('price'), unit.variable = c('unit'), time.variable = c('indx'),
                  treatment.identifier = j, controls.identifier = cons, time.predictors.prior = c(1:136),
                  time.optimize.ssr = c(1:136), unit.names.variable = 'symbol', time.plot = c(1:230))
  
  iter <- synth(data.prep.obj = sob, optimxmethod = 'All')
  
  store[,j] <- sob$Y1plot - (sob$Y0plot %*% iter$solution.w)
  
  rmspe.pre <- c(rmspe.pre, rmspe.fx(sob, iter)[1])
  rmspe.post.160 <- c(rmspe.post.160, rmspe.fx(sob, iter)[2])
  rmspe.post.168 <- c(rmspe.post.168, rmspe.fx(sob, iter)[3])
  rmspe.post.211 <- c(rmspe.post.211, rmspe.fx(sob, iter)[4])
  rmspe.post.230 <- c(rmspe.post.230, rmspe.fx(sob, iter)[5])
  
}

# Computing p-values

vals.160 <- rmspe.post.160 / rmspe.pre
vals.168 <- rmspe.post.168 / rmspe.pre
vals.211 <- rmspe.post.211 / rmspe.pre
vals.230 <- rmspe.post.230 / rmspe.pre

sorted.160 <- sort(vals.160, decreasing = TRUE)
sorted.168 <- sort(vals.168, decreasing = TRUE)
sorted.211 <- sort(vals.211, decreasing = TRUE)
sorted.230 <- sort(vals.230, decreasing = TRUE)

p.160 <- which(sorted.160 == vals.160[1]) / 79
p.168 <- which(sorted.168 == vals.168[1]) / 79
p.211 <- which(sorted.211 == vals.211[1]) / 79
p.230 <- which(sorted.230 == vals.230[1]) / 79

# Placebo figure

store2 <- store

for (col in 1:79) {
  
  for (row in 1:230) {
    
    tmp <- df %>% filter(unit == col)
    base <- tmp$price[row]
    store2[row,col] <- store2[row,col] / base
    
  }
  
}

keeps <- which(rmspe.pre < rmspe.pre[1])

plot(1:211, store2[1:211,1], ylim = c(-1,1), xlim = c(0, 212), type = 'l', lwd = 2, col = "black", xaxs = 'i', yaxs = 'i', xlab = 'Days', ylab = 'Relative Error')
for (i in keeps) {lines(1:211, store2[1:211,i], col = "gray")}
lines(1:211, store2[1:211,1], lwd = 2, col = "black")
abline(v = 136)
legend('topright', legend = c('CrowdStrike', 'Controls'), lty = c(1,1), col = c('black', 'gray'), lwd = c(2,1), cex = .8)
axis(1, at = seq(0, 200, by = 50))
title('Placebo Test Results')

# Histogram of rmspe ratios

histdf <- as.data.frame(cbind(vals.160, vals.168, vals.211, vals.230))
colnames(histdf) <- c('H1', 'H2', 'H3', 'H4')
histdf$Legend <- c('red4', rep('orange', 78))

ggplot(histdf, aes(x = H3, fill = Legend)) +
  theme_bw() +
  ggtitle('Histogram of the Post RMSPE to Pre RMSPE Ratios') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Frequency') +
  xlab('Ratio') +
  geom_histogram(color = 'black') +
  scale_fill_identity(guide = 'legend', labels = c('Controls', 'CrowdStrike'))

# Nice Synth figure with ggplot

syndf <- as.data.frame(cbind(1:211, sob$Y1plot[1:211], sob$Y0plot[1:211,] %*% mod$solution.w))
colnames(syndf) <- c('Days', 'CrowdStrike', 'Synthetic Control')

ggplot(syndf, aes(x = Days, y = CrowdStrike)) +
  theme_bw() +
  ggtitle('CrowdStrike Synthetic Control Model Results') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Stock Price ($USD)') +
  xlab('Date') +
  ylim(c(0,500)) +
  geom_vline(xintercept = 136) +
  geom_line(aes(y = `Synthetic Control`, col = 'Synthetic Control'), size = 1, alpha = 1, linetype = 'dotted') +
  geom_line(aes(y = CrowdStrike, col = 'CrowdStrike'), size = 1, alpha = 1) +
  scale_color_manual(name = 'Legend', values = c('Synthetic Control' = 'black', 'CrowdStrike' = 'Black'))

# Structural break time series figure

ftest <- Fstats(syndf$CrowdStrike ~ 1)

ggplot(data = syndf, aes(x = Days, y = CrowdStrike)) +
  theme_bw() +
  ggtitle('CrowdStrike Stock Price Time Series') +
  ylab('Stock Price ($USD') +
  geom_line(aes(x = Days, y = CrowdStrike, group = 1), size = 1, alpha = 1) +
  geom_vline(xintercept = ftest$breakpoint-1) +
  theme(plot.title = element_text(hjust = 0.5))

# Figure for CrowdStrike constituents from the SCM

sr.vals <- rev(sort(mod$solution.w))
sr.ids <- rev(order(mod$solution.w))
sr.labs <- unique(df$symbol)[sr.ids]
sr.df <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df) <- c('Value', 'ID', 'Label')
sr.df$ID <- as.integer(sr.df$ID)
sr.df$Value <- as.numeric(sr.df$Value)
sr.df$Rank <- 1:length(sr.df$ID)

ggplot(data = sr.df[1:10,], aes(x = Rank, y = Value)) +
  theme_bw() +
  ggtitle('Synthetic CrowdStrike Top 10 Constituents') +
  ylab('Contribution to Synthetic CRWD') +
  xlab('Contribution Rank') +
  geom_point(size = 2, pch = 19) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none') +
  geom_text_repel(aes(label = Label), nudge_y = 0.005) +
  ylim(c(0,0.2)) +
  scale_x_continuous(breaks = seq(1, 10, by = 1))

# Creating a summary stats figure

dfx <- df[,c(3,4,5,6,8,9,10,11)]

colnames(dfx) <- c('Price (USD)', 'Lagged Price (USD)', 'Trade Volume', 'Lagged Trade Volume', 'Daily Rate of Return (%)',
                   'Daily Change in Volume (%)', 'Cumulative Rate of Return (%)', 'Cumulative Change in Volume (%)')

datasummary_skim(dfx, fmt = '%.2f')

# Markov switching model to prep for the persistence analysis

gaps <- as.data.frame(cbind(as.data.frame(store)$CRWD[137:211], as.data.frame(store)$CRWD[136:210]))
colnames(gaps) <- c('gap', 'lag')

mark <- lm(gap ~ 1, data = gaps)

msm <- msmFit(mark, k = 2, sw = c(FALSE, TRUE))

plotProb(msm, which = 1)

# Persistence analysis figure

gapss <- rbind(c(0, 0), gaps)

ggplot(data = gapss, aes(y = gap)) +
  theme_bw() +
  ggtitle('Difference Between the Synthetic Control and Actual Price') +
  ylab('Difference ($USD)') +
  xlab('Days') +
  ylim(c(-150, 0)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(aes(x = 0:75, y = gap), size = 1, alpha = 1) +
  geom_vline(xintercept = 2)

# Persistence analysis starting with day 3 as the start of the post-decrease regime

adf.test(gaps$gap[3:nrow(gaps)])

pp.test(gaps$gap[3:nrow(gaps)])

ppp <- c()

for (i in 3:9) {
  
  pp <- pp.test(gaps$gap[i:nrow(gaps)])
  ppp <- c(ppp, pp$p.value)
  
}

mod <- lm(gap ~ -1 + lag, data = gaps[3:nrow(gaps),])

stargazer(mod, type = 'text')

##### NOTES #####

# these are the top 100 tech companies by market cap on 8/27/2024 when data was obtained

# the twelve symbols that start with numbers or contain decimals were omitted due to different time series lengths
# (different holidays off which could affect prices; missing observations restricting sample; changes in price over different time periods)

# https://companiesmarketcap.com/tech/largest-tech-companies-by-market-cap/

