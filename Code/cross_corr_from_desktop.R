
library(tidyverse)
library(here)

#stats:::ccf
## provind the validity of the ccf function to myself

## some set of values a (this is 'MOCI')
a <- c(1, 4, 1, 0, 1, 0, 4, 5, 4, 5, 6, 5, 8)

## values that follow the same pattern two years (lags) later using a random starting value. This is 'cpue'
b <- c(4, 3, 4, 7, 4, 3, 4, 3, 7, 8, 7, 8, 9)
ab <- data.frame(a,b)


ab_ccf <- ccf(ab$a, ab$b)
lag <- ab_ccf$lag
value <- ab_ccf$acf
## yes, the highest correlation is for the -2 lag. BOOM, just like blue rockfish.
ccf_df <- data.frame(lag, value)

ab_cor <- cor(ab$a, ab$b)

ab_cor2 <- ab_cor %>%
  lag(a, 1)

for(i in 0:8){
  lag_df <- juvenile %>%
    mutate(central_ca = dplyr::lag(central_ca, n = i)) %>% drop_na()
  cor <- (sum((lag_df$central_ca - mean(lag_df$central_ca, na.rm = T))* 
                (lag_df$cpue_site - mean(lag_df$cpue_site, na.rm = T))))/
    ((sqrt(sum((lag_df$central_ca - mean(lag_df$central_ca, na.rm = T))^2)))*
       (sqrt(sum((lag_df$cpue_site - mean(lag_df$cpue_site, na.rm = T))^2))))
  for(m in 1:length(cor)){
    juv_corr_hand <- bind_rows(juv_corr_hand, c(area = j, site = k, season = l, 
                                                lag = -i, correlation = cor[m]))
  }}




## the order does matter. CPUE has to come as the second argument.
ba_ccf <- ccf(ab$b, ab$a)
lag2 <- ba_ccf$lag
value2 <- ba_ccf$acf
ccf_df2 <- data.frame(lag2, value2)


## now with an NA value
b_na <- c(4, 3, 4, 7, 4, 3, 4, 3, NA, 8, 7, 8, 9)
ab_na <- data.frame(a, b_na)


## na.action = na.pass
ab_ccf_na <- ccf(ab_na$a, ab_na$b_na, na.action = na.pass)
ccf_df3 <- data.frame(ab_ccf_na$lag, ab_ccf_na$acf)

## na.action = na.exclude
ab_ccf_na2 <- ccf(ab_na$a, ab_na$b_na, na.action = na.exclude)
ccf_df4 <- data.frame(ab_ccf_na2$lag, ab_ccf_na2$acf)

## NA by hand for lag 0
#mean(ab_na$a)
#mean(ab_na$b_na, na.rm = T)
n1 <- ab_na$a - mean(ab_na$a)
n2 <- ab_na$b_na - mean(ab_na$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sum(d1)
d2b <- sum(d2, na.rm = T)
d_tot <- d1b*d2b
d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## 0.5396802

## NA by hand for lag -1
lag1 <- ab_na %>%
  mutate(a = lag(a)) %>% drop_na()
#lag1 = lag1[-1,]

n1 <- lag1$a - mean(lag1$a, na.rm = T)
n2 <- lag1$b_na - mean(lag1$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sum(d1, na.rm = T)
d2b <- sum(d2, na.rm = T)
d_tot <- d1b*d2b
d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## 0.5724554, this is different if I take out the line of NA vs leaving it in.

## Na by hand for lag -2
lag2 <- lag1 %>%
  mutate(a = lag(a)) %>% drop_na()

n1 <- lag2$a - mean(lag2$a, na.rm = T)
n2 <- lag2$b_na - mean(lag2$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sqrt(sum(d1, na.rm = T))
d2b <- sqrt(sum(d2, na.rm = T))
d_tot2 <- d1b*d2b

ccf_ben <- ccf(lag2$a, lag2$b_na, na.action = na.exclude)
ben_df <- data.frame(lag = ccf_ben$lag, corr = ccf_ben$acf)

ccf_ben2 <- ccf(ab_na$a, ab_na$b_na, na.action = na.exclude)
ben_df2 <- data.frame(lag = ccf_ben2$lag, corr = ccf_ben2$acf)

#d_tot <- d1b*d2b
#d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## 0.898775. what the fuck. Why is it so good by hand? Something is not right

## Na by hand for lag -3
lag3 <- lag2 %>%
  mutate(a = lag(a))
lag3 <- lag3 %>% drop_na()

n1 <- lag3$a - mean(lag3$a, na.rm = T)
n2 <- lag3$b_na - mean(lag3$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sum(d1, na.rm = T)
d2b <- sum(d2, na.rm = T)
d_tot <- d1b*d2b
d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## 0.5816238


## Na by hand for lag -4
lag4 <- lag3 %>%
  mutate(a = lag(a))

n1 <- lag4$a - mean(lag4$a, na.rm = T)
n2 <- lag4$b_na - mean(lag4$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sum(d1, na.rm = T)
d2b <- sum(d2, na.rm = T)
d_tot <- d1b*d2b
d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## 0.3608541

## Na by hand for lag -5
lag5 <- lag4 %>%
  mutate(a = lag(a))

n1 <- lag5$a - mean(lag5$a, na.rm = T)
n2 <- lag5$b_na - mean(lag5$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sum(d1, na.rm = T)
d2b <- sum(d2, na.rm = T)
d_tot <- d1b*d2b
d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## 0.3096261

## Na by hand for lag -6
lag6 <- lag5 %>%
  mutate(a = lag(a))

n1 <- lag6$a - mean(lag6$a, na.rm = T)
n2 <- lag6$b_na - mean(lag6$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sum(d1, na.rm = T)
d2b <- sum(d2, na.rm = T)
d_tot <- d1b*d2b
d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## -0.1683345

## Na by hand for lag -7
lag7 <- lag6 %>%
  mutate(a = lag(a))

n1 <- lag7$a - mean(lag7$a, na.rm = T)
n2 <- lag7$b_na - mean(lag7$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sum(d1, na.rm = T)
d2b <- sum(d2, na.rm = T)
d_tot <- d1b*d2b
d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## -0.2713114

## Na by hand for lag -8
lag8 <- lag7 %>%
  mutate(a = lag(a))

n1 <- lag8$a - mean(lag8$a, na.rm = T)
n2 <- lag8$b_na - mean(lag8$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sum(d1, na.rm = T)
d2b <- sum(d2, na.rm = T)
d_tot <- d1b*d2b
d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## 0.04671657



##acf.out$snames <- paste(acf.out$snames, collapse = " & ")


#### NA PASS ##### 

ccf_new_pass <- function (x, y, lag.max = NULL, type = c("correlation", 
                                         "covariance"), plot = TRUE, na.action = na.fail, ...) 
{
  type <- match.arg(type)
  if (is.matrix(x) || is.matrix(y)) 
    stop("univariate time series only")
  X <- ts.intersect(as.ts(x), as.ts(y)) ## binds time series which have a common frequency
  colnames(X) <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
  acf.out <- acf(X, lag.max = lag.max, plot = FALSE, type = type, 
                 na.action = na.pass) ## this na.action is what really controls it.
  lag <- c(rev(acf.out$lag[-1, 2, 1]), acf.out$lag[, 1, 2])
  y <- c(rev(acf.out$acf[-1, 2, 1]), acf.out$acf[, 1, 2])
  acf.out$acf <- array(y, dim = c(length(y), 1L, 1L)) ## stores value 'y'. the '1L' arguments make it just one column instead of two horizonal repeated
  acf.out$lag <- array(lag, dim = c(length(y), 1L, 1L))
  acf.out$snames <- paste(acf.out$snames, collapse = " & ") ##"Sup" ## controls what goes on top of the plot output
  if (plot) {
    plot(acf.out, ...)
    return(invisible(acf.out))
  }
  else return(acf.out)
}

try1 <- ccf_new_pass(ab_na$a, ab_na$b_na, lag.max = 7)
pass <- data.frame(corr = try1$acf, lag = try1$lag)


##### NA EXCLUDE #####


ccf_new_exclude <- function (x, y, lag.max = NULL, type = c("correlation", 
                                                         "covariance"), plot = TRUE, na.action = na.fail, ...) 
{
  type <- match.arg(type)
  if (is.matrix(x) || is.matrix(y)) 
    stop("univariate time series only")
  X <- ts.intersect(as.ts(x), as.ts(y)) ## binds time series which have a common frequency
  colnames(X) <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
  acf.out <- acf(X, lag.max = lag.max, plot = FALSE, type = type, 
                 na.action = na.exclude) ## this na.action is what really controls it.
  lag <- c(rev(acf.out$lag[-1, 2, 1]), acf.out$lag[, 1, 2])
  y <- c(rev(acf.out$acf[-1, 2, 1]), acf.out$acf[, 1, 2])
  acf.out$acf <- array(y, dim = c(length(y), 1L, 1L)) ## stores value 'y'. the '1L' arguments make it just one column instead of two horizonal repeated
  acf.out$lag <- array(lag, dim = c(length(y), 1L, 1L))
  acf.out$snames <- paste(acf.out$snames, collapse = " & ") ##"Sup" ## controls what goes on top of the plot output
  if (plot) {
    plot(acf.out, ...)
    return(invisible(acf.out))
  }
  else return(acf.out)
}

try2 <- ccf_new_exclude(ab_na$a, ab_na$b_na)
exclude <- data.frame(corr = try2$acf, lag = try2$lag)


#### GET RID OF NA #####

## the argument na.exclude does the same thing as not having an NA in the dataframe at all. 
## it removes that row/ignores it completely.

ab_no_na <- ab_na %>%
  drop_na()


try3 <- ccf(ab_no_na$a, ab_no_na$b_na)
no_na <- data.frame(corr = try3$acf, lag = try3$lag)

try4 <- ccf_new_pass(ab_no_na$a, ab_no_na$b_na)
no_na_2 <- data.frame(corr = try4$acf, lag = try4$lag)

try5 <- ccf_new_exclude(ab_no_na$a, ab_no_na$b_na)
no_na_3 <- data.frame(corr = try5$acf, lag = try5$lag)


ab_na
na.omit(ab_na)
na.exclude(ab_na)
na.pass(ab_na)

## create temporary lag dfs
## take out dfs
## run cor()
## store those back into a dataframe.
## this takes the place of ccf within the larger loop frame

df_corrs <- data.frame(lag = numeric(), correlation = numeric())

for(i in 0:8) {
  lag_df <- ab_na %>%
    mutate(a = dplyr::lag(a, n = i)) %>% drop_na()
  corr <- (sum((lag_df$a - mean(lag_df$a, na.rm = T))* 
                 (lag_df$b_na - mean(lag_df$b_na, na.rm = T))))/
    ((sqrt(sum((lag_df$a - mean(lag_df$a, na.rm = T))^2)))*
       (sqrt(sum((lag_df$b_na - mean(lag_df$b_na, na.rm = T))^2))))
  for(j in 1:length(corr)){
    df_corrs <- bind_rows(df_corrs, c(lag = -i, correlation = corr[j]))
  }
}


juv_moci <- read_csv("C:/Users/ejohn122/Documents/Thesis/R Thesis Stuff/thesis_proj_desktop/Data/2021-03-02_juv_moci.csv")
  
juv_corr <- data.frame(area = character(), site = character(), season = character(), lag = character(), correlation = character())  

area_list <- c("PB", "BL", "AN", "PL")
site_list <- c("MPA", "REF")
season_list <- c("JFM", "AMJ", "JAS", "OND")
  
for(j in area_list){
  for(k in site_list){
    for(l in season_list){
      juvenile <- juv_moci %>%
        filter(area == j, site == k, season == l)
      for(i in 0:8){
        lag_df <- juvenile %>%
          mutate(central_ca = dplyr::lag(central_ca, n = i)) %>% drop_na()
        cor <- (sum((lag_df$central_ca - mean(lag_df$central_ca, na.rm = T))* 
                      (lag_df$cpue_site - mean(lag_df$cpue_site, na.rm = T))))/
          ((sqrt(sum((lag_df$central_ca - mean(lag_df$central_ca, na.rm = T))^2)))*
             (sqrt(sum((lag_df$cpue_site - mean(lag_df$cpue_site, na.rm = T))^2))))
        for(m in 1:length(cor)){
          juv_corr <- bind_rows(juv_corr, c(area = j, site = k, season = l, 
                                            lag = -i, correlation = cor[m]))
        }
      }
    }
  }
}  
  


juv_vis <- juv_corr %>%
  mutate(correlation = as.numeric(correlation),
         lag = as.numeric(lag))
  
juv_vis$season <- factor(juv_vis$season, levels = c("JFM", "AMJ" ,"JAS", "OND"))  

new_corr_plot <- ggplot()+
  scale_color_manual(values = c("#999999", "#009999", "#666666", "#000000"))+
  scale_fill_manual(values = c("#999999", "#009999", "#666666", "#000000"))+
  geom_col(position = "dodge2", data = juv_vis, aes(x = lag, y = correlation, color = season, fill = season))+
  labs(title = "New Correlation - Juvenile Blue rockfish & MOCI", y = "correlation", x = "lag") +
  theme_bw()+
  theme(panel.grid = element_blank())+
  facet_grid(site~area)
  

ggsave("new_corr_plot_juv_blue_moci.png", plot = new_corr_plot, 
          path = "C:/Users/ejohn122/Documents/Thesis/R Thesis Stuff/thesis_proj_desktop", 
          dpi = 1000, height = 4, width = 6)
  
## NA by hand for lag 0
#mean(ab_na$a)
#mean(ab_na$b_na, na.rm = T)
n1 <- ab_na$a - mean(ab_na$a)
n2 <- ab_na$b_na - mean(ab_na$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sum(d1)
d2b <- sum(d2, na.rm = T)
d_tot <- d1b*d2b
d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## 0.5396802

## NA by hand for lag -1
lag1 <- ab_na %>%
  mutate(a = lag(a)) %>% drop_na()
#lag1 = lag1[-1,]

n1 <- lag1$a - mean(lag1$a, na.rm = T)
n2 <- lag1$b_na - mean(lag1$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sum(d1, na.rm = T)
d2b <- sum(d2, na.rm = T)
d_tot <- d1b*d2b
d_tot2 <- sqrt(d_tot)
hand_cor <-  n_tot/d_tot2 ## 0.5724554, this is different if I take out the line of NA vs leaving it in.

## Na by hand for lag -2
lag2 <- lag1 %>%
  mutate(a = lag(a)) %>% drop_na()

n1 <- lag2$a - mean(lag2$a, na.rm = T)
n2 <- lag2$b_na - mean(lag2$b_na, na.rm = T)
n_tot <- sum(n1*n2, na.rm = T)
d1 <- n1^2
d2 <- n2^2
d1b <- sqrt(sum(d1, na.rm = T))
d2b <- sqrt(sum(d2, na.rm = T))
d_tot2 <- d1b*d2b

lag_2 <- ab_na %>%
  mutate(a = dplyr::lag(a, n =3)) %>% drop_na()


(sum((lag1$a - mean(lag1$a, na.rm = T))* 
       (lag1$b_na - mean(lag1$b_na, na.rm = T))))/
  ((sqrt(sum((lag1$a - mean(lag1$a, na.rm = T))^2)))*
     (sqrt(sum((lag1$b_na - mean(lag1$b_na, na.rm = T))^2))))




ccf_ben <- ccf(lag2$a, lag2$b_na, na.action = na.exclude)
ben_df <- data.frame(lag = ccf_ben$lag, corr = ccf_ben$acf)

ccf_ben2 <- ccf(ab_na$a, ab_na$b_na, na.action = na.exclude)
ben_df2 <- data.frame(lag = ccf_ben2$lag, corr = ccf_ben2$acf)

## stats:::acf

function (x, lag.max = NULL, type = c("correlation", "covariance", 
                                      "partial"), plot = TRUE, na.action = na.fail, demean = TRUE, 
          ...) 
{
  type <- match.arg(type)
  if (type == "partial") {
    m <- match.call()
    m[[1L]] <- quote(stats::pacf)
    m$type <- NULL
    return(eval(m, parent.frame()))
  }
  series <- deparse(substitute(x))
  x <- na.action(as.ts(x))
  x.freq <- frequency(x)
  x <- as.matrix(x)
  if (!is.numeric(x)) 
    stop("'x' must be numeric")
  sampleT <- as.integer(nrow(x))
  nser <- as.integer(ncol(x))
  if (is.na(sampleT) || is.na(nser)) 
    stop("'sampleT' and 'nser' must be integer")
  if (is.null(lag.max)) 
    lag.max <- floor(10 * (log10(sampleT) - log10(nser)))
  lag.max <- as.integer(min(lag.max, sampleT - 1L))
  if (is.na(lag.max) || lag.max < 0) 
    stop("'lag.max' must be at least 0")
  if (demean) 
    x <- sweep(x, 2, colMeans(x, na.rm = TRUE), check.margin = FALSE)
  lag <- matrix(1, nser, nser)
  lag[lower.tri(lag)] <- -1
  acf <- .Call(C_acf, x, lag.max, type == "correlation")
  lag <- outer(0:lag.max, lag/x.freq)
  acf.out <- structure(list(acf = acf, type = type, n.used = sampleT, 
                            lag = lag, series = series, snames = colnames(x)), class = "acf")
  if (plot) {
    plot.acf(acf.out, ...)
    invisible(acf.out)
  }
  else acf.out
}


ggplot(data = lag2) +
  geom_point(aes(x = a, y = b_na))
