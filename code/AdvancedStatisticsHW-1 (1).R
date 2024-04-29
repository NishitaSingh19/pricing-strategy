library(ggplot2)
library(dplyr)

hdtv = read.csv('C:/Users/NP/Downloads/pn_hdtv.csv')
hdtv

calculate_optimal_pricing <- function(hdtv) {
  
  
  screen_75 <- hdtv$Screen.75.inch
  screen_85 <- hdtv$Screen.85.inch
  screen_4k <- hdtv$X1K.or.4K
  screen_sony <- hdtv$Sony...1 
  screen_price <- hdtv$Price..low...0..hi..1.
  
  linear_model <- lm(hdtv$Preference ~ screen_75 + screen_85 + screen_4k + screen_sony + screen_price)
  linear_model 
  
  summary(linear_model)
  
  df <- summary(linear_model)
  
  # Create a DataFrame from the model summary
  coefficients_df <- as.data.frame(df$coefficients,index =TRUE)
  
  # The coefficients DataFrame will have estimates, standard error, t value, and p value
  print(coefficients_df)
  
  coef_intercept <- coefficients_df["(Intercept)",'Estimate']
  print("Parthworth")
  print(coef_intercept)
  coef_screen_75 <- coefficients_df['screen_75','Estimate']
  print("Parthworth screen 75")
  print(coef_screen_75)
  coef_screen_85 <- coefficients_df['screen_85','Estimate']
  print("Parthworth screen 85")
  print(coef_screen_85)
  coef_screen_4k <- coefficients_df['screen_4k','Estimate']
  print("Parthworth 4k")
  print(coef_screen_4k)
  coef_screen_sony <- coefficients_df['screen_sony','Estimate']
  print("Parthworth sony")
  print(coef_screen_sony)
  coef_screen_price <- coefficients_df['screen_price','Estimate']
  print("Parthworth price")
  print(coef_screen_price)
  
  ## partworth of the attribute
  
  range_size <- abs(coef_screen_85 - coef_screen_75)
  range_4k <- abs(coef_screen_4k - 0)
  range_sony <- abs(coef_screen_sony - 0)
  range_price <- abs(0-(coef_screen_price))
  sum_of_range <- abs(range_size + range_4k +range_sony +range_price)
  
  
  ## Importance of the attributes
  importance_screen_size <- abs(round((range_size/sum_of_range)* 100,2 ))
  print("importance screen size")
  print(importance_screen_size)
  importance_screen_resolution <- abs(round((range_4k/sum_of_range)* 100,2 ))
  print("importance screen resolution")
  print(importance_screen_resolution)
  importance_screen_brand <- abs(round((range_sony/sum_of_range)* 100,2))
  print("importance screen brand")
  print(importance_screen_brand)
  importance_screen_price <- abs(round((range_price/sum_of_range)* 100,2))
  print("importance screen price")
  print(importance_screen_price)
  
  ## willingness to pay
  sony_price <- 2500
  sharp_price <- 2000
  utility <- (sony_price - sharp_price)/partworth_price
  wtp_screen_size_85 <- utility * coef_screen_85
  print("wtp screen size 85")
  print(wtp_screen_size_85)
  wtp_brand_sony <- utility * coef_screen_sony
  print("wtp brand sony")
  print(wtp_brand_sony)
  wtp_resolution_4k <- utility * coef_screen_4k
  print("wtp resolution 4k")
  print(wtp_resolution_4k)
  
  ## optimal price
  
  my_design <- c(1,0,1,0,0,1500)
  sony_design <- c(1,1,0,1,1,2500)
  sharp_design <-c(1,0,1,1,0,2000)
  
  
  design_df <- data.frame(my_design = c(1,0,1,0,0,1500),
                          sony_design = c(1,1,0,1,1,2500),
                          sharp_design =c(1,0,1,1,0,2000),
                          partworth = c(coef_intercept,coef_screen_75,coef_screen_85,coef_screen_4k,coef_screen_sony,coef_screen_price),
                          row.names = c('Intercept','screen_75','screen_85','resolution','sony','price'))
  
  
  design_df <- t(design_df)
  
  ## calculating utility for the designs
  utility <- function(df,design_name){
    utility <- design_df[design_name,'Intercept'] * design_df['partworth','Intercept'] + 
      design_df[design_name,'screen_75'] * design_df['partworth','screen_75'] +
      design_df[design_name,'screen_85'] * design_df['partworth','screen_85'] +
      design_df[design_name,'resolution'] * design_df['partworth','resolution'] +
      design_df[design_name,'sony'] * design_df['partworth','sony'] +
      (((design_df[design_name,'price'] - 2000)/(2500 -2000))* design_df['partworth','price'])
    
    
  }
  
  # Assuming your initial 'design_df' structure is correct and the transpose 't()' has already been applied
  
  # Initialize vectors to store market share results
  market_shares_my_design <- c()
  market_shares_sony <- c()
  market_shares_sharp <- c()
  
  # For loop to calculate market shares for prices from 1500 to 2200
  for (price in seq(1500, 2600, by=100)) {
    design_df['my_design', 'price'] <- price
    
    utility_my_design <- utility(design_df, 'my_design')
    utility_sony <- utility(design_df, 'sony_design')
    utility_sharp <- utility(design_df, 'sharp_design')
    
    attractiveness_my_design <- exp(utility_my_design)
    attractiveness_sony <- exp(utility_sony)
    attractiveness_sharp <- exp(utility_sharp)
    
    total_attractiveness <- attractiveness_my_design + attractiveness_sony + attractiveness_sharp
    
    # Calculating market share for each design
    market_share_my_design <- attractiveness_my_design / total_attractiveness
    market_share_sony <- attractiveness_sony / total_attractiveness
    market_share_sharp <- attractiveness_sharp / total_attractiveness
    
    # Storing the results
    market_shares_my_design <- c(market_shares_my_design, market_share_my_design)
    market_shares_sony <- c(market_shares_sony, market_share_sony)
    market_shares_sharp <- c(market_shares_sharp, market_share_sharp)
  }
  
  # Combine the market shares into a data frame
  market_size = 100
  net_cost = 2000 
  market_share_results <- data.frame(
    price = seq(1500, 2600, by=100),
    market_share = market_shares_my_design
  )
  
  
  market_share_results$sales <- market_share_results$market_share * market_size
  market_share_results$margin <- market_share_results$price - net_cost
  market_share_results$profit <- market_share_results$margin * market_share_results$sales
  
  # Print the results
  print(market_share_results)
  max_profit_analysis <- market_share_results %>%
    arrange(desc(profit)) %>%
    top_n(1, profit)
  
  # Extract the values for maximum profit
  max_profit_value <- max_profit_analysis$profit
  price_at_max_profit <- max_profit_analysis$price
  market_share_at_max_profit <- max_profit_analysis$market_share
  
  # Print the values
  print(paste("Maximum profit:", max_profit_value))
  print(paste("Price at maximum profit:", price_at_max_profit))
  print(paste("Market share at maximum profit:", market_share_at_max_profit))
  
  library(ggplot2)
  
  
  # Plot
  ## 7 and 8
  ggplot(market_share_results, aes(x=price, y=sales)) +
    geom_line() +geom_point()
  
  ggplot(market_share_results, aes(x=price, y=profit,main = "profit. vs price") ) +
    geom_line() +geom_point() 
  
}

hdtv = read.csv('C:/Users/NP/Downloads/pn_hdtv.csv')
hdtv
# To use the function, pass the hdtv DataFrame like this:
calculate_optimal_pricing(hdtv)
