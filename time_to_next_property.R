# time to buy next 

property_value_proxy = 250000
down_payment = .25 * property_value_proxy
ltv=.75
tax_rate = 0.025 #2.5%
loan_term = 360 # 30 years fixed
estimated_interest_rate = .06
principal_interest_payments = (property_value_proxy*ltv*(estimated_interest_rate/12)/(1-(1/(1+estimated_interest_rate/12)^loan_term)))
monthly_property_tax = (tax_rate * property_value_proxy) / 12
monthly_insurance_estimate = (2000+2668+1928.85)/12 # flood, windstorm, hazard/homeowners insurance
PITI = principal_interest_payments + monthly_property_tax + monthly_insurance_estimate
fannie_mae_cash_reserves = PITI * 6
estimated_closing_costs = 0.041*property_value_proxy

cash_to_close = estimated_closing_costs + fannie_mae_cash_reserves + down_payment

property_no = seq(1,5,1)
cash_flow_projection_10k_annual = c(17500,rep(10000,4))
cash_flow_projection_12k_annual = c(17500,rep(12000,4))
cash_flow_projection_15k_annual = c(17500,rep(15000,4))
cash_flow_projection_17k_annual = c(17500,rep(17000,4))
cash_flow_projection_20k_annual = c(17500,rep(20000,4))
work_savings = 50000
df = data.frame(property_no,cash_flow_projection_10k_annual,cash_flow_projection_12k_annual,cash_flow_projection_15k_annual,cash_flow_projection_17k_annual,cash_flow_projection_20k_annual,work_savings)
df$cumsum_10k = cumsum(df$cash_flow_projection_10k_annual)
df$cumsum_12k = cumsum(df$cash_flow_projection_12k_annual)
df$cumsum_15k = cumsum(df$cash_flow_projection_15k_annual)
df$cumsum_17k = cumsum(df$cash_flow_projection_17k_annual)
df$cumsum_20k = cumsum(df$cash_flow_projection_20k_annual)
df$total_work_and_cash_flow_10k = (df$work_savings + df$cumsum_10k)
df$total_work_and_cash_flow_12k = (df$work_savings + df$cumsum_12k)
df$total_work_and_cash_flow_15k = (df$work_savings + df$cumsum_15k)
df$total_work_and_cash_flow_17k = (df$work_savings + df$cumsum_17k)
df$total_work_and_cash_flow_20k = (df$work_savings + df$cumsum_20k)
df$time_to_buy_10k = (cash_to_close / df$total_work_and_cash_flow_10k) *12
df$time_to_buy_12k = (cash_to_close / df$total_work_and_cash_flow_12k)*12
df$time_to_buy_15k = (cash_to_close / df$total_work_and_cash_flow_15k)*12
df$time_to_buy_17k = (cash_to_close / df$total_work_and_cash_flow_17k)*12
df$time_to_buy_20k = (cash_to_close / df$total_work_and_cash_flow_20k)*12

library(ggplot2)
library(reshape2)

.25*175000

plot_df = df[1:nrow(df),18:22]
plot_df = cbind("property_no"=df$property_no,plot_df)

mm <- melt(plot_df, id='property_no')
# plot data
p1 = ggplot(data = mm, aes(x = property_no, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_y_continuous(breaks = seq(0, max(mm$value)+1, by = 1),"Time to buy 250k property (months)")+
  theme_minimal()+
  ggtitle("Time To buy 250k Property At Varying Cash Flow Projections - Cumulative cash flow + 50k work savings")+
  guides(fill=guide_legend(title="Legend"))+
  scale_fill_discrete(labels = c("10K Annual Cash Flow", "12K Annual Cash Flow", "15K Annual Cash Flow", "17K Annual Cash Flow", "20K Annual Cash Flow"))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


library(gridExtra)
d <- round(plot_df,2)
g <- tableGrob(d)
grid.arrange(p1, g,
             nrow=2,
             as.table=TRUE,
             heights=c(3,1))
