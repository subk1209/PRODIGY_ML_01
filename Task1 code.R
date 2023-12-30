library(tidyverse)
library(ggcorrplot)
library(gridExtra)
library(ggpubr)
library(scales)
library(car)
library(jtools)


df <- read_csv('D:/Internships/Prodigy/Task 1 (Price Prediction)/Data1.csv',
               show_col_types = F)

glimpse(df)


df <- df %>% rename('FirstFlrSf' = '1stFlrSF',
                    'SecondFlrSF' = '2ndFlrSF') %>% 
  select(FirstFlrSf, SecondFlrSF,
         TotalBsmtSF, LotArea, BedroomAbvGr,
         BsmtFullBath, BsmtHalfBath,
         FullBath, HalfBath, SalePrice)%>% 
  mutate(across(c(BedroomAbvGr,BsmtFullBath,
                  BsmtHalfBath, FullBath,
                  HalfBath), as.factor))

df %>% is.na() %>% sum()

#======================================================================
uni_cont <- function(var){
  df %>% ggplot(aes({{var}})) +
    geom_histogram(aes(y = after_stat(density)),
                   colour = 'red', fill = 'pink') +
    geom_density(colour = 'darkgreen', lty = 2, lwd = 1) +
    theme_minimal() + 
    labs(x = '', y = 'Frequency density\n') -> p1
  
  df %>% ggplot(aes({{var}})) +
    geom_boxplot(fill = 'skyblue', outlier.colour = 'red',
                 outlier.shape = 4,
                 outlier.stroke = 1) + theme_minimal() +
    labs(x = '') -> p2
  
  gridExtra::grid.arrange(p1,p2, ncol = 2)
}


uni_cont(FirstFlrSf)
uni_cont(SecondFlrSF)
uni_cont(SecondFlrSF)
uni_cont(TotalBsmtSF)
uni_cont(LotArea)


uni_cat <- function(var){
  df %>% count({{var}}) %>% 
    mutate(prop = percent(n/sum(n))) %>% 
    ggplot(aes(x = {{var}}, y = n)) +
    geom_col(fill = 'steelblue', colour = 'darkblue', width = 0.3) +
    theme_minimal() + labs(x = '', y = 'Frequency\n') +
    geom_text(aes(label = prop), vjust = -0.5)
}

uni_cat(BedroomAbvGr)

df %>% mutate(BedroomAbvGr = 
                case_when(BedroomAbvGr %in% 0:2 ~ '<=2',
                          BedroomAbvGr == 3 ~ '3',
                          BedroomAbvGr %in% 4:8 ~ '>3')) -> df


uni_cat(BsmtFullBath)
df %>% mutate(BsmtFullBath = 
                case_when(BsmtFullBath == 0 ~ 'No',
                          BsmtFullBath %in% 1:3 ~ 'Yes')) -> df


uni_cat(BsmtHalfBath)
df %>% mutate(BsmtHalfBath = 
                case_when(BsmtHalfBath == 0 ~ 'No',
                          BsmtHalfBath %in% 1:2 ~ 'Yes')) -> df



uni_cat(FullBath)
df %>% mutate(FullBath = 
                case_when(FullBath %in% 0:1 ~ '<=1',
                          FullBath %in% 2:3 ~ '>1')) -> df

uni_cat(HalfBath)
df %>% mutate(HalfBath = 
                case_when(HalfBath == 0 ~ '<1',
                          HalfBath %in% 1:2 ~ '>=1')) -> df


df %>% select(where(is.numeric)) %>% 
  cor() %>% ggcorrplot(lab = T, type = 'upper')
lm(SalePrice ~ ., data = df) %>% vif()



cont_cont <- function(var, var_text){
  df %>% ggplot(aes(x = {{var}}, y = SalePrice)) +
    geom_point(size = 1, colour = 'red') + theme_bw() +
    labs(x = '', y = '', title = paste0('Predictor ~ ', var_text)) +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5))
}

cont_cont(FirstFlrSf, 'FirstFlrSf') -> p1
cont_cont(SecondFlrSF, 'SecondFlrSF') -> p2
cont_cont(TotalBsmtSF, 'TotalBsmtSF') -> p3
cont_cont(LotArea, 'LotArea') -> p4

grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)



cont_cat <- function(var, var_text){
  df %>% ggplot(aes(x = {{var}}, y = SalePrice)) +
    geom_boxplot(fill = 'lightblue', outlier.colour = 'red',
                 outlier.stroke = 1, outlier.shape = 4) +
    theme_minimal() +
    labs(x = '', y = '', title = paste0('Response ~ ', var_text)) + 
    theme(plot.title = element_text(face = 'bold',
                                    hjust = 0.5))
}

cont_cat(BedroomAbvGr, 'BedroomAbvGr') -> p1
cont_cat(BsmtFullBath, 'BsmtFullBath') -> p2
cont_cat(BsmtHalfBath, 'BsmtHalfBath') -> p3
cont_cat(FullBath, 'FullBath') -> p4
cont_cat(HalfBath, 'HalfBath') -> p5


ggarrange(p3,p4,p5, ncol = 3) %>% 
  ggarrange(ggarrange(p1,p2), ., nrow = 2)




l <- lm(log(SalePrice) ~ ., data = df)
summ(l)


ggplot(NULL, aes(x = l$fitted.values, y = l$residuals)) +
  geom_point(size = 1, colour = 'red') + 
  geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = 'blue') +
  labs(x = '\nFitted values', y = 'Residuals\n') +
  theme_minimal()

df %>% mutate('resid' = l$residuals) %>% 
  filter(resid > -1.1) -> df

# Re-fitting the model:
l2 <- lm(log(SalePrice) ~ .-resid, data = df)
summ(l2)




ggplot(NULL, aes(x = l2$fitted.values, y = l2$residuals)) +
  geom_point(size = 1, colour = 'red') + 
  geom_hline(yintercept = 0, lty = 2, lwd = 1, colour = 'blue') +
  labs(x = '\nFitted values', y = 'Residuals\n') +
  theme_minimal()















