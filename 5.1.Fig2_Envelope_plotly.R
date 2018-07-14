
## 06/2018 - Leberger et al. 2018. Mediterranean wetland conservation in the context of climate and land-cover change.


### 5.1. Figures : Climate envelope
###############################################

change_tab <- read.table('/Users/localadmin/Documents/PhD/Paper3_Wetlands/Data_to_publish/Table_all_new.txt', h=T)
head(change_tab)
dim(change_tab)


## I. Natural wetlands change (pOc)
#---------

## 1. Make figure 

library(plotly)

## colorblind palette 

f <- list(size=18)
color_tab <- change_tab $pa_cover_cat
color_tab[change_tab $pa_cover_cat == '100'] <- "rgb(69, 117, 180)" 
color_tab[change_tab $pa_cover_cat == '50'] <- "rgb(254, 224, 144)"
color_tab[change_tab $pa_cover_cat == '0'] <- "rgb(215, 48, 39)" 

## When positive loss => draw outline differently

positiv_change <- change_tab [change_tab$natwet_poc > 0,]

outline_col = rep('grey90', length(change_tab $natwet_poc))
outline_col[change_tab$natwet_poc > 0] <- 'black' 

outline_width = rep(1, length(change_tab $natwet_poc))
outline_width[change_tab$natwet_poc > 0] <- 3

## When no loss => no outline

outline_width[change_tab$natwet_poc == 0] <- 0

## Figure

p_nat <- plot_ly(change_tab, x = ~pmean_diff, y = ~tmean_diff, type = 'scatter', mode = 'markers',
        marker = list(size = ~abs(natwet_poc)*60+6, opacity = 0.55, color = color_tab, line = list(width = outline_width, color = outline_col))) %>% 
  layout(#title = 'Natural wetland change',
         xaxis = list(
                 title='Precipitation change (mm)', 
                 size=12,
                 range=c(-410, 310),
         		  showgrid = F, 
         		  gridcolor = 'light grey',
         		  titlefont=f),
         yaxis = list(
                 title='Temperature change (°C)',
                 size=12,
                 range=c(-0.2, 1.3),
                 showgrid = F, 
                 gridcolor = 'light grey',
                 titlefont=f),
         paper_bgcolor = '#FFFFFF',
         plot_bgcolor = '#FFFFFF') 
p_nat


# to correct bubble size, interval goes from 0.7 (initially NO loss) to 100.7...
summary(abs(change_tab $natwet_poc)*100+0.7)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.700   1.859   5.844  14.203  17.781 100.700 


## 2. Number of site per quantile

# in qq11 (bottom left):
#---

table(change_tab$pa_cover_cat[change_tab$tmean_diff <= median(change_tab$tmean_diff) & change_tab$pmean_diff <= median(change_tab$pmean_diff)])
#  0  50 100 
#  8   5  19 

# in qq12 (bottom right):
#---

table(change_tab$pa_cover_cat[change_tab$tmean_diff <= median(change_tab$tmean_diff) & change_tab$pmean_diff > median(change_tab$pmean_diff)])
#  0  50 100 
# 13   7  66 

# in qq21 (up left):
#---

table(change_tab$pa_cover_cat[change_tab$tmean_diff > median(change_tab$tmean_diff) & change_tab$pmean_diff < median(change_tab$pmean_diff)])
#  0  50 100 
# 51  21  14

# in qq22 (up right):
#---

# with data
table(change_tab$pa_cover_cat[change_tab$tmean_diff > median(change_tab$tmean_diff) & change_tab$pmean_diff > median(change_tab$pmean_diff)])
#  0  50 100 
#  6   6  20 




## II. Beta diversity 
#--------

## colorblind palette :

## Bubble size
summary(change_tab $beta_sor_1991_2010)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.08333 0.26510 0.43396 0.45992 0.62434 1.00000     125 

summary(abs(change_tab $natwet_poc))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.01159 0.05144 0.13503 0.17081 1.00000 

f <- list(size=18)
color_tab <- change_tab $pa_cover_cat
color_tab[change_tab $pa_cover_cat == '100'] <- "rgb(69, 117, 180)" 
color_tab[change_tab $pa_cover_cat == '50'] <- "rgb(254, 224, 144)"
color_tab[change_tab $pa_cover_cat == '0'] <- "rgb(215, 48, 39)" 
color_tab[is.na(change_tab$beta_sor_1991_2010)] <- "grey"

# bubble size of the grey bubble for them to appear in the plot = 0.1000000
change_tab2 <- change_tab
change_tab2 $beta_sor_1991_2010[is.na(change_tab2 $beta_sor_1991_2010)] <- as.numeric('0.100000') # 0.100000 * 60 = 6

## When no data => no outline
outline_width = rep(1, length(change_tab $beta_sor_1991_2010))
outline_width[is.na(change_tab $beta_sor_1991_2010)] <- 6
outline_col = rep('NA', length(change_tab $beta_sor_1991_2010))
outline_col[!is.na(change_tab $beta_sor_1991_2010)] <- 'grey90' 

## When no data => no outline!
outline_width[is.na(change_tab $beta_sor_1991_2010)] <- 0

p_beta <- plot_ly(change_tab2, x = ~pmean_diff, y = ~tmean_diff, type = 'scatter', mode = 'markers',
        marker = list(size = ~ beta_sor_1991_2010*60, opacity = 0.55, color = color_tab, line = list(width = outline_width, color= outline_col))) %>%
  layout(#title = 'Sites',
         xaxis = list(
                 title='Precipitation change (mm)',
                 range=c(-400, 300),
                 size=12,
                 showgrid = F, 
                 gridcolor = 'rgb(255, 255, 255)',
                 titlefont=f),
         yaxis = list(
                 title='Temperature change (°C)', 
                 range=c(-0.2, 1.3),
                 size=12,
                 showgrid = F, 
                 gridcolor = 'rgb(255, 255, 255)',
                 titlefont=f),
         paper_bgcolor = '#FFFFFF',
         plot_bgcolor = '#FFFFFF')

p_beta



## to know how many sites with no observation data:

# in total:
table(change_tab$pa_cover_cat[is.na(change_tab$beta_sor_1991_2010)] )
#  0  50 100 
# 69  20  36

# percentage of missing sites that are not protected:
69 / (69 + 20 + 36)
# [1] 0.552

236 - (69 + 20 + 36)
# [1] 111


## 2. Number of site per quantile

# in each quarter:
tmean_qq11 <- quantile(change_tab$tmean_diff, 0.5)
pmean_qq11 <- quantile(change_tab$pmean_diff, 0.5)

# in qq11 (bottom left):
#---

# with data
table(change_tab$pa_cover_cat[!is.na(change_tab$beta_sor_1991_2010) & change_tab$tmean_diff <= tmean_qq11 & change_tab$pmean_diff <= pmean_qq11 ] )
# 50 100 
#  2  11 

# with NA 
table(change_tab$pa_cover_cat[is.na(change_tab$beta_sor_1991_2010) & change_tab$tmean_diff <= tmean_qq11 & change_tab$pmean_diff <= pmean_qq11 ] )
#  0  50 100 
#  8   3   8 

# in qq12 (bottom right):
#---

# with data
table(change_tab$pa_cover_cat[!is.na(change_tab$beta_sor_1991_2010) & change_tab$tmean_diff <= tmean_qq11 & change_tab$pmean_diff > pmean_qq11 ] )
#  0  50 100 
#  7   6  51 

# with NA 
table(change_tab$pa_cover_cat[is.na(change_tab$beta_sor_1991_2010) & change_tab$tmean_diff <= tmean_qq11 & change_tab$pmean_diff > pmean_qq11 ] )
#  0  50 100 
#  6   1  15 


# in qq21 (up left):

# with data
table(change_tab$pa_cover_cat[!is.na(change_tab$beta_sor_1991_2010) & change_tab$tmean_diff > tmean_qq11 & change_tab$pmean_diff <= pmean_qq11 ] )
#  50 100 
  7   6 
  
# with NA 
table(change_tab$pa_cover_cat[is.na(change_tab$beta_sor_1991_2010) & change_tab$tmean_diff > tmean_qq11 & change_tab$pmean_diff <= pmean_qq11 ] )
#  0  50 100 
# 51  14   8 

# in qq22 (up right):

# with data
table(change_tab$pa_cover_cat[!is.na(change_tab$beta_sor_1991_2010) & change_tab$tmean_diff > tmean_qq11 & change_tab$pmean_diff > pmean_qq11 ] )
#  50 100 
#   2   4  15 

# with NA 
table(change_tab$pa_cover_cat[is.na(change_tab$beta_sor_1991_2010) & change_tab$tmean_diff > tmean_qq11 & change_tab$pmean_diff > pmean_qq11 ] )
#  0  50 100 
#  4   2   5 



## Simson Diversity
##---------

## colorblind palette

f <- list(size=18)
color_tab <- change_tab $pa_cover_cat
color_tab[change_tab $pa_cover_cat == '100'] <- "rgb(69, 117, 180)" 
color_tab[change_tab $pa_cover_cat == '50'] <- "rgb(254, 224, 144)"
color_tab[change_tab $pa_cover_cat == '0'] <- "rgb(215, 48, 39)" 
color_tab[is.na(change_tab$div_poc)] <- "grey"

## When positive loss => draw outline differently

positiv_change <- change_tab [change_tab$div_poc > 0,]

outline_col = rep('NA', length(change_tab $div_poc))
outline_col[!is.na(change_tab $div_poc)] <- 'grey' 
outline_col[change_tab$div_poc > 0] <- 'black' 

outline_width = rep(1.5, length(change_tab $div_poc))
outline_width[change_tab$div_poc > 0] <- 3
outline_width[is.na(change_tab $div_poc)] <- 0

## When no loss => no outline

outline_width[change_tab$div_poc == 0] <- 0


# grey bubble size to appear in the plot

change_tab2 <- change_tab
change_tab2 $div_poc[is.na(change_tab2 $div_poc)] <- as.numeric('0')

# Plot

p_div <- plot_ly(change_tab2, x = ~pmean_diff, y = ~tmean_diff, type = 'scatter', mode = 'markers',
        marker = list(size = ~ div_poc*60+6, opacity = 0.55, color = color_tab, line = list(width = outline_width, color= outline_col))) %>%
  layout(#title = 'Sites',
         xaxis = list(
                 title='Precipitation change (mm)',
                 showgrid = F, 
                 size=12,
                 gridcolor = 'rgb(255, 255, 255)',
                 titlefont=f),
         yaxis = list(
                 title='Temperature change (°C)', 
                 showgrid = F, 
                 size=12,
                 gridcolor = 'rgb(255, 255, 255)',
                 titlefont=f),
         paper_bgcolor = '#FFFFFF',
         plot_bgcolor = '#FFFFFF')

p_div


