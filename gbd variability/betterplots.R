# Library
library(tidyverse)
library(ggtext)
library(ggplot2)
library(RColorBrewer)
library(ggbump)
library(colorspace)  # for high-quality qualitative colors
library(dplyr)
library(tidyr)
library(scales)
library(stringr)
require(gridExtra)
library(patchwork)

library(Cairo)
library(ggplot2)
library(colorspace)  # for qualitative_hcl
theme_set(theme_classic(base_family = "ARIAL"))

# Read data.
data <- read.csv('./tables/Supplementary Data File 1.csv')
data1<- read.csv('./tables/Supplementary Data File 2.csv')

# mutate to get factors for leveling of data points
data <- data %>%
  mutate(
    Overarching_group = factor(Overarching_group,
                               levels = c("Behavioural risks",
                                          "Environmental/occupational risks",
                                          "Metabolic risks")
    ),
    is_overarching = ifelse(Overarching_group == Risk_factor, 0, 1),
    is_group       = ifelse(Group == Risk_factor, 0, 1)
  ) %>%
  arrange(Overarching_group, is_overarching, Group, is_group, fold_range_vs_median_Deaths) %>%
  mutate(
    sort_id     = row_number(),
    Risk_factor = factor(Risk_factor, levels = Risk_factor) # lock in arranged order
  )

risk_order=unique(data$Risk_factor)

data <- data %>%
  mutate(Risk_factor = forcats::fct_rev(Risk_factor))

# get positions of the overarching group headers
group_lines <- data %>%
  filter(Risk_factor %in% Group) %>%
  mutate(xpos = as.numeric(Risk_factor))



#====================== QUICK NESTED ANALYSIS-HOW MUCH OF VARIANCE
#====================== EXPLAINED BY INTER-ITERATION VAR
library(lme4)
df=read_csv('./data/processed_data/temps.csv')
df=df%>%filter(measure=='Deaths'&anal_year<2019)%>%
  mutate(year_gbd=as.factor(year_gbd),
         anal_year=as.factor(anal_year))%>%
  group_by(Risk_factor)%>%
  mutate(center_val=(val-mean(val)))%>%
  ungroup()
df$Riskyear=interaction(df$Risk_factor,df$anal_year)
model=lmer(center_val~1+(1|Riskyear/year_gbd),data=df)
summary(model)
ranef(model)$`year_gbd:Riskyear`
vcmodel=VarCorr(model)%>%as.data.frame()
vcmodel
uno=vcmodel$vcov[vcmodel$grp=='Riskyear']
dos=vcmodel$vcov[vcmodel$grp=='year_gbd:Riskyear']
tres=vcmodel$vcov[vcmodel$grp=='Residual']

# write a math case for this
tot_var=uno+dos+tres
dos/tot_var




#====================== data preparation
df1=data%>%select(Overarching_group,Group,Risk_factor,
                  rmu_Deaths,rmu_DALYs)%>%
  mutate(anal_year=as.factor('across years'))
df2 <- data1 %>%
  select(Overarching_group, Group, Risk_factor, anal_year,
         rmu_Deaths, rmu_DALYs) %>%
  mutate(anal_year = factor(anal_year,
                     levels = c('across years','1990','2005','2006','2007',
                                '2010','2013','2015','2016','2017','2019')))
  

sdf=rbind(df1,df2)
sdf$Risk_factor=str_replace(sdf$Risk_factor,'\n','')

sdf_long <- sdf %>%
  pivot_longer(
    cols = c(rmu_Deaths, rmu_DALYs),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    Overarching_group = factor(Overarching_group,
                               levels = c("Behavioural risks",
                                          "Environmental/occupational risks",
                                          "Metabolic risks")
    ),
    is_overarching = ifelse(Overarching_group == Risk_factor, 0, 1),
    is_group       = ifelse(Group == Risk_factor, 0, 1)
  ) %>%
  arrange(Overarching_group, is_overarching, Group, is_group, measure)%>%
  # set measure factor
  mutate(
    measure = factor(measure,
                     levels = c("rmu_Deaths", "rmu_DALYs"),
                     labels = c("Deaths", "DALYs")),
    # lock Risk_factor factor levels according to original order
  )

sdf_long=sdf_long%>%group_by(measure,anal_year)%>% 
  arrange(Overarching_group, is_overarching, Group, is_group, measure)

risk_order=unique(sdf_long$Risk_factor)

sdf_long=sdf_long%>%filter(Risk_factor!='All risk factors')
sdf_long$Risk_factor=factor(sdf_long$Risk_factor,level=rev(risk_order))

sdf_long %>% filter(str_detect(Risk_factor, "\\s+$"))

#====================== HEATMAP FOR R/M
heatmap_Deaths=ggplot(sdf_long%>%filter(measure=='Deaths'), aes(x = anal_year, y = Risk_factor, fill = value)) +
  geom_tile() +
  #facet_wrap(~measure, ncol = 2, scales = "free_x") +  # one heatmap per measure
  scale_fill_gradient2(
    low = "white",
    high = "red",
    midpoint = 0.95,
    limits = c(0, 3),
    oob = squish
  ) +
  theme_classic() +
  theme(text = element_text(family = "ARIAL", size = 10),
    axis.text.x = element_text(size=10,angle = 45, hjust = 1,color='black'),
    axis.text.y = element_text(size=10,color = "black", hjust=1,
                               margin = margin(r = 5)  # add small gap from the axis line
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
    
  )


# --- Prepare the data (starting from df2) ---
df2 <- data1 %>%
  select(Overarching_group, Group, Risk_factor, anal_year,
         cv_Deaths, cv_DALYs) %>%
  mutate(anal_year = factor(anal_year,
                            levels = c('across years','1990','2005','2006','2007',
                                       '2010','2013','2015','2016','2017','2019')))


df2_long <- df2 %>%
  select(Overarching_group, Group, Risk_factor, anal_year, cv_Deaths, cv_DALYs) %>%
  pivot_longer(
    cols = c(cv_Deaths, cv_DALYs),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = factor(measure,
                     levels = c("cv_Deaths", "cv_DALYs"),
                     labels = c("Deaths", "DALYs")),
    Overarching_group = factor(Overarching_group,
                               levels = c("Behavioural risks",
                                          "Environmental/occupational risks",
                                          "Metabolic risks")),
    Risk_factor = str_replace(Risk_factor, "\n", "")
  )


# --- Step 2: Create dummy rows for overarching group labels ---
dummy_groups <- df2_long %>%
  select(Overarching_group) %>%
  distinct() %>%
  mutate(
    Risk_factor = Overarching_group,  # use group name as risk factor
    value = NA,
    measure = "Deaths",
    anal_year = NA
  )

df2_long <- bind_rows(df2_long, dummy_groups)
df2_long$Risk_factor=factor(df2_long$Risk_factor,level=rev(risk_order))

boxplot_Deaths=ggplot(df2_long %>% filter(measure == "Deaths"),
       aes(x = Risk_factor, y = value+0.0001)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.5, color = "black") +
  scale_y_continuous(
    breaks = c(0, 0.1, 0.2, 0.5, 1),  # desired tick positions
    labels = scales::label_number(accuracy = 0.1),  # optional formatting
    limits=c(0,1.5)
  ) +
  coord_flip() +
  theme_classic() +
  
  labs(
    x = "Risk factor",
    y = "Coefficient of variation",
    fill = "Overarching group",
  ) +

  theme(text = element_text(family = "ARIAL", size = 10),
        axis.text.x = element_text(color='black'),
        axis.text.y = element_text(size=10,color = "black", hjust=1,
                                   margin = margin(r = 5)  # add small gap from the axis line
        ),
        axis.title.y = element_blank(),
        #axis.title.x = element_blank(),
        legend.position = 'none'
  )







CairoPDF('./figs/Fig2.pdf',height=10,width=16)

# Arrange with unequal widths and aligned axes
heatmap_Deaths + boxplot_Deaths + plot_layout(ncol = 2, widths = c(1, 1.5))
dev.off()

#====================== HEATMAP FOR R/M
heatmap_DALYs=ggplot(sdf_long%>%filter(measure=='DALYs'), aes(x = anal_year, y = Risk_factor, fill = value)) +
  geom_tile() +
  #facet_wrap(~measure, ncol = 2, scales = "free_x") +  # one heatmap per measure
  scale_fill_gradient2(
    low = "white",
    high = "red",
    midpoint = 0.95,
    limits = c(0, 3),
    oob = squish
  ) +
  theme_classic() +
  theme(text = element_text(family = "ARIAL", size = 10),
        axis.text.x = element_text(size=10,angle = 45, hjust = 1,color='black'),
        axis.text.y = element_text(size=10,color = "black", hjust=1,
                                   margin = margin(r = 5)  # add small gap from the axis line
        ),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
        
  )



# --- Prepare the data (starting from df2) ---
df2 <- data1 %>%
  select(Overarching_group, Group, Risk_factor, anal_year,
         cv_Deaths, cv_DALYs) %>%
  mutate(anal_year = factor(anal_year,
                            levels = c('across years','1990','2005','2006','2007',
                                       '2010','2013','2015','2016','2017','2019')))


df2_long <- df2 %>%
  select(Overarching_group, Group, Risk_factor, anal_year, cv_Deaths, cv_DALYs) %>%
  pivot_longer(
    cols = c(cv_Deaths, cv_DALYs),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = factor(measure,
                     levels = c("cv_Deaths", "cv_DALYs"),
                     labels = c("Deaths", "DALYs")),
    Overarching_group = factor(Overarching_group,
                               levels = c("Behavioural risks",
                                          "Environmental/occupational risks",
                                          "Metabolic risks")),
    Risk_factor = str_replace(Risk_factor, "\n", "")
  )


# --- St ep 2: Create dummy rows for overarching group labels ---
dummy_groups <- df2_long %>%
  select(Overarching_group) %>%
  distinct() %>%
  mutate(
    Risk_factor = Overarching_group,  # use group name as risk factor
    value = NA,
    measure = "DALYs",
    anal_year = NA
  )

df2_long <- bind_rows(df2_long, dummy_groups)
df2_long$Risk_factor=factor(df2_long$Risk_factor,level=rev(risk_order))

boxplot_DALYs=ggplot(df2_long %>% filter(measure == "DALYs"),
          aes(x = Risk_factor, y = value+0.0001)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.5, color = "black") +
  scale_y_continuous(
    breaks = c(0, 0.1, 0.2, 0.5, 1),  # desired tick positions
    labels = scales::label_number(accuracy = 0.1),  # optional formatting
    limits=c(0,1.5)
  ) +
  coord_flip() +
  theme_classic() +
  
  labs(
    x = "Risk factor",
    y = "Coefficient of variation",
    fill = "Overarching group",
  ) +
  
  theme_classic() +
  theme(text = element_text(family = "ARIAL", size = 10),
        axis.text.x = element_text(size=10,angle = 45, hjust = 1,color='black'),
        axis.text.y = element_text(size=10,color = "black", hjust=1,
                                   margin = margin(r = 5)  # add small gap from the axis line
        ),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
        
  )

CairoPDF('./figs/eFig3.pdf',height=10,width=16)

# Arrange with unequal widths and aligned axes
heatmap_DALYs + boxplot_DALYs + plot_layout(ncol = 2, widths = c(1, 1.5))

dev.off()

#====================== RANKS PLOTS THAT ARE NICE




# Read data
df <- read.csv("./data/processed_data/dietary_ranks.csv")
df$Risk_factor <- gsub("\n", "", df$Risk_factor)


library(dplyr)
library(ggplot2)
library(patchwork)

make_plot <- function(df_sub) {
  # Order by 2021 rank within this measure
  order_2021 <- df_sub %>%
    filter(anal_year == 2023) %>%
    arrange(Rank) %>%
    pull(Risk_factor)
  full_order <- c(order_2021, setdiff(unique(df_sub$Risk_factor), order_2021))
  
  df_sub <- df_sub %>%
    mutate(Risk_factor = factor(Risk_factor, levels = full_order))
  
  ggplot(df_sub, aes(x = anal_year, y = Rank, color = Risk_factor)) +
    geom_bump(size = 1.5, alpha = 0.8) +
    geom_point(size = 3) +
    scale_y_reverse(breaks = seq(min(df_sub$Rank), max(df_sub$Rank), by = 1)) +
    scale_color_manual(values = colors_used) +
    labs(
      title = unique(df_sub$measure),
      x = "Year", y = "Rank", color = "Risk factor"
    ) +
    theme_classic()
}

plots <- df %>%
  split(.$measure) %>%
  lapply(make_plot)
# Export plot with Arial and CairoPDF (true text)
CairoPDF("figs/eFig7.pdf", width = 8, height = 8)
plots[[1]] / plots[[2]]  # stack vertically (or use | for side-by-side)

dev.off()

# install if not already installed
#install.packages("ineq")


