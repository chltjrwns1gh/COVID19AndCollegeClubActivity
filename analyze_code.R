library(ggplot2)
library(epitools)
library(gridExtra)
library(ggpubr)
library(rvg)
library(officer)
library(plotrix)
library(Epi)
library(kableExtra)
library(pROC)
library(ztable)
library(moonBook)
library(caret)
library(RColorBrewer)
library(psych)

data_directory <- 'revised_data.csv'
data <- read.csv(data_directory, colClasses=c('numeric', 'numeric', 'numeric', 'numeric', rep('numeric', 12)))

# COVID_19 2번 감염된 사람 데이터는 제거함
data <- data[data$number_of_COVID19_infected<=1, ]
# data =       'school_year'                   #[1]
#            , 'year_of_birth'                 #[2]
#            , 'number_of_participated_club'   #[3]
#            , 'number_of_COVID19_infected'    #[4]
#            
#            , 'activity_code_1_participated'  #[5]
#            , 'activity_code_2_participated'  #[6]
#            , 'activity_code_3_participated'  #[7]
#            , 'activity_code_4_participated'  #[8]
#            , 'activity_code_5_participated'  #[9]
#            , 'activity_code_6_participated'  #[10]
#            , 'activity_code_7_participated'  #[11]
#            , 'activity_code_8_participated'  #[12]
#            , 'activity_code_9_participated'  #[13]
#            , 'activity_code_10_participated' #[14]
#            , 'activity_code_11_participated' #[15]
#            , 'total_activity_number'         #[16]

tbl_school_year = table(COVID19_infection = data$number_of_COVID19_infected, school_year = data$school_year)
tbl_year_of_birth = table(COVID19_infection = data$number_of_COVID19_infected, year_of_birth = data$year_of_birth)
tbl_number_of_club = table(COVID19_infection = data$number_of_COVID19_infected, number_of_club = data$number_of_participated_club)
tbl_total_activity_num = table(COVID19_infection = data$number_of_COVID19_infected, total_activity_number = data$total_activity_number)

tbl_act_1 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_1 = data$activity_code_1_participated)
tbl_act_2 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_2 = data$activity_code_2_participated)
tbl_act_3 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_3 = data$activity_code_3_participated)
tbl_act_4 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_4 = data$activity_code_4_participated)
tbl_act_5 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_5 = data$activity_code_5_participated)

tbl_act_6 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_6 = data$activity_code_6_participated)
tbl_act_7 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_7 = data$activity_code_7_participated)
tbl_act_8 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_8 = data$activity_code_8_participated)
tbl_act_9 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_9 = data$activity_code_9_participated)
tbl_act_10 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_10 = data$activity_code_10_participated)

tbl_act_11 = table(COVID19_infection = data$number_of_COVID19_infected, activity_code_11 = data$activity_code_11_participated)

# 시각화해야 할 데이터들 목록
# [1] raw data
# as.data.frame(data$school_year) # histogram
graph1 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=school_year, fill=-..count..))+
    labs(title='학년별 분포(2022)', x='학년', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(1,2,3,4), labels=c('예과 1학년', '예과 2학년', '본과 1학년', '본과 2학년'))+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
# as.data.frame(data$year_of_birth) # histogram
graph2 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=year_of_birth, fill=-..count..))+
    labs(title='출생년도의 분포', x='출생년도', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(1991, 1994, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004))+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
# as.data.frame(data$number_of_participated_club) # histogram
graph3 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=number_of_participated_club, fill=-..count..))+
    labs(title='활동한 동아리+동문회 수의 분포', x='활동한 동아리+동문회 수', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1,2,3,4,5), labels=c('0','1','2','3','4','5+'))+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
# as.data.frame(data$number_of_COVID19_infected) # histogram
graph4 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=number_of_COVID19_infected, fill=-..count..))+
    labs(title='COVID19 과거력 분포', x='COVID19 과거력', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')

# as.data.frame(data$activity_code_1_participated) # merge, PIE_chart
g1 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_1_participated, fill=-..count..))+
    labs(title='activity 1', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='A')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
g2 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_2_participated, fill=-..count..))+
    labs(title='activity 2', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='B')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
g3 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_3_participated, fill=-..count..))+
    labs(title='activity 3', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='C')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
g4 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_4_participated, fill=-..count..))+
    labs(title='activity 4', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='D')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
g5 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_5_participated, fill=-..count..))+
    labs(title='activity 5', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='E')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
g6 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_6_participated, fill=-..count..))+
    labs(title='activity 6', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='F')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
g7 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_7_participated, fill=-..count..))+
    labs(title='activity 7', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='G')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
g8 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_8_participated, fill=-..count..))+
    labs(title='activity 8', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='H')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
g9 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_9_participated, fill=-..count..))+
    labs(title='activity 9', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='I')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
g10 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_10_participated, fill=-..count..))+
    labs(title='activity 10', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='J')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')
g11 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_11_participated, fill=-..count..))+
    labs(title='activity 11', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='K')+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')


# grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, ncol=3)
# as.data.frame(data$activity_code_2_participated)
# as.data.frame(data$activity_code_3_participated)
# as.data.frame(data$activity_code_4_participated)
# as.data.frame(data$activity_code_5_participated)
# as.data.frame(data$activity_code_6_participated)

# as.data.frame(data$activity_code_7_participated)
# as.data.frame(data$activity_code_8_participated)
# as.data.frame(data$activity_code_9_participated)
# as.data.frame(data$activity_code_10_participated)
# as.data.frame(data$activity_code_11_participated)
# as.data.frame(data$total_activity_number) # histogram
graph5 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=total_activity_number, fill=-..count..))+
    labs(title='참여 활동 분야 수의 분포', x='참여 활동 분야 수', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7))+ 
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')

# [2] relation
# as.data.frame(data[,c(1,4)]) # school_year # table, 
graph6 <- ggplot(data=as.data.frame(data[,c(1,4)]))+
    theme_classic()+
    geom_bar(mapping=aes(x=school_year, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='학년별 분포(2022)', x='학년', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(1,2,3,4), labels=c('예과 1학년', '예과 2학년', '본과 1학년', '본과 2학년'))+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
# as.data.frame(data[,c(2,4)]) # year_of_birth
graph7 <- ggplot(data=as.data.frame(data[,c(2,4)]))+
    theme_classic()+
    geom_bar(mapping=aes(x=year_of_birth, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='출생년도의 분포', x='출생년도', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(1991, 1994, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004))+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
# as.data.frame(data[,c(3,4)]) # number_of_part_club
graph8 <- ggplot(data=as.data.frame(data[,c(3,4)]), aes(x=number_of_participated_club, fill=as.factor(number_of_COVID19_infected)))+
    geom_bar(position='dodge')+
    theme_classic()+
    labs(title='활동한 동아리+동문회 수의 분포', x='활동한 동아리+동문회 수', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1,2,3,4,5), labels=c('0','1','2','3','4','5+'))+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
# as.data.frame(data[,c(5,4)]) # act_code_1 # merge, table, OR box_plot
# 이 이하는 각각도 그려야 하고, OR(w/ CI)도 box-plot으로 통합해서 그려야 한다.
# OR(w/ CI)는 아마도 각각의 데이터를 따로 받은 다음 하나의 data.frame을 만든 다음에 그려야 할 듯.
# as.data.frame(data[,c(6,4)]) # act_code_2
# as.data.frame(data[,c(7,4)]) # act_code_3
# as.data.frame(data[,c(8,4)]) # act_code_4
# as.data.frame(data[,c(9,4)]) # act_code_5
# as.data.frame(data[,c(10,4)]) # act_code_6
# as.data.frame(data[,c(11,4)]) # act_code_7
# as.data.frame(data[,c(12,4)]) # act_code_8
# as.data.frame(data[,c(13,4)]) # act_code_9
# as.data.frame(data[,c(14,4)]) # act_code_10
# as.data.frame(data[,c(15,4)]) # act_code_11
g01 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_1_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 1', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='A')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
g02 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_2_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 2', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='B')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
g03 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_3_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 3', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='C')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
g04 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_4_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 4', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='D')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
g05 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_5_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 5', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='E')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
g06 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_6_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 6', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='F')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
g07 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_7_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 7', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='G')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
g08 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_8_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 8', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='H')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
g09 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_9_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 9', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='I')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
g010 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_10_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 10', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='J')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
g011 <- ggplot()+
    theme_classic()+
    geom_bar(data=data, aes(x=activity_code_11_participated, fill=as.factor(number_of_COVID19_infected)), position='dodge')+
    labs(title='activity 11', x='참여 여부', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1), labels=c('없음', '있음'))+
    labs(tag='K')+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')

# grid.arrange(g01, g02, g03, g04, g05, g06, g07, g08, g09, g010, g011, ncol=3)
# as.data.frame(data[,c(16,4)]) # total_activity_number
graph9 <- ggplot(data=as.data.frame(data[,c(16,4)]), aes(x=total_activity_number, fill=as.factor(number_of_COVID19_infected)))+
    geom_bar(position='dodge')+
    theme_classic()+
    labs(title='참여한 활동의 종류', x='참여한 활동의 종류', y='사람 수')+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9), labels=c('0','1','2','3','4','5','6','7','8','9'))+
    scale_fill_discrete(labels=c("없음", "있음"), name='COVID19 감염력')
plot_file <- read_pptx() %>%
    add_slide() %>% ph_with(dml(ggobj=graph9), location=ph_location_type(type='body')) %>%
print(plot_file,target='plot_file3.pptx')


# as.data.frame(data[,c(5,4)]) # act_code_1 # merge, table, OR box_plot
# 이 이하는 각각도 그려야 하고, OR(w/ CI)도 box-plot으로 통합해서 그려야 한다.
# OR(w/ CI)는 아마도 각각의 데이터를 따로 받은 다음 하나의 data.frame을 만든 다음에 그려야 할 듯.

calc_CI_odds_ratio <- function(x, conf.level=0.95){
    calc_odds_ratio <- function(x){
        return((x[4,3]*x[1,3])/(x[2,3]*x[3,3]))
    }
    alpha = qnorm(1-(1.0-conf.level)/2)
    lower = exp(log(calc_odds_ratio(x)) - alpha*sqrt(1/x[4,3]+1/x[3,3]+1/x[2,3]+1/x[1,3]))
    upper = exp(log(calc_odds_ratio(x)) + alpha*sqrt(1/x[4,3]+1/x[3,3]+1/x[2,3]+1/x[1,3]))
    return(c(calc_odds_ratio(x), lower, upper))
}
estimated_range <- t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_1)))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_2))))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_3))))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_4))))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_5))))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_6))))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_7))))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_8))))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_9))))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_10))))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_11))))))
rownames(estimated_range) <- c(1:11)
colnames(estimated_range) <- c('estimate', 'lower', 'upper')

p_value <- as.data.frame(oddsratio(t(tbl_act_1), method='wald')$p.value)[2,]
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_2), method='wald')$p.value)[3,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_3), method='wald')$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_4), method='wald')$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_5), method='wald')$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_6), method='wald')$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_7), method='wald')$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_8), method='wald')$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_9), method='wald')$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_10), method='wald')$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_11), method='wald')$p.value)[2,])
rownames(p_value) <- c(1:11)
p_value_fisher.exact <- p_value[[2]]

activity_1to11_CL_95 <- cbind(estimated_range, p_value_fisher.exact)

estimated_range <- t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_1)), conf.level=0.90)))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_2)), conf.level=0.90))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_3)), conf.level=0.90))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_4)), conf.level=0.90))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_5)), conf.level=0.90))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_6)), conf.level=0.90))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_7)), conf.level=0.90))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_8)), conf.level=0.90))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_9)), conf.level=0.90))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_10)), conf.level=0.90))))
estimated_range <- rbind(estimated_range, t(as.data.frame(calc_CI_odds_ratio(as.data.frame(t(tbl_act_11)), conf.level=0.90))))
rownames(estimated_range) <- c(1:11)
colnames(estimated_range) <- c('estimate', 'lower', 'upper')

p_value <- as.data.frame(oddsratio(t(tbl_act_1),conf.level = 0.90)$p.value)[2,]
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_2),conf.level = 0.90)$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_3),conf.level = 0.90)$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_4),conf.level = 0.90)$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_5),conf.level = 0.90)$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_6),conf.level = 0.90)$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_7),conf.level = 0.90)$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_8),conf.level = 0.90)$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_9),conf.level = 0.90)$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_10),conf.level = 0.90)$p.value)[2,])
p_value <- rbind(p_value, as.data.frame(oddsratio(t(tbl_act_11),conf.level = 0.90)$p.value)[2,])
rownames(p_value) <- c(1:11)
p_value_fisher.exact <- p_value[[2]]

activity_1to11_CL_90 <- cbind(estimated_range, p_value_fisher.exact)

activity_1to11_CL_95 <- cbind(as.data.frame(c(1:11)), activity_1to11_CL_95)
colnames(activity_1to11_CL_95) <- c('activity_code', 'estimate', 'lower', 'upper', 'p_value_fisher.exact')
activity_1to11_CL_90 <- cbind(as.data.frame(c(1:11)), activity_1to11_CL_90)
colnames(activity_1to11_CL_90) <- c('activity_code', 'estimate', 'lower', 'upper', 'p_value_fisher.exact')

# 사용하는 데이터가 자유도가 1이고, 데이터 중 5 이하인 것도 있으니 fisher.exact을 해야 함.
# 정규분포만 가정할 수 있다면 숫자의 근사 없이 직접 계산을 수행하는 fisher.exact test로 p.value를 구해야 함.


graph9 <- ggplot(activity_1to11_CL_95, aes(x=activity_code, y=estimate))+
    geom_point(size=3)+
    theme_bw()+
    geom_errorbar(aes(x=activity_code, ymin=lower, ymax=upper), width=0.4, size=1.3)+
    labs(title='참여한 활동에 따른 Odds Ratio(CL 95%)', x='활동 번호', y='Odds Ratio')+
    theme(plot.title=element_text(hjust=0.5))+
    geom_hline(yintercept=1, linetype='dashed')+
    scale_x_continuous(breaks=c(1:11), labels=c('1','2','3','4','5','6','7','8','9','10','11'))+
    coord_flip(ylim=c(0,10))+
    geom_point(data=activity_1to11_CL_95[activity_1to11_CL_95$activity_code==3 | activity_1to11_CL_95$activity_code==4,], aes(x=activity_code, y=estimate), color='red', fill='red', size=3)+
    geom_errorbar(data=activity_1to11_CL_95[activity_1to11_CL_95$activity_code==3 | activity_1to11_CL_95$activity_code==4,], aes(x=activity_code, ymin=lower, ymax=upper), color='red', fill='red', width=0.4, size=1.3)


graph10 <- ggplot(activity_1to11_CL_90, aes(x=activity_code, y=estimate))+
    geom_point(size=3)+
    theme_bw()+
    geom_errorbar(aes(x=activity_code, ymin=lower, ymax=upper), width=0.4, size=1.3)+
    labs(title='참여한 활동에 따른 Odds Ratio(CL 90%)', x='활동 번호', y='Odds Ratio')+
    theme(plot.title=element_text(hjust=0.5))+
    geom_hline(yintercept=1, linetype='dashed')+
    scale_x_continuous(breaks=c(1:11), labels=c('1','2','3','4','5','6','7','8','9','10','11'))+
    coord_flip(ylim=c(0,10))+
    geom_point(data=activity_1to11_CL_90[activity_1to11_CL_90$activity_code==3 | activity_1to11_CL_90$activity_code==4,], aes(x=activity_code, y=estimate), color='red', fill='red', size=3)+
    geom_errorbar(data=activity_1to11_CL_90[activity_1to11_CL_90$activity_code==3 | activity_1to11_CL_90$activity_code==4,], aes(x=activity_code, ymin=lower, ymax=upper), color='red', fill='red', width=0.4, size=1.3)

tbl_school_year = table(COVID19_infection = data$number_of_COVID19_infected, school_year = data$school_year)
tbl_year_of_birth = table(COVID19_infection = data$number_of_COVID19_infected, year_of_birth = data$year_of_birth)
tbl_number_of_club = table(COVID19_infection = data$number_of_COVID19_infected, number_of_club = data$number_of_participated_club)
tbl_total_activity_num = table(COVID19_infection = data$number_of_COVID19_infected, total_activity_number = data$total_activity_number)

tbl_sum_school_year = array(tbl_school_year, dim(tbl_school_year) + c(0,1))
tbl_sum_school_year[1,length(tbl_sum_school_year)/2] = sum(tbl_school_year[1,])
tbl_sum_school_year[2,length(tbl_sum_school_year)/2] = sum(tbl_school_year[2,])
tbl_sum_school_year <- cbind(tbl_sum_school_year[,length(tbl_sum_school_year)/2], tbl_sum_school_year[,-length(tbl_sum_school_year)/2])
Oddsratio_sum_school_year <- cbind(oddsratio(t(tbl_sum_school_year))$measure, oddsratio(t(tbl_sum_school_year))$p.value[,2])
colnames(Oddsratio_sum_school_year) <- c('estimate', 'lower', 'upper', 'p_value_fisher.exact')
rownames(Oddsratio_sum_school_year) <- c('1','2','3','4','5')
Oddsratio_sum_school_year <- cbind(rownames(Oddsratio_sum_school_year), as.data.frame(Oddsratio_sum_school_year))
colnames(Oddsratio_sum_school_year) <- c('name','estimate', 'lower', 'upper', 'p_value_fisher.exact')
 
tbl_sum_year_of_birth = array(tbl_year_of_birth, dim(tbl_year_of_birth) + c(0,1))
tbl_sum_year_of_birth[1,length(tbl_sum_year_of_birth)/2] = sum(tbl_year_of_birth[1,])
tbl_sum_year_of_birth[2,length(tbl_sum_year_of_birth)/2] = sum(tbl_year_of_birth[2,])
tbl_sum_year_of_birth <- cbind(tbl_sum_year_of_birth[,length(tbl_sum_year_of_birth)/2], tbl_sum_year_of_birth[,-length(tbl_sum_year_of_birth)/2])
Oddsratio_sum_year_of_birth <- cbind(oddsratio(t(tbl_sum_year_of_birth[,c(1,4,5,6,7,8,9,10)]))$measure, oddsratio(t(tbl_sum_year_of_birth[,c(1,4,5,6,7,8,9,10)]))$p.value[,2])
colnames(Oddsratio_sum_year_of_birth) <- c('estimate', 'lower', 'upper', 'p_value_fisher.exact')
rownames(Oddsratio_sum_year_of_birth) <- c('합계','1997','1998','1999','2000','2001','2002','2003')

tbl_sum_number_of_club = array(tbl_number_of_club, dim(tbl_number_of_club) + c(0,1))
tbl_sum_number_of_club[1,length(tbl_sum_number_of_club)/2] = sum(tbl_number_of_club[1,])
tbl_sum_number_of_club[2,length(tbl_sum_number_of_club)/2] = sum(tbl_number_of_club[2,])
tbl_sum_number_of_club <- cbind(tbl_sum_number_of_club[,length(tbl_sum_number_of_club)/2], tbl_sum_number_of_club[,-length(tbl_sum_number_of_club)/2])
Oddsratio_sum_number_of_club <- cbind(oddsratio(t(tbl_sum_number_of_club))$measure, oddsratio(t(tbl_sum_number_of_club))$p.value[,2])
colnames(Oddsratio_sum_number_of_club) <- c('estimate', 'lower', 'upper', 'p_value_fisher.exact')
rownames(Oddsratio_sum_number_of_club) <- c('합계','0','1','2','3','4','5+')


tbl_sum_total_activity_num = array(tbl_total_activity_num, dim(tbl_total_activity_num) + c(0,1))
tbl_sum_total_activity_num[1,length(tbl_sum_total_activity_num)/2] = sum(tbl_total_activity_num[1,])
tbl_sum_total_activity_num[2,length(tbl_sum_total_activity_num)/2] = sum(tbl_total_activity_num[2,])
tbl_sum_total_activity_num <- cbind(tbl_sum_total_activity_num[,length(tbl_sum_total_activity_num)/2], tbl_sum_total_activity_num[,-length(tbl_sum_total_activity_num)/2])
Oddsratio_sum_total_activity_num <- cbind(oddsratio(t(tbl_sum_total_activity_num[,c(1,2,4,5,6,7,8)]))$measure, oddsratio(t(tbl_sum_total_activity_num[,c(1,2,4,5,6,7,8)]))$p.value[,2])
colnames(Oddsratio_sum_total_activity_num) <- c('estimate', 'lower', 'upper', 'p_value_fisher.exact')
rownames(Oddsratio_sum_total_activity_num) <- c('합계','0','2','3','4','5','6')

# data
# 1은 yes, 0은 no
cat("\n",
    "=====================================",'\n',
    '          [ data table ]             ','\n',
    "=====================================",'\n',
    file='summary_data.txt', append=FALSE)
cat('\n\n',
    '분석 방법에 따른 분류','\n',
    '[1], [2], [3], [4]는 독립변수가 이산(0 혹은 자연수. 하나하나 셀 수 있음)변수이고.','\n',
    '종속변수는 코로나에 걸린적이 있냐라는 이항(0 또는 1)변수이다.','\n',
    '따라서 이 항목들은 로지스틱 회귀라는 방식을 사용하여 분석한다.','\n','\n',
    '[5]~[15]의 경우 독립변수가 위험 요인에 노출되었는지의 유무인 이항변수(0, 1)이고','\n',
    '종속변수는 코로나에 걸린적이 있냐라는 이항변수(0, 1)이다.','\n',
    '따라서 이 항목들은 표를 만들어 Odds ratio를 구해 분석한다.','\n',
    '[16]은 school_year, year_of_birth, number_of_club, total_activity_num, activity1~11 변수의 전체적인 통계치를 요약한 표이다.','\n',
    '[17]은 로지스틱 회귀분석한 결과이다.','\n',
    '[18]과 [19]는 activity1~11 각각의 odds ratio를 구한 후, 그 confidence interval을 구해 요약한 표이다.','\n','\n',
    file='summary_data.txt', append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[1] distribution of school_year','\n',
    'school_year : 2022년 기준 학년. 1은 예과 1학년, 2는 예과 2학년, 3은 본과 1학년, 4는 본과 2학년.','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_school_year, file="summary_data.txt", append=TRUE)

cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[2] distribution of year_of_birth','\n',
    'year_of_birth : 태어난 년도.','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_year_of_birth, file='summary_data.txt',append=TRUE)

cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[3] distribution of number_of_club','\n',
    'number_of_club : 참여하고 있는 동아리+동문회 수.','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_number_of_club, file='summary_data.txt',append=TRUE)

cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[4] distribution of total_activity_num','\n',
    'total_activity_num : 총 참여한 동아리 활동 종류의 수. ##5는 5개 이상을 의미.##','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_total_activity_num, file='summary_data.txt',append=TRUE)

cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[5] distribution of activity_1','\n',
    'activity_code_1 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_1 : 활동 내용 : 식사 (동아리원과 함께한 모든 식사자리)','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_1, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[6] distribution of activity_2','\n',
    'activity_code_2 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_2 : 활동 내용 : 술자리 (소규모 포함)','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_2, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[7] distribution of activity_3','\n',
    'activity_code_3 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_3 : 활동 내용 : MT','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_3, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[8] distribution of activity_4','\n',
    'activity_code_4 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_4 : 활동 내용 : 단체 공연 연습 (합주, 합숙)','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_4, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[9] distribution of activity_5','\n',
    'activity_code_5 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_5 : 활동 내용 : 전시회 준비 (축제 준비를 위한 전시회 준비)','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_5, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[10] distribution of activity_6','\n',
    'activity_code_6 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_6 : 활동 내용 : 야외 운동 (축구, 농구, 야구, 요트 등)','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_6, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[11] distribution of activity_7','\n',
    'activity_code_7 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_7 : 활동 내용 : 기타 야외 활동 (출사, 축제부스, 야외봉사활동 등)','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_7, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[12] distribution of activity_8','\n',
    'activity_code_8 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_8 : 활동 내용 : 실내 운동 (볼링, 검도 등)','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_8, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[13] distribution of activity_9','\n',
    'activity_code_9 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_9 : 활동 내용 : 기타 실내활동 (요리, 보드게임, 그림그리기, 실내봉사활동 등)','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_9, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[14] distribution of activity_10','\n',
    'activity_code_10 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_10 : 활동 내용 : 비대면 활동 (종교활동 등)','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_10, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[15] distribution of activity_11','\n',
    'activity_code_11 : 활동 참여 유무. 0은 참여 안함, 1은 참여 함.','\n',
    'activity_code_11 : 활동 내용 : 동아리 활동을 하지 않음','\n',
    'COVID19_infection : 0은 감염력 없음, 1은 감염력 있음.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(tbl_act_11, file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[16] summary of raw data','\n','\n',
    file='summary_data.txt',append=TRUE)
capture.output( summary(data, sep='\t'), file='summary_data.txt',append=TRUE)
# ------------------- #
# 여기서부터 로지스틱 회귀 분석 진행
set.seed(1000)
# [1] school_year
data1 <- data[,c(3, 7, 8, 11, 12, 15, 16, 4)]
# 1 AUC .438
# 2 AUC .458
# 3 AUC .662
# 4 AUC 1 # itself
# 5 AUC .409 # 1
# 6 AUC .555
# 7 AUC .847
# 8 AUC .766
# 9 AUC .516
# 10 AUC .464 # 6
# 11 AUC .669
# 12 AUC .555
# 13 AUC .403
# 14 AUC .464
# 15 AUC .591 # 11
# 16 AUC .860
# data1 <- data
index <- sample(1:nrow(data1), size=nrow(data1)*0.7)
training <- data1[index,]
test <- data1[-index,]
d1 <- glm(number_of_COVID19_infected ~ ., data=training, family=binomial)
summary(d1)
# d2 <- step(d1, direction='forward')
# print("summary of d2\n")
# summary(d2)
library(plyr)
predict2 <- predict(d1, newdata = test, type = "response")
head(sort(predict2, decreasing = TRUE),n=10)
head(arrange(data.frame(predict2,test$number_of_COVID19_infected),desc(predict2)),n=10)
predict3 <- predict(d1, newdata = test,type = "response")
roc <- ROC(predict3,test$number_of_COVID19_infected, plot="ROC", AUC=T, main="logistic regression")

cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[17] logistic regression','\n','\n',
    '사용한 변수','\n',
    'c(3, 7, 8, 11, 12, 15, 16, 4)','\n',
    '## [3] number_of_club ','\n',
    '## [7] activity_code_3','\n',
    '## [8] activity_code_4','\n',
    '## [11] activity_code_7','\n',
    '## [12] activity_code_8','\n',
    '## [15] activity_code_11','\n',
    '## [16] total_activity_number','\n',
    '## 종속변수 : [4] number_of_COVID19_infected','\n',
    '데이터를 7:3으로 나누어 70%는 logistic regression에 이용하는 training set으로, 30%는 만들어진 모델을 테스트할 test set으로 지정했음.','\n',
    file='summary_data.txt',append=TRUE)
cat('\n',
    'logistic regression 모델 요약','\n',
    file='summary_data.txt',append=TRUE)
capture.output(summary(d1), file='summary_data.txt',append=TRUE)
cat('\n',
    'test set에 최종 모델을 적용하여 확률 기준 top 10명을 추출했음.','\n',
    file='summary_data.txt',append=TRUE)
capture.output(head(sort(predict2, decreasing = TRUE),n=10), file='summary_data.txt',append=TRUE)
cat('\n',
    '실제로 코로나에 걸렸던 적이 있는지 확인해보았음.','\n',
    file='summary_data.txt',append=TRUE)
capture.output(head(arrange(data.frame(predict2,test$number_of_COVID19_infected),desc(predict2)),n=10), file='summary_data.txt',append=TRUE)
cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[18] Odds ratio of 11 activities with p-value, conf.level=0.90','\n',
    'activity_code : 활동 코드. 1~11.','\n',
    'estimate : Odds ratio 값.','\n',
    'lower : Odds ratio 범위 추정 하한값','\n',
    'upper : Odds ratio 범위 추정 상한값','\n',
    'p_value_fisher.exact : p-value. 방법은 fisher.exact.','\n',
    '사용하는 데이터가 자유도가 1이고, 데이터 중 5 이하인 것도 있으니 fisher.exact을 해야 함.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(activity_1to11_CL_90, file='summary_data.txt',append=TRUE)

cat("\n",
    '------------------------------------------------------------------------------------','\n',
    '[19] Odds ratio of 11 activities with p-value, conf.level=0.95','\n',
    'activity_code : 활동 코드. 1~11.','\n',
    'estimate : Odds ratio 값.','\n',
    'lower : Odds ratio 범위 추정 하한값','\n',
    'upper : Odds ratio 범위 추정 상한값','\n',
    'p_value_fisher.exact : p-value. 방법은 fisher.exact.','\n',
    '사용하는 데이터가 자유도가 1이고, 데이터 중 5 이하인 것도 있으니 fisher.exact을 해야 함.','\n','\n',
    file='summary_data.txt', append=TRUE)
capture.output(activity_1to11_CL_95, file='summary_data.txt',append=TRUE)

oddsratio_data_1 <- c(strsplit("1	2.117647059	0.668491461	6.708281751	0.2727",split='\t'),
                      strsplit("2	1.138047138	0.421505514	3.072679352	1",split='\t'),
                      strsplit("3	0.532407407	0.19803717	1.431335583	0.2207",split='\t'),
                      strsplit("4	0.907936508	0.328493115	2.509485483	1",split='\t'))
oddsratio_data_1 <- as.data.frame(t(as.data.frame(oddsratio_data_1)))
colnames(oddsratio_data_1) <- c('num','estimate', 'lower', 'upper', 'p.value')
rownames(oddsratio_data_1) <- c('예과1학년','예과2학년','본과1학년','본과2학년')
oddsratio_data_1$estimate <- as.double(oddsratio_data_1$estimate)
oddsratio_data_1$num <- as.double(oddsratio_data_1$num)
oddsratio_data_1$lower <- as.double(oddsratio_data_1$lower)
oddsratio_data_1$upper <- as.double(oddsratio_data_1$upper)

a1 <- ggplot(oddsratio_data_1, aes(x=num, y=estimate))+
    geom_point(size=3)+
    labs(title='학년에 따른 Odds ratio', x='학년', y='Odds Ratio')+
    theme_bw()+
    geom_errorbar(aes(x=num, ymin=lower, ymax=upper), width=0.4, size=1.3)+
    theme(plot.title=element_text(hjust=0.5))+
    #scale_x_discrete(breaks=c(1, 2, 3, 4), label=c('예과1학년','예과2학년','본과1학년','본과2학년'))+
    #scale_y_discrete(breaks=c(0,1,2,3,4,5,6,7), label=c('0','1','2','3','4','5','6','7'))+
    geom_hline(yintercept=1, linetype='dashed')+
    coord_flip()

oddsratio_data_2 <- c(strsplit("1997	0.237037037	0.023572183	2.383595863	0.3105",split='\t'),
                      strsplit("1998	1.6	0.370905898	6.902020194	0.7249",split='\t'),
                      strsplit("1999	0.46031746	0.119239965	1.777023029	0.3155",split='\t'),
                      strsplit("2000	2.571428571	0.928964406	7.117866793	0.08965",split='\t'),
                      strsplit("2001	0.710526316	0.237184572	2.128501194	0.5825",split='\t'),
                      strsplit("2002	0.540697674	0.112874107	2.590088939	0.4583",split='\t'),
                      strsplit("2003	1.300813008	0.289035728	5.854343671	1",split='\t'))
oddsratio_data_2 <- as.data.frame(t(as.data.frame(oddsratio_data_2)))
colnames(oddsratio_data_2) <- c('태어난년도','estimate', 'lower', 'upper', 'p.value')
rownames(oddsratio_data_2) <- c('1997','1998','1999','2000','2001','2002','2003')
oddsratio_data_2$estimate <- as.double(oddsratio_data_2$estimate)
oddsratio_data_2$lower <- as.double(oddsratio_data_2$lower)
oddsratio_data_2$upper <- as.double(oddsratio_data_2$upper)
oddsratio_data_2$태어난년도 <- as.numeric(oddsratio_data_2$태어난년도)

a2 <- ggplot(oddsratio_data_2, aes(x=태어난년도, y=estimate))+
    geom_point(size=3)+
    labs(title='태어난 년도에 따른 Odds ratio', x='태어난 년도', y='Odds Ratio')+
    theme_bw()+
    geom_errorbar(aes(x=태어난년도, ymin=lower, ymax=upper), width=0.4, size=1.3)+
    theme(plot.title=element_text(hjust=0.5))+
    #scale_x_discrete(breaks=c(1, 2, 3, 4), label=c('예과1학년','예과2학년','본과1학년','본과2학년'))+
    #scale_y_discrete(breaks=c(0,1,2,3,4,5,6,7), label=c('0','1','2','3','4','5','6','7'))+
    geom_hline(yintercept=1, linetype='dashed')+
    coord_flip()

oddsratio_data_3 <- c(strsplit("0	0.172222222	0.018360364	1.615463248	0.1602",split='\t'),
                      strsplit("1	1.01754386	0.317830721	3.257694865	1",split='\t'),
                      strsplit("2	0.605769231	0.196301581	1.869349998	0.4025",split='\t'),
                      strsplit("3	1.575757576	0.552622289	4.493144748	0.4449",split='\t'),
                      strsplit("4	2.962962963	0.74892846	11.72228055	0.1355",split='\t'),
                      strsplit("5+	0.717948718	0.226265916	2.278073384	0.7677",split='\t'))
oddsratio_data_3 <- as.data.frame(t(as.data.frame(oddsratio_data_3)))
colnames(oddsratio_data_3) <- c('num','estimate', 'lower', 'upper', 'p.value')
rownames(oddsratio_data_3) <- c('0','1','2','3','4','5+')
oddsratio_data_3$estimate <- as.double(oddsratio_data_3$estimate)
oddsratio_data_3$lower <- as.double(oddsratio_data_3$lower)
oddsratio_data_3$upper <- as.double(oddsratio_data_3$upper)

a3 <- ggplot(oddsratio_data_3, aes(x=num, y=estimate))+
    geom_point(size=3)+
    labs(title='동아리+동문회 참여한 수에 따른 Odds ratio', x='동아리+동문회 참여한 수', y='Odds Ratio')+
    theme_bw()+
    geom_errorbar(aes(x=num, ymin=lower, ymax=upper), width=0.4, size=1.3)+
    theme(plot.title=element_text(hjust=0.5))+
    # scale_x_discrete(breaks=c(1, 2, 3, 4), label=c('예과1학년','예과2학년','본과1학년','본과2학년'))+
    # scale_y_discrete(breaks=c(0,1,2,3,4,5,6,7), label=c('0','1','2','3','4','5','6','7'))+
    geom_hline(yintercept=1, linetype='dashed')+
    coord_flip()

oddsratio_data_4 <- c(strsplit("0	0.172222222	0.018360364	1.615463248	0.1602",split='\t'),
                      strsplit("2	0.201550388	0.049978677	0.812797799	0.02528",split='\t'),
                      strsplit("3	1.01754386	0.317830721	3.257694865	1",split='\t'),
                      strsplit("4	1.935483871	0.689264932	5.434917175	0.3132",split='\t'),
                      strsplit("5	1.6	0.370905898	6.902020194	0.7249",split='\t'),
                      strsplit("6	0.731707317	0.19428716	2.75569213	0.7389",split='\t'))
oddsratio_data_4 <- as.data.frame(t(as.data.frame(oddsratio_data_4)))
colnames(oddsratio_data_4) <- c('num','estimate', 'lower', 'upper', 'p.value')
rownames(oddsratio_data_4) <- c('0','2','3','4','5','6')
oddsratio_data_4$estimate <- as.double(oddsratio_data_4$estimate)
oddsratio_data_4$lower <- as.double(oddsratio_data_4$lower)
oddsratio_data_4$upper <- as.double(oddsratio_data_4$upper)

a4 <- ggplot(oddsratio_data_4, aes(x=num, y=estimate))+
    geom_point(size=3)+
    labs(title='참여한 동아리 활동 유형 개수별 참여자 수에 따른 Odds ratio', x='참여한 동아리 활동 유형 개수별 참여자 수', y='Odds Ratio')+
    theme_bw()+
    geom_errorbar(aes(x=num, ymin=lower, ymax=upper), width=0.4, size=1.3)+
    theme(plot.title=element_text(hjust=0.5))+
    # scale_x_discrete(breaks=c(1, 2, 3, 4), label=c('예과1학년','예과2학년','본과1학년','본과2학년'))+
    # scale_y_discrete(breaks=c(0,1,2,3,4,5,6,7), label=c('0','1','2','3','4','5','6','7'))+
    geom_hline(yintercept=1, linetype='dashed')+
    coord_flip()


plot_file <- read_pptx() %>%
        add_slide() %>% ph_with(dml(ggobj=graph1), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(ggobj=graph2), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(ggobj=graph3), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(ggobj=graph4), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, ncol=4)), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, ncol=3)), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(ggobj=graph5), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(ggobj=graph6), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(ggobj=graph7), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(ggobj=graph8), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(grid.arrange(g01, g02, g03, g04, g05, g06, g07, g08, g09, g010, g011, ncol=4)), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(grid.arrange(g01, g02, g03, g04, g05, g06, g07, g08, g09, g010, g011, ncol=3)), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(ggobj=graph9), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(ggobj=graph10), location=ph_location_type(type='body')) %>%
        add_slide() %>% ph_with(dml(pairs.panels(data)), location=ph_location_type(type='body')) %>%
print(plot_file, target='plot_file.pptx')

plot_file <- read_pptx() %>%
    add_slide() %>% ph_with(dml(ggobj=a1), location=ph_location_type(type='body')) %>%
    add_slide() %>% ph_with(dml(ggobj=a2), location=ph_location_type(type='body')) %>%
    add_slide() %>% ph_with(dml(ggobj=a3), location=ph_location_type(type='body')) %>%
    add_slide() %>% ph_with(dml(ggobj=a4), location=ph_location_type(type='body')) %>%
print(plot_file,target='plot_file2.pptx')





