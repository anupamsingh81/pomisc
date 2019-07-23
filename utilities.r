#fxn2

library(glue)
library(ggstatsplot)
library(dplyr)
library(ggplot2)
library(readr)
library(purrrr)
library(tibble)
library(tidyr)
library(insight)
library(rstatix)
library(broom)
library(jtools)
library(sjPlot)
library(rms)
library(afex)
library(emmeans)
library(pROC)
library(cutpointr)
library(finalfit)
library(kableExtra)
library(rlang)

########T-test###############

library(ggstatsplot)

customboxplot= function(df,x,y){ x1=enquo(x);y1=enquo(y); #x2=quo_name(x1);y2=quo_name(y1);
df %>% ggbetweenstats(x=!!x1,y=!!y1,plot.type = "box",messages = FALSE,results.subtitle = FALSE)}

####customboxplot(mtcars,y=wt,x=am)

customboxdescription =function(df,x,y){
  x1=enquo(x);y1=enquo(y);y2=quo_name(y1);x2=quo_name(x1);
  level1=df%>%distinct(!!y1)%>%pull(!!y1)%>%paste(.,collapse=",")%>%str_replace(.,",(?=[^,]*$)"," and ")
  num=df%>%distinct(!!y1) %>% nrow()
  description=paste0("In this Figure we see Box plot of ",x2," in ",num," sub-groups of " ,y2," : ",level1," respectively .The individual jittered data points of ", x2, " are overlaid over transparent Boxplot for better visualisation.
We see distribution of data in individual "," sub-groups of " ,y2," based on these box-plots. The lower edge of box plot represents -first quartile (Q1), Horizontal bar represents the median, Upper edge represnts third quartile (Q3), Two black lines (whiskers) emanating from box-plots signify range of non-outlier data for the particular sub-group. Lower whisker represents minimum(Q1- 1.5 *interquartile range) non-outlier limit of ",x2," and upper whisker represnts maximum(Q1+1.5*interquartile range) of ",x2," .Any data beyond whiskers of box-plots represents outliers in the sub-groups
The big brown point in the box-plots represents mean " ,x2," of ",num," groups and it has been annotated in the figure itself
We can see the statistical summary of Test while summary statistics of Effect size and its confidence Interval (represented as Hedges's G ) at top of plot. Summary Statistics of the groups is presented in table below")
  
  description %>% cat()
  
  
}

ttable=function(df,x,y){x1=enquo(x);y1=enquo(y);

df %>% group_by(!!y1) %>% get_summary_stats(!!x1) %>% select(Group :=!!y1, n,Mean=mean,SD=sd,Median=median,Minimum=min,Maximum=max)}

####ttable(mtcars,wt,am)


ttable2=function(df,a){a1=enquo(a);a2=quo_name(a1);df%>%filter(variable==!!a2) %>% select(`Group 1` =group1,`Group 2`=group2,
                                                                                          `Degree of Freedom`=parameter,
                                                                                          `T statistic`=statistic,
                                                                                          Difference=difference,
                                                                                          `95 % C.I.` =`95% Confidence Interval`,
                                                                                          `P value`=p.value)}

######ttable2(groupt,wt)

#All -t
tgroup=function(df,x){
  x1=enquo(x)
  x2= quo_name(x1)
  df <- df %>% mutate(!!x1 :=as.factor(!!x1))
  df %>% select_if(is.numeric) %>% names() ->varname
  
  # print(x1)
  # print(x2)
  level1=df%>%pull(!!x1) %>% levels()
  
  t1=df %>% select(one_of(varname),!!x1) %>%
    gather(variable,value,-!!x1) %>%
    group_by(variable) %>%
    summarise(ttest = list(t.test(as.formula(paste("value",x2,sep="~"))))) %>%
    mutate(ttest = map(ttest, tidy)) %>%
    unnest() %>%
    rename(difference=estimate,mean1=estimate1,mean2=estimate2) %>%
    select(variable,mean1,mean2,difference,p.value,conf.low,conf.high,parameter,statistic) %>% mutate_if(is.numeric,round,2)
  
  t2=df %>%select(one_of(varname),!!x1) %>%
    group_by(!!x1) %>% summarise_all(list(sd)) %>% mutate_if(is.numeric,round,2) %>% select(-!!x1) %>%
    t() %>% as_tibble() %>% add_column(variable=varname) %>%
    rename(sd1=V1,sd2=V2) %>% left_join(t1) %>%
    mutate(group1=paste(mean1,"±",sd1,sep=" "),
           group2=paste(mean2,"±",sd2,sep=" "),
           `95% Confidence Interval` = paste(conf.low," - ",conf.high,sep=" ")) %>%
    select(variable,group1,group2,parameter,statistic,difference,`95% Confidence Interval`,p.value) %>%
    mutate(p.value=ifelse(p.value==0,"<0.001",p.value)) %>%
    mutate(significance=ifelse(p.value<0.05,"significant","non-significant")) %>%add_column(name1=level1[1],name2=level1[2])
  
  t2%>%filter(!variable==!!x2)
}


tdescr=function(df,a){a1=enquo(a);a2=quo_name(a1);df%>%filter(variable==!!a2)%>%mutate(direction=ifelse(difference>0,"higher","lower")) %>%mutate(description=glue("The mean in Group {name1} [ {group1} ] was {significance}ly {direction} than Group {name2} [ {group2} ] . The mean difference was {difference} and 95 % confidence interval of the difference was ( {`95% Confidence Interval`} ) . The p value was {p.value} . The t statistic was {statistic} and degree of freedom of the Welch unpaired two-sample t test was {parameter} .In Formal statistical notation this result is expressed as : t({parameter}) = {statistic}, p= {p.value}."))%>%pull(description)}

tplotbarse = function(df,x,y){x1=enquo(x);y1=enquo(y);y2=quo_name(y1);df%>%mutate(!!y1 :=as.factor(!!y1))%>%mutate(!!y1 :=fct_reorder(!!y1,!!x1))%>%ggplot(aes(x=!!y1,y=!!x1,fill=!!y1))+stat_summary(fun.y = mean, geom = "bar")+stat_summary(fun.data = mean_se, geom = "errorbar",width=0.2)+guides(fill=FALSE)}

####Workflow___________________

#customboxplot(mtcars,y=wt,x=am)
#customboxdescription(mtcars,wt,cyl)
#ttable(mtcars,wt,cyl)

#groupt=tgroup(mtcars,am)
#tdescr(groupt,wt) # for wt~am in mtcars

#ttable2(groupt,wt)  # for wt~am in mtcars




## CHI-SQUARE###############

kyeplot =function(df,x,y){
  x1=enquo(x)
  y1=enquo(y)
  
  x2=quo_name(x1)
  y2=quo_name(y1)
  
  # print(x2)
  
  
  x3=df %>% select(!!x1) %>% pull()
  y3=df %>% select(!!y1) %>% pull()
  
  sjp.grpfrq(var.cnt=y3,var.grp=x3,show.prc=FALSE,show.grpcnt = TRUE)+labs(y="Count")+theme_light()+
    labs(x=y2,fill=x2)}

piechart=function(df,x,y){x1=sym(x);y1=sym(y);level1=df%>%distinct(!!x1)%>%pull(!!x1)%>%paste(.,collapse=",")%>%str_replace(.,",(?=[^,]*$)"," and ")
;level2=df%>%distinct(!!y1)%>%pull(!!y1)%>%paste(.,collapse=",")%>%str_replace(.,",(?=[^,]*$)"," and ");
state=glue("The Dodged bar chart above represents individual counts representing frequency of {x} categories {level1} in categories {level2} belonging to group {y}.");state}

kye = function (df,x,y){
  x1=sym(x);x2=enquo(x1);y1=sym(y);y2=enquo(y1);
  
  init2=piechart (df,x,y);
  
  df%>%group_by(!!y1,!!x1)%>%summarise(n=n())%>%mutate(freq=round(100*n/sum(n),2),total=sum(n),express=paste(n,total,sep="/"),express1=paste(express,"(",freq, "% )",sep=" "))%>%arrange(!!y1,desc(freq))%>%filter(row_number()==1)%>%mutate(description=paste(" Subgroup ",!!x1,"has highest percentage",express1," in group",!!y1,"."))%>%ungroup()%>%add_row(description=init2,.before=0)%>%add_row(description="To formally check for association between groups we performed pearson chi-square test .",.after=length(.))%>%pull(description) %>% cat()}


kyeplot2= function(df,x,y){
  x1=enquo(x);
  y1=enquo(y);
  df%>%mutate(!!x1 :=as.factor(!!x1),!!y1 :=as.factor(!!y1))%>%group_by(!!y1,!!x1)%>%summarise(n=n())%>%mutate(total=sum(n))%>%ungroup()%>%mutate(p=n/total,low=qbeta(0.025,n+0.5,total-n+0.5),high=qbeta(0.975,n+0.5,total-n+0.5),se=sqrt(p*(1-p)/n))%>%ggplot(aes(y=p,x=!!y1,color=!!x1,group=!!x1))+geom_point()+geom_line()+geom_errorbar(aes(ymin=p-se,ymax=p+se),width=0,position=position_dodge(width=0.05)
  )+scale_y_continuous(labels = percent_format())}



pieplot = function(df,x,y){
  x1=enquo(x)
  y1=enquo(y)
  
  library(ggstatsplot)
  grouped_ggpiestats(data=df,main=!!x1,grouping.var = !!y1,slice.label = "both",messages = FALSE,facet.proptest = FALSE,
                     results.subtitle = TRUE,title.prefix = "")
}

chisq_test = function(df,x,y){x=enquo(x);y=enquo(y);df%>%count(!!x,!!y)%>%spread(!!y,n)%>%select(2:length(.))%>% mutate_if(is.numeric,~replace(., is.na(.), 0)) %>% 
  chisq.test()%>%tidy()}

kyedescr= function(df,x,y){
  x=enquo(x)
  y=enquo(y)
  x1=quo_name(x)
  y1=quo_name(y)
  chisq_test(df,!!x,!!y)%>% mutate(significance=ifelse(p.value<0.05,"Significant","Non-significant")) %>%
    mutate_if(is.numeric,round,2) %>%
    mutate(p.value=ifelse(p.value==0,"<0.001",p.value)) %>%
    mutate(description=paste0("we found a ",significance," association between ",!!x1," and ",!!y1,
                              ". The chi-square statistic was ",statistic," . The degree of freedom was ",parameter,
                              " and P value was ",p.value," .Contingency and Proportion table are shown below")) %>% pull(description) %>% cat()}

#kyedescr(mtcars1,cyl,am)

kyecontingencytable=function(df,x,y){x=enquo(x);y=enquo(y);df%>%count(!!x,!!y)%>%spread(!!y,n) %>% rename(` `=1) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) }

#kyecontingencytable(mtcars1,am,cyl)

kyeproptable = function (df,x,y){
  x1=enquo(x);
  y1=enquo(y);
  
  
  df%>%group_by(!!y1,!!x1)%>%summarise(n=n())%>%mutate(freq=round(100*n/sum(n),2),total=sum(n),express=paste(n,total,sep="/"),value=paste(express,"(",freq,"%)",sep=" "))%>%
    ungroup()%>% mutate(low=qbeta(0.025,n+0.5,total-n+0.5),high=qbeta(0.975,n+0.5,total-n+0.5), `95 % Confidence Interval ` = paste0(round(100*low,2),"% - ",round(100*high,2),"%"))%>%
    
    arrange(!!y1) %>%
    select(-total,-express,-low,-high,-freq)
}

#kyeproptable(mtcars1,am,cyl)

#########W.flow chi##
#kyeplot(mtcars1,am,cyl)

#kye(mtcars1,"am","cyl") #uses quotes

#kyedescr(mtcars1,am,cyl)
#kyecontingencytable(mtcars1,am,cyl)
#kyeproptable(mtcars1,am,cyl)



########ANOVA############################################################


anovaboxplot= function(df,x,y){ x1=enquo(x);y1=enquo(y);
df %>% ggbetweenstats(x=!!x1,y=!!y1,plot.type = "box",messages = FALSE,
                      ggplot.component = list( # adding new components to `ggstatsplot` default
                        ggplot2::labs(caption = " ",subtitle = " ")))}



anovaboxdescr= function(df,x,y){
  x1=enquo(x);y1=enquo(y);y2=quo_name(y1);x2=quo_name(x1);
  level1=df%>%distinct(!!y1)%>%pull(!!y1)%>%paste(.,collapse=",")%>%str_replace(.,",(?=[^,]*$)"," and ")
  num=df%>%distinct(!!y1) %>% nrow()
  description=paste0("In this Figure we see Box plot of ",x2," in ",num," sub-groups of " ,y2," : ",level1," respectively .The individual jittered data points of ", x2, " are overlaid over transparent Boxplot for better visualisation.
We see  distribution of data in individual "," sub-groups of " ,y2," based on these box-plots. The lower edge of box plot represents -first quartile (Q1), Horizontal bar represents the median, Upper edge represnts third quartile (Q3), Two black lines (whiskers) emanating from box-plots signify range of non-outlier data for the particular sub-group. Lower whisker represents minimum(Q1- 1.5 *interquartile range) non-outlier limit of ",x2," and upper whisker represnts maximum(Q1+1.5*interquartile range) of ",x2," .Any data beyond whiskers of box-plots represents outliers in the sub-groups
The big brown point in the box-plots   represents mean " ,x2," of ",num," groups and it has been annotated in the figure itself
Summary Statistics of the groups is presented in table below")
  
  description %>% cat()}
  
  
  anovatable= function(df,x,y){x1=enquo(x);y1=enquo(y);
  
  df %>% group_by(!!y1) %>% get_summary_stats(!!x1) %>% select(Group :=!!y1, n,Mean=mean,SD=sd,Median=median,Minimum=min,Maximum=max)}
  
  
  tukeydescr =function(df,x,y){
    x1=enquo(x)
    y1=enquo(y)
    x2=quo_name(x1)
    y2=quo_name(y1)
    df = df %>% mutate(!!y1 :=as.factor(!!y1))
    
    
    TukeyHSD(aov(as.formula(paste(x2,y2,sep="~")),data=df)) %>% tidy() %>% separate(comparison,into = c("Group 1","Group 2"),sep="-",remove=FALSE) %>%
      rename(difference=estimate) %>%mutate_if(is.numeric,round,2) %>%
      mutate(`95% Confidence Interval` = paste(conf.low," - ",conf.high,sep=" ")) %>%
      
      mutate(significance=ifelse(adj.p.value<0.05,"significant","non-significant")) %>%
      mutate(adj.p.value=ifelse(adj.p.value==0,"<0.001",adj.p.value)) %>% select(-term) %>%
      mutate(direction=ifelse(difference>0,"higher","lower")) %>%
      mutate(description=glue("The mean {x2} in Group {`Group 1`} was {significance}ly {direction} than Group {`Group 2`} . The difference was {difference} and 95 % confidence interval was ( {`95% Confidence Interval`} ) . The adjusted p value was {adj.p.value} .")) %>%
      add_row(description=glue("Since Overall One-Way ANOVA was signifcant indicating an overall difference in groups, we undertook {nrow(.)} unpaired t-test to look for inter-group differences"),.before=0)%>%
      add_row(description=glue("Table describing these tests with Tukey's Post-Hoc correction is described below"),.after=nrow(.)) %>%
      pull(description) %>% cat()
  }
  
  tukeytable =function(df,x,y){
    x1=enquo(x)
    y1=enquo(y)
    x2=quo_name(x1)
    y2=quo_name(y1)
    df = df %>% mutate(!!y1 :=as.factor(!!y1))
    
    
    TukeyHSD(aov(as.formula(paste(x2,y2,sep="~")),data=df)) %>% tidy() %>% separate(comparison,into = c("Group 1","Group 2"),sep="-",remove=FALSE) %>%
      rename(difference=estimate) %>%mutate_if(is.numeric,round,2) %>%
      mutate(`95% Confidence Interval` = paste(conf.low," - ",conf.high,sep=" ")) %>%
      
      mutate(significance=ifelse(adj.p.value<0.05,"Significant","Non-significant")) %>%
      mutate(adj.p.value=ifelse(adj.p.value==0,"<0.001",adj.p.value)) %>% select(-term) %>%
      mutate(Comparison=glue("[ {`Group 1`} ] - [ {`Group 2`} ]")) %>% select(-comparison,-`Group 1`,-`Group 2`,-conf.low,-conf.high) %>%
      rename(Difference=difference,`P value`=adj.p.value,Significance=significance) %>%
      select(Comparison,Difference,`95% Confidence Interval`,`P value`,Significance)
  }
  
  anovadisc= function(df,x,y){ x=enquo(x);
  y=enquo(y);
  x1=quo_name(x);
  y1=quo_name(y);
  df %>% mutate(!!y :=as.factor(!!y)) %>% anova_test(as.formula(paste(x1,y1,sep="~"))) %>% as_tibble() %>%
    mutate(p=round(p,2),pval=ifelse(p==0,"<0.01",p),significance=ifelse(p<0.05,"significant","non-significant"),
           description=glue("We find that One-way ANOVA was {significance} for Group effect of {y1} on {x1}. In statistical notation it is expressed as F({DFn},{DFd})={pval}. The  Effect size(Omega -Squared) of this One-way ANOVA  test was {ges} .")) -> df1
  df1$description %>% cat()}
  
  
  
  ###W.flow--
    
    #anovaboxplot(mtcars,cyl,disp)
    
    #anovaboxdescr(mtcars,disp,cyl)
    
    
    #anovatable(mtcars,disp,cyl)
    
    #  anovadisc(mtcars,disp,cyl)
    
    
  #tukeytable(mtcars,wt,cyl)
  
  #tukeydescr(mtcars,wt,cyl)
  
  
  
  
  #############Correlation##########
  
  
  
  corplot = function(df,x,y,type="p",results=TRUE){
    x1=enquo(x);
    y1=enquo(y);
    ggscatterstats(data=df,x=!!x1,y=!!y1,marginal = FALSE,type=type,bf.message = FALSE,results.subtitle = results)
  }
  
  
  cordesc= function(df,x,y,method="pearson"){
    x1=enquo(x);
    y1=enquo(y);
    x2=quo_name(x1)
    y2=quo_name(y1)
    df %>% cor_test(!!x1,!!y1,method=method) %>% add_column(parameter=nrow(df)-2) %>%
      mutate(direction1=ifelse(cor<0,"decreases","also increases"),direction=ifelse(cor<0,"negative","positive"),significance=ifelse(p<0.05,"Significant","Non-Significant")) %>%
      mutate_if(is.numeric,round,2) %>% mutate(pvalue=ifelse(p==0,"<0.001",p)) %>%
      mutate(description=glue("The scatter plots above show relationship between { var1 } on X axis and {var2 } on Y axis. Graphically, we see that as {var1} increases, {var2} {direction1} .
On a formal statistical linear regression analysis, we that line of best fit (blue line signifying line with least square difference) also has a {direction} slope implying a {direction} correlation.
The gray shaded error around blue line signifies 95% confidence interval of linear regression line of best fit.
The correlation between two variables is {significance} . The {method}'s correlation between {x2} and {y2} is {cor} with 95% Confidence Interval of {conf.low} to {conf.high}. the t statistic is {statistic} The p value is {pvalue} .The degree of freedom is {parameter}.
In formal statistical notation this expressed as t({parameter})= {statistic}, P= {pvalue}. r({method}) = {cor} 95% C.I. [{conf.low}-{conf.high}]. n= {parameter+2}. The correlation is summmarised in table below")) %>% pull(description)}
  
  
  cordescgroup =function(df,x,y,z,method="pearson"){
    x1=enquo(x);
    y1=enquo(y);
    z1=enquo(z);
    x2=quo_name(x1)
    y2=quo_name(y1)
    z2=quo_name(z1)
    
    count = df %>% count(!!z1) %>% pull(n)
    
    df %>% group_by(!!z1) %>% cor_test(!!x1,!!y1,method=method) %>% rename(Group=1) %>%
      add_column(parameter=count-2) %>% mutate(direction1=ifelse(cor<0,"decreases","also increases"),direction=ifelse(cor<0,"negative","positive"),significance=ifelse(p<0.05,"Significant","Non-Significant")) %>%
      mutate_if(is.numeric,round,2) %>% mutate(pvalue=ifelse(p==0,"<0.001",p)) %>%
      mutate(description=glue("The scatter plots above show relationship between { var1 } on X axis and {var2 } on Y axis in category {Group} of {z2}. Graphically, we see that as {var1} increases, {var2} {direction1} .
On a formal statistical linear regression analysis, we that line of best fit (blue line signifying line with least square difference) also has a {direction} slope implying a {direction} correlation.
The gray shaded error around blue line signifies 95% confidence interval of linear regression line/line of best fit.
The correlation between two variables is {significance} . The {method}'s correlation between {x2} and {y2} is {cor} with 95% Confidence Interval of {conf.low} to {conf.high}. the t statistic is {statistic} The p value is {pvalue} .The degree of freedom is {parameter}.
In formal statistical notation this expressed as t({parameter})= {statistic}, P= {pvalue}. r({method}) = {cor} 95% C.I. [{conf.low}-{conf.high}]. n= {parameter+2}. The correlation is summmarised in table below")) %>% pull(description)}
  
  
  
  
  cortable=function(df,x,y){x1=enquo(x);y1=enquo(y);
  
  df %>% get_summary_stats(!!x1,!!y1) %>% select(variable, n,Mean=mean,SD=sd,Median=median,Minimum=min,Maximum=max)}
  
  # cortable(mtcars,wt,disp)
  
  
  
  
  cortable2=function(df,x,y){x1=enquo(x);y1=enquo(y);df %>%cor_test(!!x1,!!y1) %>% add_column(parameter=nrow(df)-2) %>%
    mutate(significance=ifelse(p<0.05,"Significant","Non-Significant")) %>%
    mutate_if(is.numeric,round,2) %>% mutate(pvalue=ifelse(p==0,"<0.001",p),`95% Confidence Interval`=paste0(conf.low,"-",conf.high)) %>% dplyr::select(`Group 1 ` =var1,`Group 2`=var2,
                                                                                                                                                        `Degree of Freedom`=parameter,
                                                                                                                                                        `T statistic`=statistic,
                                                                                                                                                        Correlation=cor,
                                                                                                                                                        `95 % C.I.` =`95% Confidence Interval`,
                                                                                                                                                        `P value`=pvalue)}
  cortablegroup=function(df,x,y,z){ 
    x1=enquo(x);
    y1=enquo(y);
    z1=enquo(z);
    count = df %>% count(!!z1) %>% pull(n);
    df %>%group_by(!!z1) %>% cor_test(!!x1,!!y1) %>% ungroup() %>%  add_column(parameter=count-2) %>% rename(Group=1) %>%
      mutate(significance=ifelse(p<0.05,"Significant","Non-Significant")) %>%
      mutate_if(is.numeric,round,2) %>% mutate(pvalue=ifelse(p==0,"<0.001",p), Confidence_Interval=paste0(conf.low,"-",conf.high)) %>% 
      select(-method,-p) }
  
  
  corall =function(df){
    data.frame(t(combn(names(df),2)), stringsAsFactors = F) ->dx
    
    corex=function(x,y){cor_test(data=df,x,y)}
    
    
    pmap_df(list(dx$X1,dx$X2),.f=corex) %>% 
      
      mutate(pvalue=ifelse(p<0.001,"<0.001",p),significance=ifelse(p<0.05,"Significant","Non-Significant")) %>% rename(Correlation=cor,Variable1=var1,Variable2=var2) %>%  arrange(p) %>% mutate_if(is.numeric,round,2) %>% mutate(Confidence_Interval=paste0(conf.low,"-",conf.high))  %>% select(-p,-statistic,-conf.low,-conf.high,-method)}
  
  
   ##########W.FlowCorrelation######
  
  # corplot(mtcars,wt,disp)
  
  ##cordesc (mtcars,wt,disp)
  
  # cortable2(mtcars,wt,disp)  
  
  
  ##groupwise
  
  #### cordescgroup(mtcars,wt,disp,cyl)
  
  ###cortablegroup(mtcars,wt,disp,cyl)           
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #####Propcut#########
  
  
  
  cutdf =function(df,x,vec){
    x1=enquo(x)
    
    categories=paste0(quo_name(x1), " categories")
    
    
    
    df%>%mutate(!!categories :=cut2(!!x1,vec)%>%str_remove_all(.,"\\)|\\[|\\]")%>%str_replace(.,","," - "))->df
    
    df }
  
  
  propcuttable=function(df,x,vec,r=2){
    x1=enquo(x)
    
    categories=paste0(quo_name(x1), " categories")
    
    categories1=sym(categories)# to use character
    
    df%>%mutate_if(is.numeric,round,r)%>%mutate(!!categories :=cut2(!!x1,vec)%>%str_remove_all(.,"\\)|\\[|\\]")%>%str_replace(.,","," - "))%>%group_by(!! categories1)%>%summarise(n=n())%>%mutate(total=sum(n),percentage=round(100*n/sum(n),2),
                                                                                                                                                                                                   value=glue("{n}/{total}({percentage}%)"))%>%rowid_to_column()%>%mutate(!! categories :=case_when(rowid==1~ str_replace(!! categories1,'.*-','<'),rowid==nrow(.)~str_replace(!! categories1,"-.*","")%>%paste0(">",.),TRUE~!! categories1)) %>% select(2,3,5)
    
    
  }
  
  
  propcutplot=function(df,x,vec,r){
    x1=enquo(x)
    
    categories=paste0(quo_name(x1), " categories")
    
    categories1=sym(categories)# to use character
    
    df%>%mutate_if(is.numeric,round,r)%>%mutate(!!categories :=cut2(!!x1,vec)%>%str_remove_all(.,"\\)|\\[|\\]")%>%str_replace(.,","," - "))%>%group_by(!! categories1)%>%summarise(n=n())%>%mutate(total=sum(n),percentage=round(100*n/sum(n),2),
                                                                                                                                                                                                   value=glue("{n}/{total}({percentage}%)"))%>%rowid_to_column()%>%mutate(labels=case_when(rowid==1~ str_replace(!! categories1,'.*-','<'),rowid==nrow(.)~str_replace(!! categories1,"-.*","")%>%paste0(">",.),TRUE~!! categories1))%>%mutate(order =parse_number(!! categories1))%>%mutate(labels=fct_reorder(labels,order))%>%ggplot(aes(x=labels,y=n,fill=labels))+geom_col()+geom_text(aes(label=paste0(percentage,"%")),position = position_stack(vjust = 0.5))+coord_flip()+guides(fill=FALSE)+theme_light()+labs(x= categories,y="Count")
    
  }
  
  propcutdescr=function(df,x,vec,r=2){
    x1=enquo(x);
    
    categories=paste0(quo_name(x1), " categories");
    
    categories1=sym(categories)# to use character
    
    df%>%mutate_if(is.numeric,round,r)%>%mutate(!!categories :=cut2(!!x1,vec)%>%str_remove_all(.,"\\)|\\[|\\]")%>%str_replace(.,","," - "))%>%group_by(!! categories1)%>%summarise(n=n())%>%mutate(total=sum(n),percentage=round(100*n/sum(n),2),
                                                                                                                                                                                                   value=glue("{n}/{total}({percentage}%)"))%>%rowid_to_column()%>%mutate(!! categories :=case_when(rowid==1~ str_replace(!! categories1,'.*-','<'),rowid==nrow(.)~str_replace(!! categories1,"-.*","")%>%paste0(">",.),TRUE~!! categories1)) %>%
      arrange(desc(n)) %>% head(2) %>% mutate(description=paste0(value, " patients are in group ",!!categories1, " .")) %>%
      add_row(description=paste0("The plot above shows a flipped bar plot of Counts(X axis) and percentages(annotated within bar) of various categories. The top 2 ",categories," are as follows :"),.before=0) %>%
      add_row(description=paste0("The Full details of distribution is in table below."),.after =nrow(.)) %>%
      pull(description) %>% cat()
  }
  
  #####W.flow prop.cut#######
  
  #df= data.frame(x = runif(1000, 0, 100))
  #vec=c(10,20,30)
  
  #propcutplot(df,x,vec,0)
  #propcutdescr(df,x,vec,0)
  #propcuttable(df,x,vec,0)
  
  
  #########Proportion#####
  
  
  
  
  propplot =function(df,x){
    x=enquo(x);
    # categories=paste0(quo_name(x)," ")
    
    df %>% mutate(categories :=as.factor(!!x))%>% group_by(categories) %>%
      summarise(n=n()) %>% mutate(total=sum(n),percentage=round(100*n/total,2)) %>% arrange(desc(n)) %>%
      mutate(labels=glue("{n}({percentage}%)")) %>%
      mutate(categories=fct_reorder(categories,n)) %>%
      #  mutate(labels=fct_inorder(labels,categories))%>%
      ggplot(aes(x=categories,y=n))+geom_bar(stat="identity",fill="sky blue")+geom_text(aes(label=paste0(percentage,"%")),position = position_stack(vjust = 0.5))+coord_flip()+
      guides(fill=FALSE)+theme_light()+labs(x= "Group",y="Count")}
  
  propdescr= function(df,x){
    
    x=enquo(x);
    
    df %>% group_by(!!x) %>% summarise(n=n()) %>% arrange(desc(n)) %>%  
      mutate(total=sum(n),percentage=round(100*n/sum(n),2),
             value=glue("{n}({percentage}%)")) %>%
      ungroup()%>% mutate(low=qbeta(0.025,n+0.5,total-n+0.5),high=qbeta(0.975,n+0.5,total-n+0.5), Confidence_Interval = paste0(round(100*low,2),"% - ",round(100*high,2),"%")) %>%
      rename(Group=1) %>% select(Group,n,total,percentage,Confidence_Interval) %>%
      mutate(description=glue(" {n}/{total}({percentage} %) patients are in sub-group {Group} ")) %>%
      rowid_to_column() %>% filter(rowid<3) %>%
      add_row(description=paste0("The Flipped Bar- plot above shows Counts(X axis) and percentages(annotated within bar) of various categories. The top 2 sub-groups are as follows :"),.before=0) %>%
      add_row(description=paste0("The Full details of distribution is in table below."),.after =nrow(.)) %>%
      pull(description) %>% cat()
    
  }
  
  
  
  
  proptable= function(df,x){
    
    x=enquo(x);
    
    df %>% group_by(!!x) %>% summarise(n=n()) %>% arrange(desc(n)) %>%  
      mutate(total=sum(n),percentage=round(100*n/sum(n),2),
             value=glue("{n}({percentage}%)")) %>%
      ungroup()%>% mutate(low=qbeta(0.025,n+0.5,total-n+0.5),high=qbeta(0.975,n+0.5,total-n+0.5),  Confidence_Interval = paste0(round(100*low,2),"% - ",round(100*high,2),"%")) %>%
      rename(Group=1) %>% select(Group,n,total,percentage,Confidence_Interval)
  }
    
    
    
    
    
    
    ####W.flow___Prop__
    
    #propplot(mtcars,cyl)
    
    #propdescr(mtcars,cyl)
    
    
    #proptable(mtcars,cyl)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ####################
    
    
    ## twowayanovaplot##
    
    twowayplot=function(df,x,y,z){
      x1=enquo(x);
      y1=enquo(y);
      z1=enquo(z);
      
      df%>%ggplot(aes(!!y1,!!x1,color=!!z1,group=!!z1))+stat_summary(fun.y = mean, geom = "point")+stat_summary(fun.y = mean, geom = "line")+stat_summary(fun.data = mean_se, geom = "errorbar",width=0.2)}
    
    
    library(afex)
    library(car)
    library(tidyverse)
    library(glue)
    #data(obk.long)
    library(broom)
    
    library(emmeans)
    
    twowaydf = function(data=df,id="id",dv="value",between="between",within="within"){
      fit_all <- aov_ez(id=id,dv=dv,data=data,between=between,within=within)
      fit_nice <- aov_ez(id=id,dv=dv,data=data,between=between,within=within,return="nice")
      #fit_nice <- aov_ez(id=id,"value",obk.long,between=c("treatment"),within=c("phase"),return="nice")
      table1=fit_nice%>%as_data_frame()%>%mutate_at(vars(MSE:p.value),parse_number)%>%mutate(significance=ifelse(p.value<0.05,"significant","non-significant"))
      correction= summary(fit_all)$sphericity.tests%>%map_df(~as_data_frame(.))%>%pull(value)%>%matrix(2,2)%>%as_tibble()%>%setNames(nm=summary(fit_all)$sphericity.tests%>%colnames())%>%mutate(variable=summary(fit_all)$sphericity.tests%>%rownames())%>% rename(pval=2) %>% 
        mutate(significance=ifelse(pval<0.05,"significant","non-significant"))%>%mutate(correction=ifelse(pval<0.05,"Since Sphericity (homogeneity of variance between pairs of intra-group comparisons) assumption was not met.Greenhouse-Geisser Correction was applied"," ")) %>%slice(2) %>%  
        pull(correction)
      
      
      
      init=glue("Two way repeated measures ANOVA was done to assess difference between average {dv} of {within} group and if there was inter-group differences between {between} . An interaction test was also conducted to see if inter-group variation of {dv} within {within} was affected by {between}. {correction} ")
      print(init)
      describe=table1 %>% add_column(type=c(glue(" Inter group comparison of average difference between {dv} of  {between}"),
                                            glue(" Intra group comparison of average difference between {dv} of  {within}"),
                                            glue(" Interaction  of  {dv} within {within}  with  {between}"))) %>% 
        mutate(description=glue(" {type} was {significance} with a  p value of {format(p.value,scientific=FALSE)} . In Formal statistical notation it is expressed as F({df}) = {F},p= {format(p.value,scientific=FALSE)}")) %>% pull(description) %>% collapse(.,".")
      
      print(describe)   }
    
    
    #twowaydf(obk.long,id="id",dv="value",between="treatment",within="phase")
    
    
    
    betweentable = function(data=df,id="id",dv="value",between="between",within="within"){
      fit_nice <- aov_ez(id=id,dv=dv,data=data,between=between,within=within,return="nice")
      #fit_nice <- aov_ez(id=id,"value",obk.long,between=c("treatment"),within=c("phase"),return="nice")
      pval= fit_nice%>%as_data_frame()%>%mutate_at(vars(MSE:p.value),parse_number) %>% slice(1) %>% pull(p.value)
      
      if(pval<0.05){
        print(glue("Since Inter group average differences of {dv} of between {between} was significant. We performed a test of contrasts. "))
        
        f=as.formula(paste0("pairwise~",between))
        emm1 =emmeans(fit_nice,specs=f)
        emm1$emmeans %>% as_data_frame()
        emm1$conttrasts %>% as_data_frame()
      }
    }
    
    withincontrast = function(data=df,id="id",dv="value",between="between",within="within"){
      fit_nice <- aov_ez(id=id,dv=dv,data=data,between=between,within=within,return="nice")
      fit_all <- aov_ez(id=id,dv=dv,data=data,between=between,within=within)
      #fit_nice <- aov_ez(id=id,"value",obk.long,between=c("treatment"),within=c("phase"),return="nice")
      pval= fit_nice%>%as_data_frame()%>%mutate_at(vars(MSE:p.value),parse_number) %>% slice(2) %>% pull(p.value)
      
      if(pval<0.05){
        print(glue("Since Intra group average differences of {dv} within {within} group was significant. We performed a test of contrasts. "))
        
        f=as.formula(paste0("pairwise~",within))
        emm1 =emmeans(fit_all,specs=f)
        
        # print(emm1$emmeans %>% as_data_frame());
        
        emm1$contrasts %>% as_data_frame() %>% mutate(significance=ifelse(p.value<0.05,"significant","non-significant"),p.value=ifelse(p.value<0.001,"<0.001",round(p.value,3)),
                                                      description=glue("The Inter-group difference  between {contrast} was {significance} with mean difference of {round(estimate,2)}+-{round(SE,2)}, pvalue= {p.value} ")) %>% 
          pull(description) %>% collapse(.,".") %>% print()
        
        ph=emmeans(fit_all,specs=within)
        
        contrast(ph,"poly") %>% as_data_frame() %>% mutate( 
          significance=ifelse(p.value<0.05,"significant","non-significant"),
          p.value=ifelse(p.value<0.001,"<0.001",round(p.value,3)),
          description=glue("We found a {significance} {contrast} trend in intra-group difference with {within} , {round(estimate,2)}+-{round(SE,2)}, p value= {p.value}")) %>% 
          add_row(description=glue("We also wanted to look for a trend in our {within} group since it had a time varying component "),.before=0) %>% 
          pull(description) %>% collapse(.,".") %>% print()
        
        
      }
    }
    
    betweencontrast = function(data=df,id="id",dv="value",between="between",within="within"){
      fit_nice <- aov_ez(id=id,dv=dv,data=data,between=between,within=within,return="nice")
      fit_all <- aov_ez(id=id,dv=dv,data=data,between=between,within=within)
      #fit_nice <- aov_ez(id=id,"value",obk.long,between=c("treatment"),within=c("phase"),return="nice")
      pval= fit_nice%>%as_data_frame()%>%mutate_at(vars(MSE:p.value),parse_number) %>% slice(1) %>% pull(p.value)
      
      if(pval<0.05){
        print(glue("Since Intra group average differences of {dv} within {within} group was significant. We performed a test of contrasts. "))
        
        f=as.formula(paste0("pairwise~",between))
        emm1 =emmeans(fit_all,specs=f)
        
        # print(emm1$emmeans %>% as_data_frame());
        
        emm1$contrasts %>% as_data_frame() %>% mutate(significance=ifelse(p.value<0.05,"significant","non-significant"),p.value=ifelse(p.value<0.001,"<0.001",round(p.value,3)),
                                                      description=glue("The Inter-group difference  between {contrast} was {significance} with mean difference of {round(estimate,2)}+-{round(SE,2)}, pvalue= {p.value} ")) 
        
      }
    }
    
    
    #withincontrast(obk.long,id="id",dv="value",between="treatment",within="phase")
    
    #betweencontrast(obk.long,id="id",dv="value",between="treatment",within="phase")
    
    betweentablemeans = function(data=df,id="id",dv="value",between="between",within="within"){
      
      fit_all <- aov_ez(id=id,dv=dv,data=data,between=between,within=within)
      
      
      f=as.formula(paste0("pairwise~",between))
      emm1 =emmeans(fit_all,specs=f)
      
      emm1$emmeans %>% as_data_frame()
      
      
    }
    
    #betweentablemeans(obk.long,id="id",dv="value",between="treatment",within="phase")
    
    betweentablecontrasts = function(data=df,id="id",dv="value",between="between",within="within"){
      
      fit_all <- aov_ez(id=id,dv=dv,data=data,between=between,within=within)
      
      
      f=as.formula(paste0("pairwise~",between))
      emm1 =emmeans(fit_all,specs=f)
      
      emm1$contrasts %>% as_data_frame()
      
      
    }
    
    #betweentablecontrasts(obk.long,id="id",dv="value",between="treatment",within="phase")
    
    
    withintablemeans = function(data=df,id="id",dv="value",between="between",within="within"){
      
      fit_all <- aov_ez(id=id,dv=dv,data=data,between=between,within=within)
      
      
      f=as.formula(paste0("pairwise~",within))
      emm1 =emmeans(fit_all,specs=f)
      
      emm1$emmeans %>% as_data_frame()
      
      
    }
    
    withintablecontrasts = function(data=df,id="id",dv="value",between="between",within="within"){
      
      fit_all <- aov_ez(id=id,dv=dv,data=data,between=between,within=within)
      
      
      f=as.formula(paste0("pairwise~",within))
      emm1 =emmeans(fit_all,specs=f)
      
      emm1$contrasts %>% as_data_frame()
      
      
    }
    
    #withintablecontrasts(obk.long,id="id",dv="value",between="treatment",within="phase")
    
    #withintablemeans(obk.long,id="id",dv="value",between="treatment",within="phase")
    
    ##afex_plot##
    
    #afex_plot(fit_all,x="phase",trace="treatment")
    
    
    #This is a trace-plot of variation in {dv} (on Y axis)with various levels of {within} on X-axis , Various groups are marked 
    #by traces of {between}. we can clearly see a {positive} linear trend/slope indicating a {rise} with {within}. The difference between {between} is not-significant,
    #however the {rise} in {value} in {within} is moderated by {between} as we can see different slopes of variation with {within}
    #between {between} groups. we decided to explore this intuitive graphical relationship  with formal statistical tests.
    
    ##linear model
    
    library(insight)
    
    lminterpret = function(m1){response=model_info(m1)$model_terms$response;tidy(m1)%>%mutate_if(is.numeric,round,2)%>%mutate(direction=ifelse(estimate<0,"decrease","increase"),description=glue(" {estimate}*{term} "),description1= glue(" 1 unit change in {term} leads to {abs(estimate)} {direction} in {response} "))%>%rownames_to_column()%>%mutate(description=ifelse(rowname=="1",estimate,description),description1==ifelse(rowname=="1"," ",description1))%>%slice(-1)%>%pull(description1)%>%collapse(.,".")}
    
    lmformula =function (m1)  {
      response=model_info(m1)$model_terms$response;
      rhs=tidy(m1)%>%mutate_if(is.numeric,round,2)%>%mutate(description=glue("{estimate}*{term} "))%>%rownames_to_column()%>%mutate(description=ifelse(rowname=="1",estimate,description))%>%pull(description)%>%collapse(.,"+");
      formula=glue(" Our Final regression equation was predicted  {response} = {rhs} ");
      formula
    }
    
    
    
    lminit= function(m1){
      response=model_info(m1)$model_terms$response;
      predictors= tidy(m1)%>%slice(-1)%>%select(term)%>%pull(term)%>%collapse(.,",")%>%stri_replace_last(.,fixed = ",", " & ");
      desc=glue("
Multiple linear regression was conducted to find best combination of {predictors} for predicting {response} . Dummy indicator(0/1) were used for categorical variables. The Forest plot above shows standardized regression coefficients of {predictors} with their confidence intervals as horizontal error bars on X axis. An error bar which crosses vertical line of zero in this plot is non-significant.");
      desc}
    
    
    lminit1= function(m1){
      response=model_info(m1)$model_terms$response;
      glance(m1)%>%mutate(significance= ifelse(p.value<0.05,"significantly","non-significantly"),pval=ifelse(p.value<0.001,"<0.001","p.value"),effect=ifelse(adj.r.squared<0.2,"small",ifelse(adj.r.squared<0.5,"moderate","large")), percentage=round(100*adj.r.squared,2)
      )%>%mutate_if(is.numeric,round,2)%>%mutate(description=glue("The combination of these predictors {significance} predicted {response} .There were {df+df.residual} observations in our model. The number of predictors in model was {df-1} ,while degree of freedom of residuals(no.of observation-number Of predictors in model)  was {df.residual}. In statistical notation this is expressed as F({df-1},{df.residual}) = {statistic}, P = {pval} .The standard deviation of residual error was {sigma} implying {response} was predicted with average accuracy of +-  {sigma} by our model. The adjusted R - Square for our model is {adj.r.squared} implying our model predicts  {percentage}  percentage variation in {response}  ."))%>%pull(description)%>%cat()}
    
    
    lminit2= function(m1){
      response=model_info(m1)$model_terms$response;
      signfpredictors= tidy(m1)%>%slice(-1)%>%filter(p.value<0.05)%>%select(term)%>%pull(term)%>%collapse(.,",")%>%stri_replace_last(.,fixed = ",", " & ");
      desc=glue("In Our Multivariable linear regression Model,On adjusting for all variables , {signfpredictors} significantly predicted  {response} . ");
      desc}
    
    lminit3= function(m1){
      response=model_info(m1)$model_terms$response;
      tidy(m1)%>%slice(-1)%>%mutate_if(is.numeric,round,2)%>%
        filter(statistic==max(statistic))%>%mutate(
          
          desc=glue("Out of all variables, {term} [ Beta = {estimate} +- {std.error} ] had highest standardized regression coefficient and contributed maximum to predicted { response} ."))%>%
        pull(desc)%>%cat()
      
    }
    
    
    # lminit(m1)
    
    # lminit1(m1)
    
    # lminit2(m1)
    
    
    # lminit3(m1)
    
    # lmformula(m1)
    
    # lminterpret(m1)
    
    
    ascobar =function(df,age,sex, category,outcome){
      age=enquo(age);
      sex=enquo(sex);
      category=enquo(category)
      outcome=enquo(outcome);
      
      f=paste0(quo_name(sex),"~",quo_name(category))
      
      
      df %>% mutate(`Age Group` := cut(!!age,breaks=seq(
        (min(!!age)%/%10)*10,(max(!!age)%/%10+1)*10,
        by=10))%>% str_remove_all("\\(|]")%>%
          str_replace_all(",","-") %>%as.factor())%>%
        
        ggplot(aes(x=`Age Group`,y=!!outcome))+stat_summary(fun.y="mean",geom="bar",fill="sky blue")+
        coord_flip()+facet_grid(f)+xlab("Age group")}
    
    library(rstatix)
    
    ascobox =function(df,age,sex, category,outcome){
      age=enquo(age);
      sex=enquo(sex);
      category=enquo(category)
      outcome=enquo(outcome);
      
      f=paste0(quo_name(sex),"~",quo_name(category))
      
      
      df %>% mutate(`Age Group` := cut(!!age,breaks=seq(
        (min(!!age)%/%10)*10,(max(!!age)%/%10+1)*10,
        by=10))%>% str_remove_all("\\(|]")%>%
          str_replace_all(",","-") %>%as.factor())%>%
        
        ggplot(aes(x=`Age Group`,y=!!outcome))+geom_boxplot()+stat_summary(fun="mean",geom="point",color="red")+
        coord_flip()+facet_grid(f)+xlab("Age group")}
    
    
    ascotable =function(df,age,sex, category,outcome){
      age=enquo(age);
      sex=enquo(sex);
      category=enquo(category)
      outcome=enquo(outcome);
      x1=paste0("Mean ( ",quo_name(outcome)," )");
      
      x2=paste0("SD ( ",quo_name(outcome)," )");
      
      x3=paste0("Median ( ",quo_name(outcome)," )");
      
      df %>%mutate(age_grp := cut(!!age,breaks=seq(
        (min(!!age)%/%10)*10,(max(!!age)%/%10+1)*10,
        by=10))%>% str_remove_all("\\(|]")%>%
          str_replace_all(",","-") %>%as.factor())%>%
        group_by(age_grp,!!sex,!! category) %>% get_summary_stats(!!outcome)%>%select(age_grp,!!sex,n,!! category,Mean=mean,SD=sd,Median=median)%>%
        mutate_if(is.numeric,round,2)%>%
        
        mutate(SD=ifelse(is.na(SD)," ",SD))%>%
        
        select(!!category,age_grp,!!sex,n,!!x1 := Mean,!!x2 :=SD,!!x3 :=Median)%>%arrange(!!category)
    }
    
    
    
    
    
    
    
    agegroupdf= function(df,age,category){
      age=enquo(age);
      
      
      df1=df %>%mutate(age_grp := cut(!!age,breaks=seq(
        (min(!!age)%/%10)*10,(max(!!age)%/%10+1)*10,
        by=10))%>% str_remove_all("\\(|]")%>%
          str_replace_all(",","-")) 
      
      df1}
    
    
    
    
    
    
    














