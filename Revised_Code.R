library(readxl)
library(knitr)
library(gplots)
library(doBy)
library(dplyr)
library(plyr)
library(printr)
library(xtable)
library(gmodels)
library(survival)
library(pander)
library(psych)
library(questionr)
library(DT)
library(data.table)
library(expss)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
library(kableExtra)
library(magrittr)
library(Hmisc)
library(forestmangr)
library(summarytools)
library(stats)
library(gridExtra)
library(yaml)
library(tibble)


#==========================#

# Functions

multi.fun  <- function (x,digit=3){
  m<-as.data.frame(table(x))
  m<-cbind(m,round(m$Freq/sum(m$Freq)*100, digit))
  colnames(m)<-c("","Frequency","%")
  return(m)
}



getCI.exp <- function(beta){
  round(c( exp( c(beta[1], Lower=beta[1]-qnorm(0.975)*beta[2],
                  Upper=beta[1]+qnorm(0.975)*beta[2])), beta[4]),3)  }



getCI  <- function(beta){
  round(c( beta[1], Lower=beta[1]-qnorm(0.975)*beta[2],
           Upper=beta[1]+qnorm(0.975)*beta[2] , beta[4]),3)  }



get.col <- function(coln, data=rawdata, myLetters=letters[1:26] ){
  if (nchar(coln)==1) { out <- data[, match(coln, myLetters)] }
  if (nchar(coln)==2) {
    f1 <- substr(coln, 1,1)
    f2 <- substr(coln, 2,2)
    out <- data[, 26*+match(f1, myLetters)+match(f2, myLetters)]
  }
  out
}



# figTitle.lm = function(response=NULL, y.name=NULL) {
#   if (!is.null(response) & !is.null(y.name)) {
#     paste("Linear Regression with", response, "as Response and", y.name, "as Predictor")
#   }
#   else if (!is.null(y.name) | y.name != " ") {
#     paste("Linear Regression with", y.name, "as Predictor")
#   }
#   else if (!is.null(response) | response != "") {
#     paste("Linear Regression with", response, "as Response")
#   }
#   else {
#     paste("Linear Regression")
#   }
# }
#
#
#
# figTitle.logistic = function(response, y.name) {
#   if ((!is.null(y.name) | y.name != " ") & (!is.null(response) | response != "")) {
#     paste("Logistic Regression with", response, "as Response and", y.name, "as Predictor")
#   }
#   else if (!is.null(y.name) | y.name != " ") {
#     paste("Logistic Regression with", y.name, "as Predictor")
#   }
#   else if (!is.null(response) | response != "") {
#     paste("Logistic Regression with", response, "as Response")
#   }
#   else {
#     paste("Logistic Regression")
#   }
# }
#
#
#
# format.tabplot = function(df, y.name, title=NULL, caption_heading=NULL, caption=NULL, plot) {
#
#   table = kable(df, align = "c", "latex", booktabs = T, caption=figTitle.lm(x, title, y.name)) %>%
#     kable_styling(position = 'center',
#                   latex_options = c("striped", "repeat_header", "hold_position")) %>%
#     footnote(general = caption, general_title = caption_heading, footnote_as_chunk = T,
#              title_format = c("italic", "underline"), threeparttable = T)
#   plot = plot
#   list(table = df, plot = plot)
# }



insertRow2 <- function(existingDF, newrow, r) {
  existingDF <- rbind(existingDF,newrow)
  existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
  row.names(existingDF) <- 1:nrow(existingDF)
  return(existingDF)
}



crosstable <- function(y, bygroup, bygroup.name=NULL, bygroup.labels=NULL, y.name=NULL, y.labels=NULL) {
  df = rbind(as.data.frame.matrix(table(bygroup, y)), margin.table(table(bygroup, y), 2))
  rowpercent = as.data.frame.matrix(round(prop.table(as.matrix(df), 1), 4)) * 100
  colnames(rowpercent) = rep("(%)", ncol(rowpercent))
  df = cbind(df, All=margin.table(data.matrix(df), 1)); m = data.frame("(%)" = "")
  for (val in 1:ncol(rowpercent)) {m = cbind(m, df[val], rowpercent[val])}
  m=cbind(m, df[ncol(df)]); colnames(m)[1] = bygroup.name
  if (!is.null(y.labels) & length(y.labels) == (ncol(df)-1)) {
    for (val in 1:length(y.labels)) {colnames(m)[val * 2] = y.labels[val]}
  } else {
    for (val in 1:(ncol(df) - 1)) {colnames(m)[val * 2] = val}
  }
  rownames(m) = NULL
  if (!is.null(bygroup.labels)) {m[1] = c(bygroup.labels, "All")}
  else {m[1] = c(1:(nrow(m) - 1), "All")}
  return(m)
}



summaryTable <- function(y, bygroup, digit=3, table.title=NULL, caption_heading=NULL, caption=NULL, freq.tab=F,
                         y.name=NULL, y.labels=NULL, bygroup.name=NULL, bygroup.labels=NULL,
                         boxplot=F, boxplot.labelsize = 1, plot.title=NULL) {
  if (!missing(bygroup)){
    if (freq.tab){
      if ( !is.null(bygroup.name) & !is.null(y.name)){
        m <- crosstable(y=y, bygroup=bygroup, y.name=y.name, bygroup.labels=bygroup.labels, bygroup.name=bygroup.name, y.labels=y.labels)
      }else{
        m <- crosstable(y=y, bygroup=bygroup, bygroup.labels=bygroup.labels, bygroup.name=" ", y.labels=y.labels)
      }
    }else{
      m = merge(data.frame(y.group = ""), round(data.frame(describeBy(y, bygroup, mat = T)[,c(2, 4, 5,6,7,10,11)][,2:7]), digit))
      if ( !is.null(bygroup.name) & !is.null(bygroup.labels)){
          m[1]<-bygroup.labels;colnames(m) <- c(bygroup.name, "n", "Mean", "Std", "Min", "Median", "Max")
      } else {
        m[1] = c(1:nrow(m));colnames(m) = c(" ", "n", "Mean", "Std", "Min", "Median", "Max")
      }
    }
  }else{
    if (freq.tab) {
      m <- data.frame(multi.fun(y, digit), check.names = F)
      colnames(m) = c(y.name, "Frequency", "%")
      if (!missing(y.labels) & length(y.labels) == nrow(m)) {m[y.name] = y.labels}
      else {m[y.name] = 1:nrow(m)}
    }else{
      m <- data.frame(descr(y,stats = c("N.valid","mean","sd","min","med","max"),transpose =T,headings =F), check.names=F)
      m = round(m, digits=digit)
      colnames(m) = c("n", "Mean", "Std", "Min", "Median", "Max")
      if (!is.null(y.labels) & (nrow(m) == length(y.labels))) {rownames(m) <- y.labels}
      else {rownames(m) = NULL}
    }
  }
  if (nrow(m) == 1) {rowcolor = "white";num = 1}
  else {rowcolor = "#E3E5E7";num = 2}
  layout(matrix(c(1, 1, 2, 1)), 2, 1)
  m <- kable(m, align="c", "latex", booktabs=T, caption=table.title) %>%
    kable_styling(position='center', stripe_color = rowcolor, stripe_index = seq(num, nrow(m), 2),
                  latex_options=c("striped", "repeat_header", "hold_position")) %>%
    footnote(general=caption, general_title=caption_heading, footnote_as_chunk=T,
             title_format=c("italic", "underline"), threeparttable = T)
  if (boxplot) {
    plot <- boxplot(y~bygroup, main=plot.title, names=bygroup.labels, ylab=y.name,
                    xlab=bygroup.name, col=c("red", "blue", "orange", "pink", "green", "purple", "grey", "yellow"), border="black",
                    horizontal=F, varwidth=T, main=par(cex.axis=boxplot.labelsize))
    return(list(m, plot))
  }
  else{return(m)}
}



output.pv <- function(pp, digit) {
  ifelse (pp < 0.001, paste("< 0.001"), signif(pp, digit))
}



regTable.lm = function(formula, y.name=NULL, y.labels=NULL, digit=3,
                       caption=NULL, caption_heading=NULL, title=NULL, data) {
  f = data.frame(summary(lm(formula, data=data))$coef, check.names=F)[-c(1),]
  colnames(f) = c("Difference", "CI Lower", "CI Upper", "p value")
  row.names(f) = NULL; CI = as.data.frame(summary(lm(formula, data=data))$coef)[-c(1),]
  for (val in 1:length(y.labels)) {
    f[val, "Difference"] = round(f[val, "Difference"], digit)
    f[val, "CI Upper"] = round(CI[val, 1] + qnorm(0.975)*CI[val, 2], digit)
    f[val, "CI Lower"] = round(CI[val, 1] - qnorm(0.975)*CI[val, 2], digit)
    f[val, "p value"] = output.pv(f[val, "p value"], digit)
  }
  f = add_column(f, " "=y.labels, .before="Difference")
  colnames(f) = c(" ", "Difference", "CI Lower", "CI Upper", "p value")
  if (!missing(y.name) & !is.null(y.name)) {
    colnames(f) = c(y.name, "Difference", "CI Lower", "CI Upper", "p value")
  }
  rowcolor = "white"; num = 1
  if (nrow(f) > 1) {rowcolor = "#E3E5E7"; num = 2}
  kable(f, align="c", "latex", booktabs=T, caption=title) %>%
    kable_styling(position='center', stripe_color = rowcolor, stripe_index = seq(num, nrow(f), 2),
                  latex_options=c("striped", "repeat_header", "hold_position")) %>%
    footnote(general=caption, general_title=caption_heading, footnote_as_chunk=T,
             title_format=c("italic", "underline"), threeparttable = T)
}



regTable.logistic = function(formula, y.name=NULL, y.labels=NULL, digit=3,
                             title=NULL, caption=NULL, caption_heading=NULL, data) {
  f = data.frame(summary(glm(formula, data=data, family=binomial(link="logit")))$coef)[-c(1),]
  colnames(f) = c( "OR", "CI Lower", "CI Upper", "p value");row.names(f) = NULL
  CI = as.data.frame(summary(glm(formula, data=data, family=binomial(link="logit")))$coef)[-c(1),]
  OR = as.data.frame(odds.ratio(glm(formula, data=data, family=binomial(link="logit"))))[-c(1),]
  for (val in 1:length(y.labels)) {
    f[val, "CI Upper"] = round(exp(CI[val, 1] + qnorm(0.975)*CI[val, 2]), digit)
    f[val, "CI Lower"] = round(exp(CI[val, 1] - qnorm(0.975)*CI[val, 2]), digit)
    f[val, "OR"] = round(OR[val, 1], digit)
    f[val, "p value"] = output.pv(f[val, "p value"], digit = digit)
  }
  f = add_column(f, " "=y.labels, .before="OR")
  colnames(f) = c(" ", "OR", "CI Lower", "CI Upper", "p value")
  if (!missing(y.name) & !is.null(y.name)) {
    colnames(f) = c(y.name, "OR", "CI Lower", "CI Upper", "p value")
  }
  if (nrow(f) == 1) {rowcolor = "white"; num = 1}
  else {rowcolor = "#E3E5E7"; num = 2}
  kable(f, align="c", "latex", booktabs=T, caption=title) %>%
    kable_styling(position='center', stripe_color = rowcolor, stripe_index = seq(num, nrow(f), 2),
                  latex_options=c("striped", "repeat_header", "hold_position")) %>%
    footnote(general=caption, general_title=caption_heading, footnote_as_chunk=T,
             title_format=c("italic", "underline"), threeparttable = T)
}



#==========================#

# This is based on the dataset which is updated on 12/15/2018,
# with longer follow-up of the patients.
# There are some chages on data associated with OS,PFS and NRM.

rawdata <- data.frame(read_excel("FUN allo 121518.xlsx"))

# on_treatment.date <- as.Date(get.col("iv"))
on_treatment.date <- as.Date(get.col("iv"))

wdata <- data.frame(
  NRM.time = (as.Date(get.col("iy"))-on_treatment.date)/365.25*12,
  NRM.event= get.col("ix"),
  # PFS
  PFS.time = (as.Date(get.col("jb"))-on_treatment.date)/365.25*12,
  # PFS.event= get.col("ja"),
  PFS.event= as.numeric(get.col("ix")!=0),
  # OS
  OS.time = (as.Date(get.col("je"))-on_treatment.date)/365.25*12,
  OS.event= get.col("jd"),
  # Duration of hospitalization
  LOS = get.col("x"),
  # SAEs within 100 days post-transplant
  SAE = get.col("ip"),
  # readmissions within 100 days
  readm = get.col("y"),
  readm.any = as.integer(get.col("y")>0),
  # donor type matched sib (=0), MUD (=1-2), others (=3-5)
  donor = as.numeric(get.col("v")),
  donor.3cat = cut(as.numeric(get.col("v")),breaks = c(0,1,3,8), right=FALSE),
  # IADL independence
  IADL.score = get.col("fe"),
  IADL.any = get.col("ff"),
  IADL.domains = get.col("fg"),
  # ADL independence
  ADL.score = get.col("fr"),
  ADL.any = get.col("fs"),
  # Number of falls in last 6 months
  Falls.num = get.col("fu"),
  # KPS: Patient assigned
  KPS.patient = as.numeric(as.character(get.col("ft"))),
  # KPS: Provider assigned
  KPS.provider = as.numeric(as.character(get.col("dw"))),
  # TUG in seconds
  TUG = get.col("ec"),
  #	Mental health/MHI
  MHI.score = get.col("el"),
  MHI.2cat = as.integer(get.col("el")>76),
  # Cognition/BOMC
  BOMC.score = get.col("ed"),
  BOMC.2cat = as.integer(get.col("ed")>4),
  # MOS social support
  MOC = get.col("io"),
  #	Weight
  wtchange = get.col("ea"),
  BMI = get.col("eb"),
  # Baseline QOL
  pw = get.col("bs"),
  sw = get.col("cb"),
  ew = get.col("ci"),
  fw = get.col("cq"),
  BMTS = get.col("dh"),
  #	Number of medications
  meds = get.col("hc"),
  # Age
  age = get.col("c"),
  age.3cat = cut(get.col("c"), breaks=c(50,60,70,80), right=FALSE),
  age.2cat = cut(get.col("c"), breaks=c(50,60,80), right=FALSE),
  # Gender
  female = as.integer(get.col("d")=="F"),
  # living alone
  LA = as.numeric(as.character(get.col("et"))),
  #	Marital status  1 v all others=0,2,3,4,9
  MS = as.numeric(as.character(get.col("ep"))),
  MS.2cat = as.integer(as.numeric(as.character(get.col("ep")))==1),
  # Education
  ED = as.numeric(as.character(get.col("eo"))),
  ED.3cat = cut(as.numeric(get.col("eo")), breaks=c(0,3,6,8), right=FALSE),
  # HCT-CI raw score
  HCTCI.score = get.col("bb"),
  # HCT-CI 	0-2 v 3+ (categories)
  HCTCI.2cat = cut(get.col("bb"),breaks = c(0,3,8), right=FALSE),
  # HCT-CI 	0 v 1-2 v 3+ (categories)
  HCTCI.3cat = cut(get.col("bb"),breaks = c(0,1,3,8), right=FALSE),
  # HCT-CI 	0-1 v 2 v 3+ (categories)
  HCTCI.3 = cut(get.col("bb"),breaks = c(0,2,3,8), right=FALSE),
  # Disease status at transplant, CR1, PR1 (=1,4) vs everyone else (=2, 3, 5, 6, 7, 8), exclude unknown and other (=9, 10)
  DZ = as.numeric(as.character(get.col("r"))),
  # CIBMTR
  CIBMTR = as.numeric(as.character(get.col("q"))),
  # Categorical: low (=0) vs intermediate (=1) vs high (=2); exclude other and unknown (=3, 9)
  CIBMTR.3cat = cut(as.numeric(as.character(get.col("q"))),breaks = c(0,1,2,3,10), right=FALSE),
  # Categorical: low (=0) vs intermediate-high (=1-2); exclude other and unknown (=3, 9)
  CIBMTR.2cat = cut(as.numeric(as.character(get.col("q"))),breaks = c(0,1,3,10), right=FALSE),
  # Transplant intensity (E: 0=fully ablative, 1=non-myeloablative)
  intensity = as.numeric(as.character(get.col("u"))),
  # Diagnosis
  DX = as.numeric(as.character(get.col("k")))
)


FL100 <- 100/30.4 * (wdata$OS.time>100/30.4) +  wdata$OS.time * (wdata$OS.time<=100/30.4)

wdata$Falls.any <- as.integer(wdata$Falls.num>0)

wdata$ED[wdata$ED==9]<-NA

wdata$DX.3cat <- wdata$DX
wdata$DX.3cat[which(wdata$DX.3cat!=1&wdata$DX.3cat!=3)]<-2

wdata$DZ.2cat <- wdata$DZ
wdata$DZ.2cat[which(wdata$DZ.2cat==1|wdata$DZ.2cat==4)]<-0
wdata$DZ.2cat[which(wdata$DZ.2cat==9|wdata$DZ.2cat==10)]<-NA
wdata$DZ.2cat[which(wdata$DZ.2cat!=0)]<-1

wdata$IADL.score[wdata$IADL.score==999]<-NA
wdata$IADL.any[wdata$IADL.any==999]<-NA
wdata$IADL.domains[wdata$IADL.domains==999]<-NA
wdata$ADL.score[wdata$ADL.score==999]<-NA
wdata$ADL.any[wdata$ADL.any==999]<-NA

wdata$ADL <- 100-5*wdata$ADL.score
wdata$ADL.2cat <- as.numeric(wdata$ADL>=85)

wdata$meds[wdata$meds==999]<-NA
wdata$TUG[wdata$TUG==9999]<-NA
wdata$MOC[wdata$MOC==999]<-NA
wdata$pw[wdata$pw==999]<-NA
wdata$sw[wdata$sw==999]<-NA
wdata$ew[wdata$ew==999]<-NA
wdata$CIBMTR.2cat[wdata$CIBMTR.2cat==3]<-NA
wdata$CIBMTR.3cat[wdata$CIBMTR.3cat==4]<-NA
wdata$CIBMTR.2cat<-as.numeric(wdata$CIBMTR.2cat)
wdata$CIBMTR.3cat<-as.numeric(wdata$CIBMTR.3cat)
wdata$CIBMTR.2cat[wdata$CIBMTR.2cat==3]<-NA
wdata$CIBMTR.3cat[wdata$CIBMTR.3cat==4]<-NA
