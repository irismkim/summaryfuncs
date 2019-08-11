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
  row.names(f) = NULL; CI = as.data.frame(summary(glm(formula, data=data))$coef)[-c(1),]
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

