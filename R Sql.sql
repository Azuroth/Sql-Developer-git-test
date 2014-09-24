set define off;
set serveroutput on;
exec dbms_output.enable(1000000000);
set escape '\';
create or replace
procedure          predictive_webcall
(	
  modelName in varchar2(100);
  inputs in varchar2(1000);
)
IS
DECLARE
  req   UTL_HTTP.REQ;
  resp  UTL_HTTP.RESP;
  text clob;  -- URL to post to
  v_url VARCHAR2(200) := 'http://rdev/r/predictModel.R';
  -- Post Parameters
  v_param VARCHAR2(500) := 'modelFile=/var/www/html/r/svm1.rds&data=SERIAL NUMBER TAG,CENTERA,1,26.09,FRU REPLACEMENT,UNKNOWN/NULL';
  v_param_length NUMBER := length(v_param);
BEGIN
  req := UTL_HTTP.BEGIN_REQUEST (url=> v_url, method => 'POST');
  --UTL_HTTP.SET_HEADER(req, 'User-Agent', 'Mozilla/4.0');
  UTL_HTTP.SET_HEADER (r      =>  req,
                       name   =>  'Content-Type',
                       VALUE  =>  'application/x-www-form-urlencoded');
  UTL_HTTP.SET_HEADER (r      =>   req,
                       name   =>   'Content-Length',
                       VALUE  =>   v_param_length);
  UTL_HTTP.WRITE_TEXT (r      =>   req,
                       data   =>   v_param);  
  resp := UTL_HTTP.GET_RESPONSE(req);
  --DBMS_OUTPUT.PUT_LINE('HTTP response status code: ' || resp.status_code);
  --DBMS_OUTPUT.PUT_LINE('HTTP response reason phrase: ' || resp.reason_phrase);
  LOOP
    UTL_HTTP.READ_LINE(resp, text, TRUE);
    DBMS_OUTPUT.PUT_LINE(text);
  END LOOP;
  UTL_HTTP.END_RESPONSE(resp);
EXCEPTION
  WHEN UTL_HTTP.END_OF_BODY THEN
    UTL_HTTP.END_RESPONSE(resp);
  WHEN OTHERS THEN
    DBMS_OUTPUT.put_line ('Error: ' || Utl_Http.get_detailed_sqlerrm);
    UTL_HTTP.END_RESPONSE(resp);
END;
/
declare
  lop number := 10;
  text clob;
BEGIN
for lop in 1..1000
Loop
  dbms_output.put_line(systimestamp);
  predictive_webcall('svm1.rds','SERIAL NUMBER TAG,CENTERA,1,26.09,FRU REPLACEMENT,UNKNOWN/NULL',text);
end loop;
  dbms_output.put_line(text);
END;

/
--------------------------------------------------------------------- ORE Stuff
SELECT *
FROM TABLE(rqTableEval(CURSOR (SELECT BRAND, PROPERTY,VALUE FROM pb_lenth_test), 
                      NULL, 
                      'select BRAND,PROPERTY,VALUE from pb_lenth_results',
                      'lenthsTest')
          );

/
begin
sys.rqScriptDrop('lenthsTest');
sys.rqScriptCreate('lenthsTest',
'function(rawFrame) {
  wideFrame <- reshape(rawFrame,idvar="PROPERTY",timevar="BRAND",direction="wide");
  names(wideFrame) = gsub("VALUE.","",names(wideFrame));
  rownames(wideFrame) <- wideFrame[["PROPERTY"]];
  wideFrame <- subset(wideFrame, select = -c(PROPERTY));
  stdFrame <- wideFrame;
  wideFrame <- sapply(wideFrame, as.numeric);
  globalMean <- mean(as.vector(as.matrix(wideFrame)),na.rm=TRUE);
  for(r in 1:nrow(wideFrame)) {
    for (c in 1:ncol(wideFrame)) {
      stdFrame[r,c] <- wideFrame[r,c] - ((colMeans(wideFrame,na.rm=TRUE)[c]+rowMeans(wideFrame,na.rm=TRUE)[r])-globalMean)
    }
  }
  stdFrameMed <- median(abs(as.vector(as.matrix(stdFrame))),na.rm=TRUE);
  stDev <- stdFrameMed*1.5;
  outliers <- stDev*2.5;
  cleanFrame <- abs(stdFrame)*ifelse(abs(stdFrame)>outliers,NA,1);
  cleanFrameMedian <- median(as.vector(as.matrix(cleanFrame)),na.rm <- TRUE);
  psoe <- cleanFrameMedian*1.5;
  outFrame <- stdFrame/psoe;
  outFrame <- reshape(outFrame, idvar = "PROPERTY", ids = row.names(outFrame), times = names(outFrame), timevar = "BRAND", varying = list(names(outFrame)), direction = "long",v.names="VALUE")
  return(outFrame);
 }
');
commit;
end;

/

begin
sys.rqScriptDrop('pbTest');
sys.rqScriptCreate('pbTest',
'function() {
  PROPERTY = c("SinglePlat","PriceAcc","ContMgmt","InvMgmt","InvTrack","JiTDelMdl","FlexDelv","DelvPoC","ProdMix","Breadth","MinPurch","ProdCost","WorkDir","ServNet","SinglePlat","PriceAcc","ContMgmt","InvMgmt","InvTrack","JiTDelMdl","FlexDelv","DelvPoC","ProdMix","Breadth","MinPurch","ProdCost","WorkDir","ServNet","SinglePlat","PriceAcc","ContMgmt","InvMgmt","InvTrack","JiTDelMdl","FlexDelv","DelvPoC","ProdMix","Breadth","MinPurch","ProdCost","WorkDir","ServNet","SinglePlat","PriceAcc","ContMgmt","InvMgmt","InvTrack","JiTDelMdl","FlexDelv","DelvPoC","ProdMix","Breadth","MinPurch","ProdCost","WorkDir","ServNet","SinglePlat","PriceAcc","ContMgmt","InvMgmt","InvTrack","JiTDelMdl","FlexDelv","DelvPoC","ProdMix","Breadth","MinPurch","ProdCost","WorkDir","ServNet","SinglePlat","PriceAcc","ContMgmt","InvMgmt","InvTrack","JiTDelMdl","FlexDelv","DelvPoC","ProdMix","Breadth","MinPurch","ProdCost","WorkDir","ServNet");
  BRAND = c("Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Owens & Minor (Client)","Medline","Medline","Medline","Medline","Medline","Medline","Medline","Medline","Medline","Medline","Medline","Medline","Medline","Medline","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","Cardinal Health","PHS","PHS","PHS","PHS","PHS","PHS","PHS","PHS","PHS","PHS","PHS","PHS","PHS","PHS","Seneca","Seneca","Seneca","Seneca","Seneca","Seneca","Seneca","Seneca","Seneca","Seneca","Seneca","Seneca","Seneca","Seneca","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing","Direct Purchasing");
  VALUE = c(267,230,205,207,200,203,181,155,189,216,165,212,173,141,59,76,41,41,37,39,45,28,42,48,51,72,43,29,74,58,41,46,47,53,48,42,40,66,38,49,37,38,3,5,2,2,4,4,5,6,2,4,4,3,3,3,5,5,1,1,0,2,4,3,2,5,5,3,2,2,15,56,37,17,17,12,26,19,14,16,26,48,66,20);
  testFrame = data.frame(PROPERTY,BRAND,VALUE);
  wideFrame <- reshape(testFrame,idvar="PROPERTY",timevar="BRAND",direction="wide");
  names(wideFrame) = gsub("VALUE.","",names(wideFrame));
  rownames(wideFrame) <- wideFrame[["PROPERTY"]];
  wideFrame <- subset(wideFrame, select = -c(PROPERTY));
  stdFrame <- wideFrame;
  for(r in 1:nrow(wideFrame)) {
    for (c in 1:ncol(wideFrame)) {
      stdFrame[r,c] <- wideFrame[r,c] - ((colMeans(wideFrame)[c]+rowMeans(wideFrame)[r])-mean(as.vector(as.matrix(wideFrame))))
    }
  }
  stdFrameMed <- median(abs(as.vector(as.matrix(stdFrame))));
  stDev <- stdFrameMed*1.5;
  outliers <- stDev*2.5;
  cleanFrame <- abs(stdFrame)*ifelse(abs(stdFrame)>outliers,NA,1);
  cleanFrameMedian <- median(as.vector(as.matrix(cleanFrame)),na.rm <- TRUE);
  psoe <- cleanFrameMedian*1.5;
  outFrame <- stdFrame/psoe;
  outFrame <- reshape(outFrame, idvar = "PROPERTY", ids = row.names(outFrame), times = names(outFrame), timevar = "BRAND", varying = list(names(outFrame)), direction = "long",v.names="VALUE")
  rownames(outFrame) = c(1:nrow(outFrame));
  return(outFrame);
}
');
commit;
end;

/

begin
sys.rqScriptDrop('pbPCATest');
sys.rqScriptCreate('pbPCATest',
'function(rawData, SCALED = 0, ROTATE = 1, SHORTEST = "") {
  wideData <- reshape(rawData,idvar=colnames(rawData)[1],timevar=colnames(rawData)[2],direction="wide");
  names(wideData) = gsub(".*\\.","",names(wideData));
  rownames(wideData) <- wideData[[1]];
  wideData <- wideData[-c(1)];
  df<-scale(wideData);
  df<-t(df); #brands on left
  if(SCALED == 0){
    df.prcomp<-prcomp(df,retx=TRUE,center=TRUE,scale=FALSE);
  } else {
    df.prcomp<-prcomp(df,retx=TRUE,center=TRUE,scale=TRUE);
  }
  
  var<-df.prcomp$x;
  
  if(SHORTEST != "") {
    df <- data.frame(var);
    minDist <- sqrt(df[SHORTEST,"PC1"]^2+df[SHORTEST,"PC2"]^2);
    var <- as.matrix(subset(df, sqrt( PC1^2+PC2^2 ) >= minDist));    
  }
  
  grp<-df.prcomp$rotation;
  roots<-df.prcomp$sdev;
  
  sv<-df.prcomp$sdev*sqrt(nrow(df)-1);
  m<-mean(sv[1:2]);
  rv<-sqrt(ncol(grp)/nrow(var));
  x<-grp * m * rv;
  
  if(ROTATE != 0) {
    x<-x*-1;
    var<-var*-1;  
  }
  
  coord<-rbind(x,var)
  #now plot
  
  # percent explained
  pce<-roots^2/sum(roots^2);  
  outPlot <- biplot(x,var, var.axes=TRUE, 
         main="Principal Component Analysis - standardized",
         xlab=paste("PC1   PCE=", round(pce[1],2)*100,"%"),
         ylab=paste("PC2   PCE=", round(pce[2],2)*100,"%"));
  points(x,pch=16);
}
');
commit;
end;


/

set define off;
begin
sys.rqScriptDrop('PCBiplot');
sys.rqScriptCreate('PCBiplot',
'PCbiplot <- function(brand, pce, group, title, xlab=paste("PC1  PCE=",round(pce[1],2)*100,"%", sep=""), ylab=paste("PC2  PCE=",round(pce[2],2)*100,"%", sep="")) {
  brandf <- data.frame(obsnames=row.names(brand), brand);
  plot <- ggplot(brandf, aes_string(x="PC1", y="PC2")) + theme(legend.position = "none")+xlab(xlab)+ylab(ylab) + ggtitle(title);
  plot <- plot + geom_text(data = data.frame(brandf[1:2],brandf[3]-.3), alpha=1, size=5, aes(label = obsnames, colour = "Orange")) 
  plot <- plot + geom_point(aes(colour = red),data=brandf, pch=16, col="red");
  plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
  datapc <- data.frame(varnames=rownames(group), group)
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=.95*PC1, yend=.95*PC2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.5, color="light blue")
  plot <- plot + coord_equal() + geom_text(data=data.frame(datapc[1:2],datapc[3]-.2), aes(label=varnames), size = 5, vjust=1, color="dark green")
  plot <- plot + geom_point(aes(colour = red),data=datapc, pch=18, col="Dark Green");
  plot;
}
');
commit;
end;

/

select * 
from table(rqTableEval(CURSOR
  (select 
          db.brand BRAND, da.attribute ATTRIBUTE, avg(fra.att_ae) SCORE
        from 
          wh_demo_market_nav.fact_response_attribute fra
          inner join wh_demo_market_nav.dim_attribute da
          on fra.attribute_id = da.attribute_id
          inner join wh_demo_market_nav.dim_brand db
          on db.brand_id = fra.brand_id
        where db.brand_id in (20,23)
        group by db.brand, da.attribute),
  CURSOR(SELECT 0 scaled, 0 rotate from dual),
  'select cast(''a'' as varchar2(500)) Name, cast(''a'' as varchar2(500)) PC1, cast(''a'' as varchar2(500)) PC2, cast(''a'' as varchar2(500)) Type, cast(''a'' as varchar2(500)) CPC1, cast(''a'' as varchar2(500)) CPC2 from dual',
  'PCAChart'));

/
set define off;
begin
sys.rqScriptDrop('PCAChart');
sys.rqScriptCreate('PCAChart','
pcaChart <- function(rawData, SCALED = 0, ROTATE = 1) {
  #Transform long data into wide
  wideData <- reshape(rawData,idvar=colnames(rawData)[1],timevar=colnames(rawData)[2],direction="wide");
  names(wideData) = gsub(".*\\.","",names(wideData));
  rownames(wideData) <- wideData[[1]];
  wideData <- wideData[-c(1)];
  
  #brands on left for scaling
  wideData<-t(wideData);  
  if(SCALED == 0){
    df<-scale(wideData, center=TRUE, scale = FALSE);
  } else {
    df<-scale(wideData, center=TRUE, scale = TRUE);
  }
  #brands across top for calculations
  df<-t(df); 
  
  df.prcomp<-prcomp(df,retx=TRUE,center=FALSE,scale=FALSE);
  
  var<-df.prcomp$x;
  
  grp<-df.prcomp$rotation;
  roots<-df.prcomp$sdev;
  
  sv<-df.prcomp$sdev*sqrt(nrow(df)-1);
  m<-mean(sv[1:2]);
  rv<-sqrt(ncol(grp)/nrow(var));
  scores<-grp * m * rv;
  
  # percent explained
  pce<-roots^2/sum(roots^2);  

  if(ROTATE != 0) {
    scores<-scores*-1;
    var<-var*-1;  
  }
  
  corrows = cor(var, df);
  corrows = t(corrows);
  
  corcolumns = cor(scores, t(df))
  corcolumns = t(corcolumns);
  
  outGroup<-cbind(scores[,1:2],matrix("G",nrow(scores),1),corrows[,1:2]);
  colnames(outGroup) = c("PC1","PC2","Class","Cor PC1","Cor PC2");
  outAttrib<-cbind(var[,1:2],matrix("A",nrow(var),1),corcolumns[,1:2]);
  colnames(outAttrib) = colnames(outGroup);
  outChart<-rbind(outGroup,outAttrib,"Percent Explained" = c(pce[1],pce[2],"P",0,0));
  return(data.frame(rownames(outChart),outChart));
}
');
commit;
end;

/

set define off;
begin
sys.rqScriptDrop('pbPCATest');
sys.rqScriptCreate('pbPCATest',
--sys.rqScriptDrop('PCAGraph');
--sys.rqScriptCreate('PCAGraph',
'function(rawData, SCALED = 0, ROTATE = 1, SHORTEST = "") {
  #Transform long data into wide
  wideData <- reshape(rawData,idvar=colnames(rawData)[2],timevar=colnames(rawData)[1],direction="wide");
  names(wideData) = gsub(".*\\.","",names(wideData));
  rownames(wideData) <- wideData[[1]];
  wideData <- wideData[-c(1)];
  
  #brands on left for scaling
  wideData<-t(wideData);  
  if(SCALED == 0){
    df<-scale(wideData, center=TRUE, scale = FALSE);
    title = "Principal Component Analysis - unstandardized";
  } else {
    df<-scale(wideData, center=TRUE, scale = TRUE);
    title = "Principal Component Analysis - standardized";
  }
  #brands across top for calculations
  df<-t(df); 
  
  df.prcomp<-prcomp(df,retx=TRUE,center=FALSE,scale=FALSE);
  
  var<-df.prcomp$x;
  
  grp<-df.prcomp$rotation;
  roots<-df.prcomp$sdev;
  
  sv<-df.prcomp$sdev*sqrt(nrow(df)-1);
  m<-mean(sv[1:2]);
  rv<-sqrt(ncol(grp)/nrow(var));
  x<-grp * m * rv;
  
  if(ROTATE != 0) {
    x<-x*-1;
    var<-var*-1;  
  }
  
  if(SHORTEST != "" && SHORTEST %in% row.names(data.frame(var))) {
    df <- data.frame(var);
    minDist <- sqrt(df[SHORTEST,"PC1"]^2+df[SHORTEST,"PC2"]^2);
    var <- as.matrix(subset(df, sqrt( PC1^2+PC2^2 ) >= minDist));    
  }
  
  coord<-rbind(x,var)
  
  # percent explained
  pce<-roots^2/sum(roots^2);  
  brand <- x;
  group <- var;
  xlab <- paste("PC1  PCE=",round(pce[1],2)*100,"%", sep="");
  ylab <- paste("PC2  PCE=",round(pce[2],2)*100,"%", sep="");
  library("grid");
  library("ggplot2");
  brandnames <- rownames(brand)
  brandnames <- paste("",1:length(brandnames),brandnames)
  attrnames <- rownames(group)
  attrnames <- paste("",1:length(attrnames),attrnames)
  brandf <- data.frame(brandnames,brand[,1:2]);
  datapc <- data.frame(attrnames,group[,1:2]);
  plot <- ggplot(brandf, aes(x=PC1, y=PC2))+xlab(xlab)+ylab(ylab) + ggtitle(title) + theme(plot.margin = unit.c(unit(0,"cm"),unit(1, "strwidth",brandnames[1]), unit(c(0,0),"cm")));
  plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2);
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=.95*PC1, yend=.95*PC2), arrow=arrow(length=unit(0.2,"cm")), alpha=1, color="light blue");
  plot <- plot + geom_point(aes(colour = red),data=brandf, pch=16, col="Orange", alpha = .75);
  plot <- plot + geom_point(aes(colour = red),data=datapc, pch=18, col="Dark Green", alpha = .75);
  plot <- plot + geom_text(data = data.frame(brandf[1:3]), alpha=1, size=5, vjust=1.2, aes(label = substr(brandnames,2,3)), colour = "Red");
  plot <- plot + coord_equal() + geom_text(data=data.frame(datapc[1:3]), aes(label=substr(attrnames,2,3)), size = 5, vjust=1.2, colour="Dark Green");
  plot <- plot + scale_x_continuous( expand=c(.05,.05)) + scale_y_continuous( expand = c(.05,.05));
  for (i in 1:length(brandnames))  {
    plot <- plot + annotation_custom(grob = textGrob(label = brandnames[i], 
              hjust = 0, 
              vjust = 2+1.5*i, 
              gp = gpar(cex = .8, col="Red")),ymin = Inf, ymax = Inf,xmin = Inf, xmax = Inf);
  }    
  for (j in 1:length(attrnames))  {
    plot <- plot + annotation_custom(grob = textGrob(label = attrnames[j], 
              hjust = 0, 
              vjust = 2+1.5*i+1.5*j, 
              gp = gpar(cex = .8, col="Dark Green")),ymin = Inf, ymax = Inf,xmin = Inf, xmax = Inf);
  }
  gt <- ggplot_gtable(ggplot_build(plot))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}
');
commit;
end;
/
 
/
select * 
from table(rqTableEval(CURSOR
  (select 
          db.brand BRAND, da.attribute ATTRIBUTE, avg(fra.att_ae) SCORE
        from 
          wh_demo_market_nav.fact_response_attribute fra
          inner join wh_demo_market_nav.dim_attribute da
          on fra.attribute_id = da.attribute_id
          inner join wh_demo_market_nav.dim_brand db
          on db.brand_id = fra.brand_id
        group by db.brand, da.attribute
    
    --SELECT BRAND, PROPERTY,VALUE FROM pb_lenth_test
    ),
  CURSOR(SELECT 500 "ore.png.height", 1200 "ore.png.width", 0 scaled, 1 rotate, null shortest from dual),
  'PNG',
  --'PCAGraph'));
  'pbPCATest'));


select 
          *
        from 
          wh_demo_market_nav.dim_attribute da;
