* Written by R;
*  write.foreign(dd.m, "C:/Users/Christian Santizo/Google Drive/CSULB/Courses/STAT 560/Project/monthly_net_earnings.txt",  ;

DATA  rdata ;
INFORMAT
 month
 YYMMDD10.
;

INFILE  "C:/Users/Christian Santizo/Google Drive/CSULB/Courses/STAT 560/Project/monthly_net_earnings.txt" 
     DSD 
     LRECL= 25 ;
INPUT
 month
 m_net_earnings
 N
;
LABEL  m_net_earnings = "m.net.earnings" ;
FORMAT month yymmdd10.;
RUN;
