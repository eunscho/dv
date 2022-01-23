
widaman3c <- ' M1 =~ NA*ThrA + ThrB + ThrC; M2 =~ NA*LktA + LktB + LktC; 
               M3 =~ NA*GttA + GttB + GttC; M4 =~ NA*SlfA + SlfB + SlfC;
               TA =~ NA*ThrA + LktA + GttA + SlfA;
               TB =~ NA*ThrB + LktB + GttB + SlfB;
               TC =~ NA*ThrC + LktC + GttC + SlfC;
               M1 ~~ 1*M1; M2 ~~ 1*M2; M3 ~~ 1*M3; M4 ~~ 1*M4;
               TA ~~ 1*TA; TB ~~ 1*TB; TC ~~ 1*TC
               M1 ~~ 0*TA; M1 ~~ 0*TB; M1 ~~ 0*TC;
               M2 ~~ 0*TA; M2 ~~ 0*TB; M2 ~~ 0*TC;
               M3 ~~ 0*TA; M3 ~~ 0*TB; M3 ~~ 0*TC;
               M4 ~~ 0*TA; M4 ~~ 0*TB; M4 ~~ 0*TC;
               SlfA ~~ a*SlfA; a > 0;
'
widaman3cneg <- ' M1 =~ NA*ThrA + ThrB + ThrC; M2 =~ NA*LktA + LktB + LktC; 
               M3 =~ NA*GttA + GttB + GttC; M4 =~ NA*SlfA + SlfB + SlfC;
               TA =~ NA*ThrA + LktA + GttA + SlfA;
               TB =~ NA*ThrB + LktB + GttB + SlfB;
               TC =~ NA*ThrC + LktC + GttC + SlfC;
               M1 ~~ 1*M1; M2 ~~ 1*M2; M3 ~~ 1*M3; M4 ~~ 1*M4;
               TA ~~ 1*TA; TB ~~ 1*TB; TC ~~ 1*TC
               M1 ~~ 0*TA; M1 ~~ 0*TB; M1 ~~ 0*TC;
               M2 ~~ 0*TA; M2 ~~ 0*TB; M2 ~~ 0*TC;
               M3 ~~ 0*TA; M3 ~~ 0*TB; M3 ~~ 0*TC;
               M4 ~~ 0*TA; M4 ~~ 0*TB; M4 ~~ 0*TC;
'

widaman2c <- ' M1 =~ NA*ThrA + ThrB + ThrC; M2 =~ NA*LktA + LktB + LktC; 
               M3 =~ NA*GttA + GttB + GttC; M4 =~ NA*SlfA + SlfB + SlfC;
               T =~ NA*ThrA + LktA + GttA + SlfA + ThrB + LktB + GttB + SlfB + ThrC + LktC + GttC + SlfC;
               M1 ~~ 1*M1; M2 ~~ 1*M2; M3 ~~ 1*M3; M4 ~~ 1*M4;
               T ~~ 1*T; 
               M1 ~~ 0*T; 
               M2 ~~ 0*T; 
               M3 ~~ 0*T; 
               M4 ~~ 0*T;
               GttB ~~ a*GttB; a > 0;
               
'

fit.widaman3c <- cfa(widaman3c, sample.cov = as.matrix(ostrom12), sample.nobs = 189)
lavInspect(fit.widaman3c, what = "fit")[3:5]
lavInspect(fit.widaman3c, what = "est")
summary(fit.widaman3c)

lavaanPlot(model = fit.widaman3c)


fit.widaman3cneg <- cfa(widaman3cneg, sample.cov = as.matrix(ostrom12), sample.nobs = 189)
lavInspect(fit.widaman3cneg, what = "fit")[3:5]
lavInspect(fit.widaman3cneg, what = "est")

fit.widaman2c <- cfa(widaman2c, sample.cov = as.matrix(ostrom12), sample.nobs = 189)
lavInspect(fit.widaman2c, what = "fit")[3:5]
summary(fit.widaman2c)
