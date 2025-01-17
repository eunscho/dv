analyze_dat <- function(conditions, condition_number, rep_set, rep, data) {
  library(lavaan)
  n <- as.integer(conditions[condition_number, 1])
  fcor <- as.double(conditions[condition_number, 2])
  t <- 3
  m <- 3
  k <- 3
  ###############################################################################
  # Campbell and Fiske (1959) original techniques (using observed correlations)
  ###############################################################################
  # Campbell and Fiske's (1959) hthm vs. mthm criterion
  cmp_mat <- campbell_matrix(data, t, m, k)
  hthm_mthm <- campbell_comp(cmp_mat$hthm, cmp_mat$mthm)
  hthm_viol_ratio <- hthm_mthm$nviol / hthm_mthm$ncomp # violation ratio
  hthm_0tol <- ifelse(hthm_viol_ratio > 0, 1, 0) # 1: dv problem, 0: no problem, 0tol: zero tolerance
  hthm_5tol <- ifelse(hthm_viol_ratio > .05, 1, 0) # 5tol: five per cent tolerance for violation
  hthm_10tol <- ifelse(hthm_viol_ratio > .1, 1, 0) # 10tol: ten per cent tolerance for violation
  
  # Campbell and Fiske's (1959) htmm vs. mthm criterion
  htmm_mthm <- campbell_comp(cmp_mat$htmm, cmp_mat$mthm)
  htmm_viol_ratio <- htmm_mthm$nviol / htmm_mthm$ncomp
  htmm_10tol <- ifelse(htmm_viol_ratio > .1, 1, 0) 
  htmm_20tol <- ifelse(htmm_viol_ratio > .2, 1, 0) 
  htmm_30tol <- ifelse(htmm_viol_ratio > .3, 1, 0) 
  
  # Campbell and Fiske's (1959) htmm & mthm pattern criterion
  pattern <- campbell_pattern(cmp_mat$hthm, cmp_mat$htmm) # works only if t = 3 and m = 3
 
  # Campbell and Fiske's (1959) htmm vs. reliability criterion
  htmm_rel <- campbell_comp(cmp_mat$htmm, cmp_mat$rel)
  rel_viol_ratio <- htmm_rel$nviol / htmm_rel$ncomp
  rel_0tol <- ifelse(rel_viol_ratio > 0, 1, 0) 
  
  ###############################################################################
  # Bagozzi's (1978) and  Widaman's (1985) CFA techniques 
  # The trait-method unit, which is the sum of the item scores, is the unit of analysis.
  ###############################################################################
  cdata <- composite(data, t, m, k) # transform items scores to composite scores
  # ctcm: correlated traits correlated methods 
  model_ctcm_unconstrained <- 'T1 =~ NA * t1m1 + t1m2 + t1m3
                 T2 =~ NA * t2m1 + t2m2 + t2m3
                 T3 =~ NA * t3m1 + t3m2 + t3m3
                 M1 =~ NA * t1m1 + t2m1 + t3m1
                 M2 =~ NA * t1m2 + t2m2 + t3m2
                 M3 =~ NA * t1m3 + t2m3 + t3m3
                 T1 ~~ 1 * T1; T2 ~~ 1 * T2; T3 ~~ 1 * T3
                 M1 ~~ 1 * M1; M2 ~~ 1 * M2; M3 ~~ 1 * M3
                 T1 ~~ 0 * M1; T1 ~~ 0 * M2; T1 ~~ 0 * M3
                 T2 ~~ 0 * M1; T2 ~~ 0 * M2; T2 ~~ 0 * M3
                 T3 ~~ 0 * M1; T3 ~~ 0 * M2; T3 ~~ 0 * M3
                 T1 ~~ a12 * T2; T1 ~~ a13 * T3; T2 ~~ a23 * T3
                 M1 ~~ b12 * M2; M1 ~~ b13 * M3; M2 ~~ b23 * M3
                 a13 > -1; a13 < 1; a23 > -1; a23 < 1 
                 b12 > -1; b12 < 1; b13 > -1; b13 < 1; b23 > -1; b23 < 1
                 t1m1 ~~ c11 * t1m1; t1m2 ~~ c12 * t1m2; t1m3 ~~ c13 * t1m3
                 t2m1 ~~ c21 * t2m1; t2m2 ~~ c22 * t2m2; t2m3 ~~ c23 * t2m3
                 t3m1 ~~ c31 * t3m1; t3m2 ~~ c32 * t3m2; t3m3 ~~ c33 * t3m3
                 c11 > 0; c12 > 0; c13 > 0; c21 > 0; c22 > 0; c23 >0
                 c31 > 0; c32 > 0; c33 >0;'
  model_ctcm <- paste0(model_ctcm_unconstrained, "a12 > -1; a12 < 1")
  fit_ctcm <- cfa(model = model_ctcm, data = cdata) 
  if (lavInspect(fit_ctcm, what = "converged")) {
    ctcm_converged <- 1
    ctcm_chi2 <- ifelse(lavInspect(fit_ctcm, what = "fit")["pvalue"] < .05, 1, 0)
  }  else {
    ctcm_converged <- 0
    ctcm_chi2 <- NA
  }
  # stcm: single traits correlated methods
  model_stcm <- 'T1 =~ NA * t1m1 + t1m2 + t1m3
                 T2 =~ NA * t2m1 + t2m2 + t2m3
                 T3 =~ NA * t3m1 + t3m2 + t3m3
                 M1 =~ NA * t1m1 + t2m1 + t3m1
                 M2 =~ NA * t1m2 + t2m2 + t3m2
                 M3 =~ NA * t1m3 + t2m3 + t3m3
                 T1 ~~ 1 * T1; 
                 T1 ~~ 1 * T2; T1 ~~ 1 * T3; T2 ~~ 1 * T3
                 M1 ~~ 1 * M1; M2 ~~ 1 * M2; M3 ~~ 1 * M3
                 T1 ~~ 0 * M1; T1 ~~ 0 * M2; T1 ~~ 0 * M3
                 M1 ~~ b12 * M2; M1 ~~ b13 * M3; M2 ~~ b23 * M3
                 b12 > -1; b12 < 1; b13 > -1; b13 < 1; b23 > -1; b23 < 1
                 t1m1 ~~ c11 * t1m1; t1m2 ~~ c12 * t1m2; t1m3 ~~ c13 * t1m3
                 t2m1 ~~ c21 * t2m1; t2m2 ~~ c22 * t2m2; t2m3 ~~ c23 * t2m3
                 t3m1 ~~ c31 * t3m1; t3m2 ~~ c32 * t3m2; t3m3 ~~ c33 * t3m3
                 c11 > 0; c12 > 0; c13 > 0; c21 > 0; c22 > 0; c23 >0
                 c31 > 0; c32 > 0; c33 >0'
  fit_stcm <- cfa(model = model_stcm, data = cdata) 
  if (lavInspect(fit_stcm, what = "converged")) {
    stcm_converged <- 1
    stcm_chi2 <- ifelse(lavInspect(fit_stcm, what = "fit")["pvalue"] > .05, 1, 0)
  }  else {
    stcm_converged <- 0
    stcm_chi2 <- NA
  }
  
  if (ctcm_converged & stcm_converged) {
    widaman_dif <- semTools::compareFit(fit_ctcm, fit_stcm, nested = TRUE)
    widaman_dif_p <- ifelse(widaman_dif@nested$`Pr(>Chisq)`[2] > .05, 1, 0)
    widaman_dif_nfi <- ifelse(widaman_dif@fit.diff$nfi < .01, 1, 0)
    widaman_dif_tli <- ifelse(widaman_dif@fit.diff$tli < .01, 1, 0)
  } else {
    widaman_dif_p <- NA
    widaman_dif_nfi <- NA
    widaman_dif_tli <- NA
  }
  ###############################################################################
  # CICFA, Chi-square technique using composite scores
  ###############################################################################
  cutoff <- 1
  if (ctcm_converged) {
    c_point_estimate <- lavInspect(fit_ctcm, what = "est")$psi[2,1]
    c_se <- lavInspect(fit_ctcm, what ="se")$psi[2,1]
    c_upper <- c_point_estimate + 1.96 * c_se
    c_cicfa <- ifelse(c_upper > cutoff, 1, 0)
  } else {
    c_cicfa <- NA
  }
  model_ctcm_constrained <- paste0(model_ctcm_unconstrained, "a12 == 1")
  fit_ctcm_const <- cfa(model = model_ctcm_constrained, data = cdata)
  if( lavInspect(fit_ctcm_const, what = "converged")) {
    const_dif <- semTools::compareFit(fit_ctcm, fit_ctcm_const, nested = TRUE)
    c_chisqdif <- ifelse(const_dif@nested$`Pr(>Chisq)`[2] > .05, 1, 0)
  } else {
    c_chisqdif <- NA
  }
  ###############################################################################
  # CICFA, Chi-square technique using item scores
  ###############################################################################
  model_i_unconstrained <- 'T1 =~ NA * t1m1_1 + t1m1_2 + t1m1_3 + t1m2_1 + t1m2_2 + t1m2_3 + t1m3_1 + t1m3_2 + t1m3_3
                 T2 =~ NA * t1m1_1 + t2m1_2 + t2m1_3 + t2m2_1 + t2m2_2 + t2m2_3 + t2m3_1 + t2m3_2 + t2m3_3
                 T3 =~ NA * t3m1_1 + t3m1_2 + t3m1_3 + t3m2_1 + t3m2_2 + t3m2_3 + t3m3_1 + t3m3_2 + t3m3_3
                 M1 =~ NA * t1m1_1 + t1m1_2 + t1m1_3 + t2m1_1 + t2m1_2 + t2m1_3 + t3m1_1 + t3m1_2 + t3m1_3
                 M2 =~ NA * t1m2_1 + t1m2_2 + t1m2_3 + t2m2_1 + t2m2_2 + t2m2_3 + t3m2_1 + t3m2_2 + t3m2_3
                 M3 =~ NA * t1m3_1 + t1m3_2 + t1m3_3 + t2m3_1 + t2m3_2 + t2m3_3 + t3m3_1 + t3m3_2 + t3m3_3
                 T1 ~~ 1 * T1; T2 ~~ 1 * T2; T3 ~~ 1 * T3
                 M1 ~~ 1 * M1; M2 ~~ 1 * M2; M3 ~~ 1 * M3
                 T1 ~~ 0 * M1; T1 ~~ 0 * M2; T1 ~~ 0 * M3
                 T2 ~~ 0 * M1; T2 ~~ 0 * M2; T2 ~~ 0 * M3
                 T3 ~~ 0 * M1; T3 ~~ 0 * M2; T3 ~~ 0 * M3
                 T1 ~~ a12 * T2; T1 ~~ a13 * T3; T2 ~~ a23 * T3
                 M1 ~~ b12 * M2; M1 ~~ b13 * M3; M2 ~~ b23 * M3
                 a13 > -1; a13 < 1; a23 > -1; a23 < 1 
                 b12 > -1; b12 < 1; b13 > -1; b13 < 1; b23 > -1; b23 < 1
                 t1m1_1 ~~ c111 * t1m1_1; t1m1_2 ~~ c112 * t1m1_2; t1m1_3 ~~ c113 * t1m1_3;
                 t1m2_1 ~~ c121 * t1m2_1; t1m2_2 ~~ c122 * t1m2_2; t1m2_3 ~~ c123 * t1m2_3;  
                 t1m3_1 ~~ c131 * t1m3_1; t1m3_2 ~~ c132 * t1m3_2; t1m3_3 ~~ c133 * t1m3_3;
                 t2m1_1 ~~ c211 * t2m1_1; t2m1_2 ~~ c212 * t2m1_2; t2m1_3 ~~ c213 * t2m1_3;    
                 t2m2_1 ~~ c221 * t2m2_1; t2m2_2 ~~ c222 * t2m2_2; t2m2_3 ~~ c223 * t2m2_3;  
                 t2m3_1 ~~ c231 * t2m3_1; t2m3_2 ~~ c232 * t2m3_2; t2m3_3 ~~ c233 * t2m3_3;
                 t3m1_1 ~~ c311 * t3m1_1; t3m1_2 ~~ c312 * t3m1_2; t3m1_3 ~~ c313 * t3m1_3;  
                 t3m2_1 ~~ c321 * t3m2_1; t3m2_2 ~~ c322 * t3m2_2; t3m2_3 ~~ c323 * t3m2_3;  
                 t3m3_1 ~~ c331 * t3m3_1; t3m3_2 ~~ c332 * t3m3_2; t3m3_3 ~~ c333 * t3m3_3;
                 c111 > 0; c112 > 0; c113 > 0;  c121 > 0; c122 > 0; c123 > 0; 
                 c131 > 0; c132 > 0; c133 > 0;  c211 > 0; c212 > 0; c213 > 0; 
                 c221 > 0; c222 > 0; c223 > 0;  c231 > 0; c232 > 0; c233 > 0;
                 c311 > 0; c312 > 0; c313 > 0;  c321 > 0; c322 > 0; c323 > 0;  
                 c331 > 0; c332 > 0; c333 > 0;'
  model_i <- paste0(model_i_unconstrained, "a12 > -1; a12 < 1")
  fit_i <- cfa(model = model_i, data = data) 
  model_i_const <- paste0(model_i_unconstrained, "a12 == 1")
  fit_i_const <- cfa(model_i_const, data)
  if (lavInspect(fit_i, "converged")) {
    i_point_estimate <- lavInspect(fit_i, what = "est")$psi[2,1]
    i_se <- lavInspect(fit_i, what ="se")$psi[2,1]
    i_upper <- i_point_estimate + 1.96 * i_se
    i_cicfa <- ifelse(i_upper > cutoff, 1, 0)
  } else {
    i_cicfa <- NA
  }
  if( lavInspect(fit_i_const, what = "converged")) {
    i_const_dif <- semTools::compareFit(fit_i, fit_i_const, nested = TRUE)
    i_chisqdif <- ifelse(i_const_dif@nested$`Pr(>Chisq)`[2] > .05, 1, 0)
  } else {
    i_chisqdif <- NA
  }
  ###############################################################################
  # Report output
  ###############################################################################
  out <- data.frame(condition_number = condition_number,
              rep_set = rep_set,
              rep = rep,
              n = n,
              fcor = fcor,
              hthm_0tol = hthm_0tol,
              hthm_5tol = hthm_5tol,
              hthm_10tol = hthm_10tol,
              htmm_10tol = htmm_10tol,
              htmm_20tol = htmm_20tol,
              htmm_30tol = htmm_30tol,
              rel_0tol = rel_0tol,
              pattern = pattern,
              #ctcm_converged <- ctcm_converged,
              ctcm_chi2 = ctcm_chi2,
              #stcm_converged <- stcm_converged,
              #stcm_chi2 = stcm_chi2,
              widaman_dif_p = widaman_dif_p,
              widaman_dif_nfi = widaman_dif_nfi,
              widaman_dif_tli = widaman_dif_tli,
              c_cicfa = c_cicfa,
              c_chisqdif = c_chisqdif,
              i_cicfa = i_cicfa,
              i_chisqdif = i_chisqdif
              )
  return(out)
}