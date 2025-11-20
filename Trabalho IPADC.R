#Projeto IPADC

#Função
# tcl = total cholesterol, sbp = systolic blood pressure, treated = treatment for hbp
risco_estimado = function(sex,age,treated,tcl,hdl,sbp,smoker,diabetic) {
   if (sex == 0) { #Woman
     S0 = 0.95012
     if (treated == 0) {
       logtreated = 2.76157
     } else {
       logtreated = 2.82263
     }
     
     soma = 2.32888*log(age) + 1.20904*log(tcl) - 0.70833*log(hdl) +
       logtreated*log(sbp) + 0.52873*smoker + 0.69154*diabetic
     risco = (1 - S0^exp(soma-26.1931))*100
    
     return(round(risco,digits=1))
    
   } else { #Men
     S0 = 0.88936
     if (treated == 0) {
       logtreated = 1.93303 
     } else {
       logtreated = 1.99881
     }
     
     soma = 3.06117*log(age) + 1.12370*log(tcl) - 0.93263*log(hdl) +
       logtreated*log(sbp) + 0.65451*smoker + 0.57367*diabetic
     risco = (1 - S0^exp(soma-23.9802))*100
     
     return(round(risco,digits=1))
   }
}  

risco_estimado(0,61,0,180,47,124,1,0)
risco_estimado(1,53,1,161,55,125,0,1)
