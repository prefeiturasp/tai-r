# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Funcao para RECEBER as RESPOSTAS e os PARAMETROS DOS ITENS, e então:
# 1. estimar a proficiência
# 2. Definir se o processo allcancou um criterio de parada
# 3. Se não parou, DEVOLVER o Codigo do PROXIMO ITEM
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



EAP=function(U,PAR, administrado){
  #U = respostas
  U = t(matrix(U))
  #PAR = PAR[1:3,]
  PAR <- PAR
  #PAR[,2] <- (PAR[,2]-249.985)/55.093
  #administrado = c(1,2)
  
  naoNA=which(!is.na(U))
  It=length(naoNA)
  
  nj=1; #It=length(U)  
  #U=U[naoNA]; dim(U)=c(1,It)
  
  #seq=order(administrado[which(administrado>0)])
  #seq=order(administrado)[(nrow(PAR)-It+1):nrow(PAR)]; print(seq)
  #U=U[seq]; dim(U)=c(1,It)
  #PAR=PAR[seq,]; #print(PAR); print(U); #print(cbind(PAR,t(U)))
  
  #if (is.null(dim(U))) nj=1 else nj=nrow(U) # Numero de linhas de U
  #It = ncol(U)
  
  q=61 # Numero de pontos de quadratura    length(Xr)
  Xr=seq(-6,6,12/(q-1)); dim(Xr)=c(q,1)  # q x 1 # Pontos de quadratura
  AXr=1/sqrt(2*pi)*exp(-(Xr^2)/2)*8/(q-1) # q x 1
  
  #PAR2 <- PAR[1:2,]
  a=PAR[,1]; b=as.numeric(PAR[,2]); c=as.numeric(PAR[,3]); #print(a); print(b); print(c)
  ak2=matrix(1,nrow=q)# ; bk=matrix(t(b),It,q,1) ; ck=matrix(t(c),It,q,1) ;  # q x I
  ak=kronecker(ak2,t(a))
  bk=kronecker(ak2,t(b))
  ck=kronecker(ak2,t(c))
  
  
  Xrk=matrix(Xr,q,length(a)) # replicando Xr em I colunas # q x I
  P=ck+(1-ck)/(1+exp(-ak*(Xrk-bk))) ; #P: matriz de probabilidades # n x I
  Pjt=matrix(NA,q,1)
  theta_est=integer(nj)  
  for (j in 1:nj){
    for (l in 1:q){
      veroj=1
      for (i in 1:It)
        if (!is.na(U[j,i])) veroj=veroj*(P[l,i]^U[j,i])*((1-P[l,i])^(1-U[j,i]))
      #temp=((P[l,]^U[j,])*((1-P[l,])^(1-U[j,])))^(1-is.na(Dados01[1,])) # Excliuindo os NA do c?lculo
      Pjt[l]=veroj # L(X1)
    }
    Pj=Pjt*AXr;  # P(Uj) = L(X1)*A(Xr)
    tPj=Xr*Pj; tPj #         XrL(X1)A(Xr)
    theta_est[j]=sum(tPj)/sum(Pj) # SOMA(XrL(X1)A(Xr))/SOMA(L(X1)*A(Xr))
  }
  
  squad = (Xr-theta_est)^2
  ep_est = sqrt(sum(squad*Pj)/sum(Pj))
  cat("\nESTIMACAO: theta_est e ep_est: ", theta_est, ep_est,"\n")
  return (c(theta_est, ep_est))
}


parar_teste=function(theta,theta_erro,pontos_corte,valor_critico=valor_critico){
  parar.teste=0
  theta.range=c(theta-(valor_critico*theta_erro),theta+(valor_critico*theta_erro))
  ff=findInterval(theta.range, pontos_corte) 
  if(length(unique(ff))==1) parar.teste=1
  return(parar.teste)
}


criterio_parada=function(theta_est, theta_ep, parada="EP", EP=EP, n.resp, n.min, validEixo=validEixo, Area, AnoEscolar, proficiencia, AnoEscolarItem="", Habilidade="", Assunto="", SubAssunto="", n.Ij){
  n.Ij <- n.Ij
  area <- Area
  AnoEscolarEstudante <- AnoEscolar
  validEixo <- validEixo
  Niveis_Profic=function(area=Area, AnoEscolarEstudante=AnoEscolar){
    if (area=="LP") {
      if(AnoEscolarEstudante == 2) Niveis=c(-2.722396675,
                                            -2.268618518,
                                            -1.361062204
      )
      if(AnoEscolarEstudante == 3) Niveis=c(-2.268618518,
                                            -1.361062204,
                                            -0.45350589
      )
      if(AnoEscolarEstudante == 4) Niveis=c(-2.087107255,
                                            -1.179550941,
                                            -0.271994627
      )
      if(AnoEscolarEstudante == 5) Niveis=c(-1.814840361,
                                            -0.907284047,
                                            0.000272267
      )
      if(AnoEscolarEstudante == 6) Niveis=c(-1.542573467,
                                            -0.635017153,
                                            0.272539161
      )
      if(AnoEscolarEstudante == 7) Niveis=c(-1.361062204,
                                            -0.45350589,
                                            0.454050424
      )
      if(AnoEscolarEstudante == 8) Niveis=c(-1.179550941,
                                            0.000272267,
                                            0.907828581
      )
      if(AnoEscolarEstudante == 9) Niveis=c(-0.89393831,
                                            0.447935304,
                                            1.342517713
      )
    } 
    if (area=="MT") {
      if(AnoEscolarEstudante == 2) Niveis=c(-2.239742,
                                            -1.343523,
                                            -0.895413
      )
      if(AnoEscolarEstudante == 3) Niveis=c(-1.791633,
                                            -0.895413,
                                            0.0008
      )
      if(AnoEscolarEstudante == 4) Niveis=c(-1.522767,
                                            -0.7161691,
                                            0.2696725
      )
      if(AnoEscolarEstudante == 5) Niveis=c(-1.343523,
                                            -0.4473032,
                                            0.4489164
      )
      if(AnoEscolarEstudante == 6) Niveis=c(-1.074657,
                                            -0.1784373,
                                            0.7177823
      )
      if(AnoEscolarEstudante == 7) Niveis=c(-0.895413,
                                            0.0008,
                                            0.8970262
      )
      if(AnoEscolarEstudante == 8) Niveis=c(-0.7161691,
                                            0.4489164,
                                            1.345136
      )
      if(AnoEscolarEstudante == 9) Niveis=c(-0.4473032,
                                            0.8970262,
                                            1.793246
      )
    }      #Falta colocar os valores
    #if (area=="CI") Niveis=c()
    if (area=="CN") Niveis=c()#Falta colocar os valores
    if (area=="CH") Niveis=c()
    return (Niveis)
  }
  
  
  parada <- parada
  theta <- theta_est# <- -1.636357
  theta_erro <- theta_ep# <- 0.3
  Niveis=Niveis_Profic(area=area)
  pontos_corte <- Niveis
  valor_critico <- 1
  
  
  #cat(theta_est, theta_ep, n.resp, n.min, parada, EP)
  Parada=F
  
  if (n.resp>=n.min){
    if (parada=="EP" & theta_ep<=EP & isTRUE(validEixo)) {
      Parada=T
    }else if (parar_teste(theta,theta_erro,pontos_corte,valor_critico) == 1 & isTRUE(validEixo)) {
      Parada=T
    }else if (n.resp == 32) {
      Parada=T
    }else if (n.resp == n.Ij-2) {
      Parada=T
    }
  }
  
  return (Parada)
}


maxima_informacao_th=function(theta_est=NULL, PAR, D=1){
  if (!is.null(theta_est)) thetaq=theta_est else thetaq=seq(-6,6,.1)
  nq=length(thetaq); #cat("\nNumero de pontos de quadratura: ",nq,"\n")
  #PAR[,1] <- PAR[,1]*55.093
  #PAR[,2] <- (PAR[,2]-249.985)/55.093
  PAR <- PAR
  I=nrow(PAR); #cat("Numero de itens:", I)
  a=PAR[,1]; b=PAR[,2]; c=PAR[,3]; #print(a); print(b); print(c); 
  
  #--- repeticao dos parametros em n linhas de individuos ---#
  ones=rep(1,nq) # vetor coluna de uns (n x 1)
  ax=kronecker(ones,a) ; #ax tem dimensao n x I :  a (1 x I) repetido em n linhas
  bx=kronecker(ones,b) ; #bx tem dimensao n x I :  b (1 x I) repetido em n linhas
  cx=kronecker(ones,c) ; #cx tem dimensao n x I :  c (1 x I) repetido em n linhas
  #dim(ax);dim(bx);dim(cx)
  
  #--- repeticao das habilidades em I colunas de Itens ---#
  ones=matrix(rep(1,I),1,I) # vetor linha de uns (1 x I)
  thetak=kronecker(ones,thetaq) ;
  #cat("\nDimensao de thetak: ", dim(thetak)); 
  #cat("\nDimensao de bx: ", dim(bx)); 
  
  #--- matriz de Probabilidades ---#
  #P=cx+(1-cx)/(1+exp(-D*ax*(thetak-bx))) ; #P: matriz de probabilidades
  P=c+(1-c)/(1+exp(-D*a*(theta_est-b))) ; #P: matriz de probabilidades
  
  #max_info=D^2*ax^2*(1-P)/P*((P-cx)/(1-cx))^2
  max_info=D^2*a^2*(1-P)/P*((P-c)/(1-c))^2; #print(max_info)
  
  return (t(max_info))
} # Maxima Informacao de Fisher para um unico valor de tproficiencia


proximo_item_criterio <- function(INFO, administrado, proficiencia, componente, idItem, Par, eixo, AnoEscolarItem="", Habilidade="", Assunto="", SubAssunto="", n.Ij){
  #INFO[administrado>0]=0 # Para nao selecionar aqueles ja administrados
  
  #Analise dos eixos
  
  
  
  pos = which.max(INFO)
  #proximo = idItem[pos]
  return(pos)
}


TAI=function(respostas=respostas, parada=parada, EP=EP, n.min=n.min, ESTUDANTE=ESTUDANTE, AnoEscolarEstudante=AnoEscolarEstudante, profic.inic=profic.inic, proficiencia=proficiencia, erropadrao=erropadrao, componente=componente, idItem=idItem, gabarito=gabarito, administrado=administrado, parA=parA, parB=parB, parC=parC, AnoEscolarItem=AnoEscolarItem, Habilidade=Habilidade, Assunto=Assunto, SubAssunto=SubAssunto, n.Ij=n.Ij, idEixo=idEixo, idHabilidade=idHabilidade){
  
  
  if(!require("dplyr")) install.packages("dplyr"); library(dplyr)
  
  #cat("respostas: ", parada)
  
  ###### CONVERSAO DOS VALORES DE STRING PARA NUMERICOS  #######
  
  #respostas <- as.integer(strsplit(respostas,","))
  #n.resp=sum(respostas>=0, na.rm=T)
  #EP <- as.numeric(EP)
  #n.min <- as.numeric(n.min)
  #m.tai <- as.numeric(m.tai)
  #ESTUDANTE <- ESTUDANTE#strsplit(noquote(ESTUDANTE),",")     # prepardos inicialmente para um vetor de individuos
  #proficiencia <- as.numeric(strsplit(noquote(proficiencia),",")[[1]])    # prepardos inicialmente para um vetor de individuos
  #AnoEscolarEstudante <- as.numeric(AnoEscolarEstudante)
  #profic.inic <- as.numeric(profic.inic)
  #proficiencia <- as.numeric(strsplit(proficiencia,","))
  #erropadrao <- as.numeric(strsplit(erropadrao,","))
  #idItem <- as.integer(strsplit(idItem,","))
  #print(idItem)
  #gabarito <- strsplit(gabarito,",")
  #administrado <- as.numeric(strsplit(administrado,","))
  #parA <- as.numeric(strsplit(parA,","))
  #parB <- as.numeric(strsplit(parB,","))
  #parC <- as.numeric(strsplit(parC,","))
  #AnoEscolarItem <- as.numeric(strsplit(AnoEscolarItem,","))
  #Habilidade <- as.numeric(strsplit(Habilidade,","))
  #Assunto <- as.numeric(strsplit(Assunto,","))
  #SubAssunto <- as.numeric(strsplit(SubAssunto,","))
  
  
  
  respostas <- as.integer(strsplit(respostas,",")[[1]])
  n.resp=sum(respostas>=0, na.rm=T)
  EP <- as.numeric(EP)
  n.min <- as.numeric(n.min)
  #m.tai <- as.numeric(m.tai)
  ESTUDANTE <- strsplit(noquote(ESTUDANTE),",")[[1]]     # prepardos inicialmente para um vetor de individuos
  #proficiencia <- as.numeric(strsplit(noquote(proficiencia),",")[[1]])    # prepardos inicialmente para um vetor de individuos
  AnoEscolarEstudante <- as.numeric(AnoEscolarEstudante)
  profic.inic <- as.numeric(profic.inic)
  proficiencia <- as.numeric(strsplit(proficiencia,",")[[1]])
  erropadrao <- as.numeric(strsplit(erropadrao,",")[[1]])
  idItem <- as.character(strsplit(idItem,",")[[1]])
  #print(idItem)
  gabarito <- strsplit(gabarito,",")[[1]]
  administrado <- as.character(strsplit(administrado,",")[[1]])
  parA <- as.numeric(strsplit(parA,",")[[1]])
  parB <- as.numeric(strsplit(parB,",")[[1]])
  parC <- as.numeric(strsplit(parC,",")[[1]])
  AnoEscolarItem <- as.numeric(strsplit(AnoEscolarItem,",")[[1]])
  Habilidade <- as.numeric(strsplit(Habilidade,",")[[1]])
  Assunto <- as.numeric(strsplit(Assunto,",")[[1]])
  SubAssunto <- as.numeric(strsplit(SubAssunto,",")[[1]])
  n.Ij <- as.numeric(n.Ij)
  componente <- as.character(componente)
  parada <- parada
  idEixo <- as.numeric(strsplit(idEixo,",")[[1]])
  idHabilidade <- as.numeric(strsplit(idHabilidade,",")[[1]])
  
  #parA <- c("0.035,0.048,0.067,0.038,0.025,0.028,0.022,0.028,0.028,0.025,0.025,0.022,0.028,0.028,0.025,0.025")
  #parB <- c("152.000,200.000,300.000,400.000,241.829,111.467,81.589,76.842,122.574,261.216,302.829,159.589,121.842,301.842,281.29,206.829")
  #parC <- c("0.037,0.057,0.226,0.059,0.170,0.059,0.127,0.184,0.099,0.100,0.170,0.127,0.184,0.184,0.170,0.170")
  #idEixo <- c("6005,6005,6005,6004,6004,6004,6004,6004,6003,6003,6003,6003,6003,6003,6003,6003")
  #idHabilidade <- c("6252,6252,6253,6249,6248,6246,6246,6240,6240,6240,6238,6238,6238,6238,6238,6238")
  #Teste direto
  #ESTUDANTE <- "1723061"
  #AnoEscolarEstudante <- 4
  #profic.inic <- 189.6
  #proficiencia <- 189.6
  #erropadrao <- 0.35
  #idItem <- c("24003974","24003975","24003976","24003977","24003978","24003979","24003980","24003981","24003982","24003983","24003984","24003985","24003986","24003987","24003988","24003989")
  #respostas <- c(9,5,8,9,4,4,6,1)
  #print(idItem)
  #gabarito <- c(9,5,8,9,4,4,6,0)
  #administrado <- c("24003983","24003981")
  #parA <- as.numeric(strsplit(parA,",")[[1]])
  #parB <- as.numeric(strsplit(parB,",")[[1]])
  #parC <- as.numeric(strsplit(parC,",")[[1]])
  #n.Ij <- as.numeric("16")
  #componente <- as.character("Lingua Portuguesa")
  
  
  
  
  
  
  #PAR=data.frame(parA = parA,
  #               parB = parB,
  #               parC = parC)
  #PAR[,1] <- idItem
  PAR <- matrix(ncol= 5, nrow=length(parA))
  PAR[,1] <- parA
  print(parA)
  PAR[,2] <- parB
  print(parB)
  PAR[,3] <- parC# Estimativas dos parametros dos itens
  
  
  
  
  n.itens=length(parA)
  print("1 - teste componente")
  if(componente == "LP" | componente == "Lingua Portuguesa" | componente == "Lingua portuguesa" | componente == "Língua Portuguesa" | componente == "Língua portuguesa" | componente == "lingua portuguesa" | componente == "língua portuguesa" | componente == "lp") {
    componente = "LP"
    #Transformação dos parametros para escala 0 e 1
    PAR[,1] <- PAR[,1]*55.093
    PAR[,2] <- (PAR[,2]-249.985)/55.093
    
  }else if(componente == "MT" | componente == "Matemática" | componente == "Matematica" | componente == "matematica" | componente == "mt") {
    componente = "MT"
    #Transformação dos parametros para escala 0 e 1
    PAR[,1] <- PAR[,1]*55.892
    PAR[,2] <- (PAR[,2]-249.964)/55.892
    
  }else if(componente == "CN" | componente == "Ciências Natureza" | componente == "Ciências da Natureza" | componente == "ciências da natureza" | componente == "ciências natureza" | componente == "cn") {
    componente = "CN" 
    #Transformação dos parametros para escala 0 e 1
    PAR[,1] <- PAR[,1]*55.7899
    PAR[,2] <- (PAR[,2]-249.955)/55.7899
    
  }else {
    componente = "CH"
    #Transformação dos parametros para escala 0 e 1
    PAR[,1] <- PAR[,1]*55.093#Trocar esse valor apos a definição da escala de CH
    PAR[,2] <- (PAR[,2]-249.985)/55.093#Trocar esse valor apos a definição da escala de CH
    
  }
  print(PAR)
  PAR2 <- PAR
  if(length(idEixo) < length(idHabilidade)) {
    PAR[,4] <- rep(idEixo, length(idHabilidade))
    PAR[,5] <- idHabilidade
  }else {
    PAR[,4] <- idEixo
    PAR[,5] <- idHabilidade
  }
  PAR_adm <- PAR
  #n.adm=sum(administrado>0) # Numero de itens ja administrados
  #========================================================
  
  #========================================================
  # OS PRIMEIROS ITENS SAO DE POSICOES DETERMINISTICAS
  if (n.resp==0){
    print("2 - primeiro item")
    #ordem.B=order(parB, decreasing = c(FALSE))
    #parA.ord = parA[ordem.B]
    #parB.ord = parB[ordem.B]
    #parC.ord = parC[ordem.B]
    #idItem <- PAR[,1]
    #PAR <- PAR[,-1]
    
    #idItem.ord=idItem[ordem.B]
    #h = (n.resp+1)*round(n.Ij/(m.tai+1))
    #proximo = idItem.ord[h]
    #pos=which(idItem==proximo) # Posicao do item no BI do examinado 
    
    #Teste de nova metodologia de escolha do primeiro item
    #profic.inic <- 150
    theta_est_ep <- (profic.inic-249.985)/55.093
    INFO = maxima_informacao_th(theta_est_ep, PAR); #print(INFO)
    pos = proximo_item_criterio(INFO, administrado)
    proximo = idItem[pos]
    print("teste 3")
    
    
    
    #administrado <- which(PAR %in% administrado)
    cat("\nRealizada a Selecao (deterministica) do item #", n.resp+1,": ",proximo,"\n")    
    return (c(proximo, n.resp+1, pos, parA[pos], parB[pos], parC[pos], profic.inic, NA))
  } else 
  {
    print("2 - segundo item / montagem do BMI")
    #============  ETAPAS PRINCIPAIS DO TAI  ==============
    cat("\nIniciando a Selecao do item #", n.resp+1,"\n")
    #ordem.B=order(parB, decreasing = c(FALSE))
    #parA.ord = parA[ordem.B]
    #parB.ord = parB[ordem.B]
    #parC.ord = parC[ordem.B]
    #idItem <- PAR[,1]
    
    #corrigir as respostas
    
    respostas <- as.integer(as.logical(respostas == gabarito))
    print(paste("Correção das respostas - Respostas corrigidas:", respostas))
    
    
    print(PAR)
    administrado <- which(idItem %in% administrado)
    print(administrado)
    PAR <- PAR[-administrado,]
    
    
    
    parA <- PAR[,1]
    parB <- PAR[,2]
    parC <- PAR[,3]
    
    if(length(administrado) == 1) {
      PAR3 <- matrix(nrow = 1, ncol = 3)
      PAR3[,1] <- PAR2[administrado,1]
      PAR3[,2] <- PAR2[administrado,2]
      PAR3[,3] <- PAR2[administrado,3]
      PAR4 <- PAR3
      
      validEixo <- TRUE
      
    }else {
      PAR4 <- PAR2[administrado,]
      
      
      #Organizar o arquivo de itens para a selecao do proximo item
      
      
      #library(dplyr)
      eixo <- idEixo
      
      PAR_adm <- data.frame(PAR_adm)
      
      
      colnames(PAR_adm) <- c("par_a", "par_b", "par_c", "idEixo", "idHabilidade")
      PAR_adm$idItem <- idItem
      
      eixos <- unique(eixo)
      if(length(eixos) >= 2) {
        itens_aplicados <- PAR_adm[administrado,]
        itens_aplicados <- itens_aplicados %>%
          group_by(idEixo) %>%
          summarise(quant = n())
        
        valid1 <- which(eixos %in% unique(itens_aplicados$idEixo))
        if(length(valid1) < length(eixos)) {
          PAR_adm <- PAR_adm[-administrado,]
          PAR_adm <- PAR_adm %>% filter(idEixo == eixos[-valid1])
          validEixo <- FALSE
        }else {
          
          itens_aplicados <- PAR_adm[administrado,]
          itens_aplicados <- itens_aplicados %>%
            group_by(idEixo) %>%
            summarise(quant = n())
          
          
          if(length(eixos) < 4) {
            
            if(sum(itens_aplicados$quant) >= length(eixos)*3){
              PAR_adm <- PAR_adm[-administrado,]
              validEixo <- TRUE
            }else {
              itens_aplicados <- itens_aplicados %>%
                group_by(idEixo) %>%
                summarise(quant = n()) %>%
                filter(quant < 3)
              
              valid2 <- which(eixos %in% unique(itens_aplicados$idEixo))
              PAR_adm <- PAR_adm[-administrado,]
              PAR_adm <- PAR_adm %>% filter(idEixo == eixos[valid2])
              
              
              if(length(administrado) >= 8) {
                PAR_adm <- PAR_adm[-administrado,]
                validEixo <- FALSE
              }
              
              
            }
            
          }else {
            if(sum(itens_aplicados$quant) >= length(eixos)*2) {
              PAR_adm <- PAR_adm[-administrado,]
              validEixo <- TRUE
            }else {
              itens_aplicados <- itens_aplicados %>%
                group_by(idEixo) %>%
                summarise(quant = n()) %>%
                filter(quant < 2)
              
              valid2 <- which(eixos %in% unique(itens_aplicados$idEixo))
              PAR_adm <- PAR_adm[-administrado,]
              PAR_adm <- PAR_adm %>% filter(idEixo == eixos[valid2])
              
              if(length(administrado) >= 8) {
                PAR_adm <- PAR_adm[-administrado,]
                validEixo <- FALSE
              }
              
            }
          }
          
          
        }
        
      }else {
        PAR_adm <- PAR_adm[-administrado,]
        
        validEixo <- TRUE
        
      }
      
      
    }
    
    
    print(PAR)
    print(validEixo)
    print("3 - Estimativa da proficiencia")
    theta_est_ep = EAP(respostas, PAR=PAR4, administrado) # vetor (thesta_est, theta-ep)
    
    #cat("\nEstimativas: ", theta_est_ep); #cat()
    print("4 - Validar o criterio de parada")
    print(AnoEscolarEstudante)
    parar = criterio_parada(theta_est=theta_est_ep[1], theta_ep=theta_est_ep[2], Area=componente, AnoEscolar = AnoEscolarEstudante, parada=parada, EP=EP, n.resp=n.resp, n.min=n.min, n.Ij = n.Ij, validEixo = validEixo)
    print(parar)
    if (!parar){
      #cat("\nProcesso continua...")
      INFO = maxima_informacao_th(theta_est_ep[1], PAR); #print(INFO)
      print(INFO)
      pos = proximo_item_criterio(INFO, administrado, Par = PAR_adm, eixo=idEixo, idItem = idItem)
      print(pos)
      idItem <- idItem[-administrado]
      print(idItem)
      proximo = idItem[pos]
      teste <- PAR[pos,]
      print(teste)
      cat("\nRealizada a Selecao (Maxima Informacao de Fisher) do item #", n.resp+1,": ",proximo,"\n")  
      
      #Transformar as proficiencias com base na escala
      
      if(componente == "LP") {
        theta_est_ep[1] <- theta_est_ep[1]*55.093 + 249.985
        theta_est_ep[2] <- theta_est_ep[2]*55.093
      }else if(componente == "MT") {
        theta_est_ep[1] <- theta_est_ep[1]*55.892 + 249.964
        theta_est_ep[2] <- theta_est_ep[2]*55.892
      }else if(componente == "CN") {
        theta_est_ep[1] <- theta_est_ep[1]*55.789 + 249.955
        theta_est_ep[2] <- theta_est_ep[2]*55.789
      }else {
        theta_est_ep[1] <- theta_est_ep[1]*55.093 + 249.985#Preciso modificar quando tiver a escala de CH
        theta_est_ep[2] <- theta_est_ep[2]*55.093
      }
      
      
      
      return (c(proximo, n.resp+1, pos, parA[pos], parB[pos], parC[pos], theta_est_ep))
    }
    
    if (parar) {
      cat("\nCRITERIO DE PARADA ALCANCADO COM ",n.resp," ITENS.\n\n"); 
      proximo=-1
      if(componente == "LP") {
        theta_est_ep[1] <- theta_est_ep[1]*55.093 + 249.985
        theta_est_ep[2] <- theta_est_ep[2]*55.093
      }else if(componente == "MT") {
        theta_est_ep[1] <- theta_est_ep[1]*55.892 + 249.964
        theta_est_ep[2] <- theta_est_ep[2]*55.892
      }else if(componente == "CN") {
        theta_est_ep[1] <- theta_est_ep[1]*55.789 + 249.955
        theta_est_ep[2] <- theta_est_ep[2]*55.789
      }else {
        theta_est_ep[1] <- theta_est_ep[1]*55.093 + 249.985#Preciso modificar quando tiver a escala de CH
        theta_est_ep[2] <- theta_est_ep[2]*55.093
      }
      return (c(proximo, NA, NA, NA, NA, NA, theta_est_ep))
    }
    
    
  }
  
  
  
}



#* @apiTitle API do TAI-PSP para Aplicacao do TAI
#* @apiDescription Essa API possibilita que a aplicacao Serap-Estudante ou qualquer outra se comunique com as funcoes que compoem o TAI


#Vetor com os codigos dos estudantes
#* @param ESTUDANTE
#* @param AnoEscolarEstudante

#respectivas proficiencias
#* @param proficiencia
#* 

#Area/Componente de Avaliacao (MT, LP, CI ...)
#* @param componente

#Caracteristicas dos itens
#* @param idItem
#* @param parA
#* @param parB
#* @param parC
#* @param AnoEscolarItem
#* @param Habilidade
#* @param Assunto
#* @param SubAssunto


#Quantidade de itens na amostra
#* @param n.Ij

#* @post /proximo
#* 
#* 


function(respostas, parada="EP", EP=0.35, n.min=c(8), ESTUDANTE, AnoEscolarEstudante, profic.inic, proficiencia, erropadrao, componente, idItem, gabarito, administrado, parA, parB, parC, AnoEscolarItem="", Habilidade="", Assunto="", SubAssunto="", idEixo, idHabilidade, n.Ij, MultiEstagio=1, TxExp=F, Intervalo=F, Tema=F, Eixo.Cog=F, Eixo.Con=F, Pular=0, Voltar=0, Dif.cresc=F, PARAR=F, SIMUL=F){
  
  proximo <- TAI(respostas, parada=parada, EP=EP, n.min=n.min, ESTUDANTE=ESTUDANTE, AnoEscolarEstudante=AnoEscolarEstudante, profic.inic=profic.inic, proficiencia=proficiencia, erropadrao=erropadrao, componente=componente, idItem=idItem, gabarito=gabarito, administrado=administrado, parA=parA, parB=parB, parC=parC, AnoEscolarItem=AnoEscolarItem, Habilidade=Habilidade, Assunto=Assunto, SubAssunto=SubAssunto, n.Ij=n.Ij, idEixo=idEixo, idHabilidade = idHabilidade)
  
  return(proximo)
}

#* @post /pingR
#* 
#* 

function() {
  pingR <- "200"
  return(pingR)
}