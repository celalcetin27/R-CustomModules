v1=read.table("C:/Users/Berkay/OneDrive/Masaüstü/DatasetNA.txt", header=T, dec = ",")
#uzunluk fonksiyonu
uzunluk <- function(...) {
  sonuçlar <- sapply(list(...), function(örnek) {
    if (is.data.frame(örnek)) {
      z <- colnames(örnek)
      boy <- 0
      for (c in z) {
        if (!is.na(c)) {
          boy <- boy + 1
        }
      }
      return(boy)
    } 
    else if (is.list(örnek)){
      boy <- 0
      
      for (eleman in örnek) {
        boy <- boy + 1
      }
      return(boy)
    }
    else {
      örnek[is.na(örnek)] <- 0
      z <- length(örnek)
      boy <- 0
      for (x in 1:z) {
        if (is.na(örnek[x])) {
          break
        }
        if (örnek[x] != 0) {
          boy <- boy + 1
        }
      }
      return(boy)
    }
  })
  
  return(sonuçlar)
}
#minhesap fonksiyonu
minhesap <- function(...) {
  örnek <- data.frame(...)
  sonuçlar <- c(1)
  for (i in 1:uzunluk(örnek)) {
    x <- Inf
    for (k in 1:uzunluk(örnek[[i]])) {
      for (l in 1:uzunluk(örnek[[i]])) {
        if (!is.na(örnek[[i]][k]) && !is.na(örnek[[i]][l]) && örnek[[i]][k] < örnek[[i]][l] && örnek[[i]][k] < x) {
          x <- örnek[[i]][k]
        }
      }
    }
    sonuçlar[i] <- x
  }
  return(sonuçlar)
}
#makshesapfonksiyonu
maxhesap<- function(...) {
  örnek <- data.frame(...)
  sonuçlar <- c(1)
  for (i in 1:uzunluk(örnek)) {
    x <- -Inf
    for (k in 1:uzunluk(örnek[[i]])) {
      for (l in 1:uzunluk(örnek[[i]])) {
        if (!is.na(örnek[[i]][k]) && !is.na(örnek[[i]][l]) && örnek[[i]][k] > örnek[[i]][l] && örnek[[i]][k] > x) {
          x <- örnek[[i]][k]
        }
      }
    }
    sonuçlar[i] <- x
  }
  
  return(sonuçlar)
}
#medyan hesabı fonksiyonu
medianhesap <- function(...) {
  örnekler <- data.frame(...)
  sonuçlar <- numeric(uzunluk(örnekler))
  
  for (i in 1:uzunluk((örnekler))) {
    örnek <- örnekler[[i]]
    örnek2 <- numeric(uzunluk(örnek))
    y <- 1
    nauzun<-uzunluk(is.na(örnek))
    uzun <- uzunluk(örnek)
    uzun<- nauzun+uzun
    
    for (j in 1:uzun) {
      if (!is.na(örnek[j])) {
        örnek2[y] <- örnek[j]
        y <- y + 1
      }
    }
    
    örnek2 <- örnek2[1:y-1]
    
    for (l in 1:uzunluk(örnek2)) {
      for (m in 1:uzunluk(örnek2)) {
        if (örnek2[m] <= örnek2[l]) {
          t <- örnek2[l]
          örnek2[l] <- örnek2[m]
          örnek2[m] <- t
        }
      }
    }
    
    if (uzunluk(örnek2) %% 2 == 0) {
      a <- uzunluk(örnek2) / 2
      b <- (uzunluk(örnek2) / 2) + 1
      x <- (örnek2[a] + örnek2[b]) / 2
    } else {
      a <- (uzunluk(örnek2) / 2) + 0.5
      x <- örnek2[a]
    }
    sonuçlar[i] <- x
  }
  return(sonuçlar)
}
#yüzde değer bulma fonksiyonu
own_quant <- function(df, ratio){
  a <- sort(df)
  n <- uzunluk(a)
  new <- ceiling(n * ratio)
  return(a[new])
}



theboxplot <- function(df, Maintitle, xlimit, ylimit, xlabel, ylabel, color){
  
  vars <- df[!is.na(df)]
  medyan <- medianhesap(vars)
  
  lower_quarter <- own_quant(vars, 0.25)
  upper_quarter <- own_quant(vars, 0.75)
  
  own_iqr <- 1.5*(upper_quarter - lower_quarter)
  
  lower_limit <- max(minhesap(vars), lower_quarter - own_iqr)
  upper_limit <- min(maxhesap(vars), upper_quarter + own_iqr)
  outlier <- vars[vars < lower_limit | vars > upper_limit]
  plot.new()
  if (is.null(xlimit) && is.null(ylimit)){
    plot.window (xlim = c(0,2), ylim=c(floor(minhesap(vars)+0.5) ,floor(maxhesap(vars)+0.5 )))
  } else if (is.null(xlimit)) {
    plot.window (xlim =c (0,2), ylim = ylimit) 
  } else if(is.null(ylimit)) {
    plot.window (xlim = xlimit, ylim=c(floor(minhesap(vars)+0.5),floor(maxhesap(vars)+0.5)))
  }else{
    plot.window (xlim = xlimit, ylim = ylimit)
  }
  
  if(is.null(color)) {
    rect(1-0.2, lower_quarter, 1+0.2, medyan, border="black", col = "grey")
    rect(1-0.2, medyan, 1+0.2, upper_quarter, border="black", col = "grey")
  }else{
    rect(1-0.2, lower_quarter, 1+0.2, medyan, border="black", col = color)
    rect(1-0.2, medyan, 1+0.2, upper_quarter, border="black", col = color)
  }
  
  if(is.null(Maintitle)) {
    Maintitle <- deparse(substitute(df))
    title(main = Maintitle)
  }else{
    title(main = Maintitle)
  }
  
  
  segments(1-0.2, medyan, 1+0.2, medyan, lwd=3)
  
  segments(1, lower_quarter, 1, minhesap(vars[vars >= lower_limit]), lwd=2, lty="dashed")
  segments(1+0.1, minhesap(vars[vars >= lower_limit]), 1-0.1, minhesap(vars[vars >= lower_limit]), lwd=2)
  segments(1, upper_quarter, 1, maxhesap(vars[vars <= upper_limit]), lwd=2, lty="dashed")
  segments(1+0.1, maxhesap(vars[vars <= upper_limit]), 1-0.1, maxhesap(vars[vars <= upper_limit]), lwd=2)
  
  points(rep(1, length(outlier)), outlier, pch=19)
  
  if(is.null(xlabel)) {
    mtext("xlabel", side = 1, line = 2.7)
  }else{
    mtext(xlabel, side = 1, line = 2.7)
  }
  if(is.null(ylabel)) {
    mtext("ylabel", side = 2, line = 2.7)
  }else{
    mtext(ylabel, side = 2, line = 2.7)
  }
  
  box()
  axis(1)
  axis(2)
  
}
#theboxplot kullanım şekilleri
theboxplot(df = v1$Var1 , Maintitle = NULL , xlimit = NULL ,ylimit = NULL , xlabel= NULL ,ylabel = NULL ,color = NULL)
theboxplot(df = v1$Var1 , Maintitle = "Maintitle" , xlimit = c(0, 2) ,ylimit = c(3,5) , ylabel = "Y Label" ,xlabel = "X Label" ,color = "red")

#beraber veya tek tek yazdırabileceğiniz uygulamalı bir fonksiyon
draw_boxplot_together <- function(df){
  while (TRUE) {
  draw = readline(prompt = "Otomatik çizim ister misiniz? Yoksa çizim unsurlarını kendiniz mi dolduracaksınız? (Otomatik/Manuel): ")
  if(draw == 'Otomatik') {
    ct <- as.numeric(readline(prompt = "Kaç değer çizdireceksiniz? (1/2/3 ...): "))
    if( ct%%2 == 1 ){
      if(ct==1){
        par(mfrow = c((ct+1)/2,(ct+1)/2))
      }else{
        x = (ct+1)/((ct+1)/2)
        par(mfrow = c(x, (ct+1)/2))
      }
    }else{
      x=ct/(ct/2)
      par(mfrow = c(x,ct/2))
    }
    x <- 1 
    while (x<=ct) {
      print("Çizdirmek istediğiniz değerleri teker teker giriniz işlemden çıkmak için exit yazınız: ")
      deg = readline(prompt = "(Var1/Var2/Var3/Var4/Var5/Var6/Var7/Var8/exit): ")   
      if(deg == "exit"){
        break
      }
        
        ds = df[[deg]]
        full_name <- deg
        Maintitle = full_name
        xlimit = NULL
        ylimit = NULL
        xlabel= NULL
        ylabel = NULL
        color = NULL
        theboxplot(df = ds , Maintitle = Maintitle , xlimit = xlimit ,ylimit = ylimit , xlabel= xlabel ,ylabel = ylabel ,color = color)
        x = x+1
      }

  }else if(draw == 'Manuel'){

    ct <- as.numeric(readline(prompt = "Kaç değer çizdireceksiniz? (1/2/3 ...): "))
    if( ct%%2 == 1 ){
      if(ct==1){
        par(mfrow = c((ct+1)/2,(ct+1)/2))
      }else{
        x = (ct+1)/((ct+1)/2)
        par(mfrow = c(x, (ct+1)/2))
      }
    }else{
      x=ct/(ct/2)
      par(mfrow = c(x,ct/2))
    }
    x <- 1 
    while (x<=ct) {
      print("Çizdirmek istediğiniz değerleri teker teker giriniz işlemden çıkmak için exit yazınız: ")
      deg = readline(prompt = "(Var1/Var2/Var3/Var4/Var5/Var6/Var7/Var8/exit): ")   
      if(deg == "exit"){
        break
      }
      
      ds = df[[deg]]
      Maintitle = readline(prompt = "Başlığı giriniz: ")
      xlimit_string = readline(prompt = "x'in limitlerini seçiniz(örnegin: 0,2 (alt limit 0, üst limit 2) veya 1,5 ): ")
      xlimit <- as.numeric(strsplit(xlimit_string, ",")[[1]])
      ylimit_string = readline(prompt = "y'nin limitlerini seçiniz(örnegin: 3,5 (alt limit 3, üst limit 5) veya 50,80 ): ")
      ylimit <- as.numeric(strsplit(ylimit_string, ",")[[1]])
      xlabel= readline(prompt = "x etiketini giriniz: ")
      ylabel = readline(prompt = "y etiketini giriniz: ")
      color = readline(prompt = "rengi seçiniz(ingilizce): ")
      theboxplot(df = ds , Maintitle = Maintitle , xlimit = xlimit ,ylimit = ylimit , xlabel= xlabel ,ylabel = ylabel ,color = color)
      x = x+1
    }

  }else{
    print("!!!!!!!!!!!!!!! Hatalı yazım yaptınız !!!!!!!!!!!!!!!")
  }
  run = readline(prompt = "Programı sonlandırmak için exit yazınız devam etmek için enter'a tıklayınız: ")
  if(run=="exit"){
    break
  }
  }
}
draw_boxplot_together(df=v1)








