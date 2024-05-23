v1=read.table("C:/Users/Berkay/OneDrive/Masaüstü/DatasetNA.txt", header=T, dec = ",")


#seq fonksiyonu optimize edilmiş
real_seq <- function(x, y, z) {
  
  n <- floor((y - x) / z) + 1
  v <- vector(length = n)
  
  for (i in 1:n) {
    v[i] <- x + (i - 1) * z
  }
  
  return(v)
}
#temel seq fonksiyonu
own_seq <- function(x ,y, z){
  a <- c()
  i <- x
  a <- c(a, i)
  
  while (i <= y) {
    i <- i + z
    a <- c(a, i)
    
  }
  return(a)
}
#cut fonksiyonu
own_cut <- function(x, breaks) {
  groups <- rep(NA, length(x))
  i <- 1
  while (i <= length(breaks) - 1) {
    if (i == 1) {
      groups[x < breaks[i + 1]] <- i
    } else {
      groups[x > breaks[i] & x <= breaks[i + 1]] <- i
    }
    i <- i + 1
  }
  return(groups)
}
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
#histogram fonksiyonu
TheHist = function(df, binsnumber, Maintitle, xlimit, ylimit, color, xlabel, ylabel){
  
  if(is.null(binsnumber)) {
    
    n_bins <- 8
  }else{
    n_bins <- binsnumber
  }
  vars <- na.omit(df)
  
  breaks <- real_seq(min(vars), max(vars), (max(vars) - min(vars))/n_bins)
  
  groups <- own_cut(vars, breaks = breaks)
  
  freq <- table(groups)
  freq
  plot.new()
  if(is.null(Maintitle)) {
    Maintitle <- deparse(substitute(df))
    title(main = Maintitle)
  }else{
    title(main = Maintitle)
  }
    if (is.null(xlimit) && is.null(ylimit)){
    plot.window (xlim = c(min(vars), max(vars) ), ylim = c(0, max(freq)+1))
  } else if (is.null(xlimit)) {
    plot.window (xlim = c(min(vars), max(vars) ), ylim = ylimit) 
  } else if(is.null(ylimit)) {
    plot.window (xlim = xlimit, ylim = c(0, max(freq)+1))
  }else{
    plot.window (xlim = xlimit, ylim = ylimit)
  }
  
  
  if(is.null(color)) {
    color <- "grey"
  }
  
  
  for (i in 1:length(freq)) {
    rect(breaks[i] , 0, breaks[i+1] , freq[i], col = color, border = "black")
    text((breaks[i] + breaks[i+1]) / 2, freq[i], labels = freq[i], pos = 3)
  }
  if(is.null(xlabel)) {
    xlabel <- deparse(substitute(df))
    mtext(xlabel, side = 1, line = 2.7)
  }else{
    mtext(xlabel, side = 1, line = 2.7)
  }
  if(is.null(ylabel)) {
    mtext("Frequency", side = 2, line = 2.7)
  }else{
    mtext(ylabel, side = 2, line = 2.7)
  }
  axis(1, at=breaks)
  axis(2)
  
}
#Histogramın kullanılabilme çeşitleri
TheHist(df = v1$Var1, binsnumber = NULL, Maintitle = NULL, xlimit =NULL, ylimit = NULL,  xlabel = NULL, ylabel = NULL , color = NULL)

TheHist(df = v1$Var1, binsnumber = 8, Maintitle = "Var1 Table", xlimit =c(3.2, 5), ylimit = c(0, 40), color = "orange", xlabel = "Values", ylabel = "Individuals")

#histogramı tek tek ve birlikte yazmak için uygulamalı bir fonksiyon
draw_hist_together <- function(df){
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
        binsnumber = NULL
        xlimit = NULL
        ylimit = NULL
        xlabel= full_name
        ylabel = NULL
        color = NULL
        
        TheHist(df = ds ,binsnumber = binsnumber,  Maintitle = Maintitle , xlimit = xlimit ,ylimit = ylimit , xlabel= xlabel ,ylabel = ylabel ,color = color)
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
        binsnumber = as.numeric(readline(prompt = "Kaç kutu istersiniz: "))
        xlimit_string = readline(prompt = "x'in limitlerini seçiniz(örnegin: 3,6 (alt limit 3, üst limit 6) veya 2,5 ): ")
        xlimit <- as.numeric(strsplit(xlimit_string, ",")[[1]])
        ylimit_string = readline(prompt = "y'nin limitlerini seçiniz(örnegin: 0,30 (alt limit 0, üst limit 30) veya 7,80 ): ")
        ylimit <- as.numeric(strsplit(ylimit_string, ",")[[1]])
        xlabel= readline(prompt = "x etiketini giriniz: ")
        ylabel = readline(prompt = "y etiketini giriniz: ")
        color = readline(prompt = "rengi seçiniz(ingilizce): ")
        TheHist(df = ds ,binsnumber = binsnumber , Maintitle = Maintitle , xlimit = xlimit ,ylimit = ylimit , xlabel= xlabel ,ylabel = ylabel ,color = color)
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
draw_hist_together(df=v1)

