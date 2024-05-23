v1=read.table("C:/Users/Berkay/OneDrive/Masaüstü/DatasetNA.txt", header=T)
#10'arlı sayma fonksiyonu
ten_raund <- function(x){
  if (x %% 10 >= 5) {
    
    return(ceiling(x/10)*10)
    
  } else if (x %% 10 == 0) {
    
    return(x)
    
  } else {
    
    return(floor(x/10)*10)
  }
  
}
#seq fonksiyonu
real_seq <- function(x, y, z) {
  
  n <- floor((y - x) / z) + 1
  v <- vector(length = n)
  
  for (i in 1:n) {
    v[i] <- x + (i - 1) * z
  }
  
  return(v)
}
#5'erli yuvarlama fonksiyonu
five_raund <- function(x) {
  if (x %% 5 >= 2.5) {
    
    return(ceiling(x/5)*5)
    
  } else if (x %% 5 == 0) {
    
    return(x)
    
  } else {
    
    return(floor(x/5)*5)
  }
}

own_seq <- function(x ,y, z){
  a <- c()
  i <- x
  
  while (i <= y) {
    a <- c(a, i)
    i <- i + z
  }
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

TheBarplot = function(df , Maintitle , xlimit, ylimit , color , xlabel  , ylabel , xcolumn_names, number_of_interval ){
  groups<- factor(df)
  
  gropus <- c(levels(groups))
  
  members <- c(table(df))
  
  max(members)
  plot.new()
  if(is.null(Maintitle)) {
    Maintitle <- deparse(substitute(df))
    title(main = Maintitle)
  }else{
    title(main = Maintitle)
  }
  
  if(is.null(color)) {
    color <- "grey"
  }
   
  
  
    if (is.null(xlimit) && is.null(ylimit)){
      ylimit<-c(0, maxhesap(members))
    plot.window (xlim = c(0, length(gropus)+1 ), ylim = c(0, maxhesap(members)+1))
  } else if (is.null(xlimit)) {
    plot.window (xlim = c(0, length(gropus)+1 ), ylim = ylimit) 
  } else if(is.null(ylimit)) {
    ylimit<-c(0,maxhesap(members))
    plot.window (xlim = xlimit, ylim = c(0, maxhesap(members)+1))
  }else{
    plot.window (xlim = xlimit, ylim = ylimit)
  }
  
  for (i in 1:length(gropus)) {
    rect(i - 0.3, 0, i + 0.3, members[i], col = color, border = "black")
    text((i - 0.3 + i + 0.3) / 2, members[i], labels = members[i], pos = 3)
  }
  if(is.null(xlabel)) {
    xlabel <- deparse(substitute(df))
    mtext(xlabel, side = 1, line = 2.7)
  }else{
    mtext(xlabel, side = 1, line = 2.7)
  }
  if(is.null(ylabel)) {
    mtext("Individuals", side = 2, line = 2.7)
  }else{
    mtext(ylabel, side = 2, line = 2.7)
  }
  
  
  minus<- (max(ylimit)-min(ylimit))
  if (is.null(number_of_interval)) {
    axis(2, at = real_seq(0, ten_raund(minus), ten_raund(minus)/6))
  } else {
    axis(2, at = real_seq(0, ten_raund(minus), ten_raund(minus)/number_of_interval))
  }
  
  if (is.null(xcolumn_names)) {
    axis(1, at = 1:length(gropus), labels = gropus)
  } else {
    xcolumn_names <- strsplit(xcolumn_names, ",")[[1]]
    axis(1, at = 1:length(xcolumn_names), labels = xcolumn_names)
  }
  
}

TheBarplot(df = v1$Group , Maintitle = NULL , xlimit = NULL ,ylimit = NULL , xlabel = NULL, ylabel = NULL, color = NULL, xcolumn_names=NULL,number_of_interval = NULL)
TheBarplot(df = v1$Gender , Maintitle = "Gender table" , xlimit = c(0,3) ,ylimit = c(0,60) , xlabel = "Genders", ylabel = "INDIVIDUALS", color = "orange", xcolumn_names = "Female, Male", number_of_interval=6)

draw_barplot_together <- function(df){
  while (TRUE) {
    draw = readline(prompt = "Otomatik çizim ister misiniz? Yoksa çizim unsurlarını kendiniz mi dolduracaksınız? (Otomatik/Manuel): ")
    if(draw == 'Otomatik') {
      ct <- as.numeric(readline(prompt = "Kaç değer çizdireceksiniz? 1 veya 2 : "))
      if( ct%%2 == 1 ){
        if(ct==1){
          
          par(mfrow = c((ct+1)/2,(ct+1)/2))
        }else{
          x = (ct+1)/((ct+1)/2)
          par(mfrow = c(x, (ct+1)/2))
        }
      }else{
        yon = readline(prompt = "Dikey mi çizdireceksiniz Yatay mı? (Dikey/Yatay): ")
        if(yon=="Dikey"){
          x=ct/(ct/2)
          par(mfrow = c(x,ct/2))
        }else{
          x=ct/(ct/2)
          par(mfrow = c(ct/2,x))
        }

      }
      x <- 1 
      while (x<=ct) {
        print("Çizdirmek istediğiniz değerleri teker teker giriniz işlemden çıkmak için exit yazınız: ")
        deg = readline(prompt = "(Group/Gender/exit): ")   
        if(deg == "exit"){
          break
        }
        
        ds = df[[deg]]
        full_name <- deg
        Maintitle = full_name
        xlimit = NULL
        ylimit = NULL
        xlabel= full_name
        ylabel = NULL
        color = NULL
        xcolumn_names=NULL
        number_of_interval = NULL
        
        TheBarplot(df = ds ,  Maintitle = Maintitle , xlimit = xlimit ,ylimit = ylimit , xlabel= xlabel ,ylabel = ylabel ,color = color, xcolumn_names = xcolumn_names, number_of_interval = number_of_interval)
        x = x+1
      }
      
    }else if(draw == 'Manuel'){
      
      ct <- as.numeric(readline(prompt = "Kaç değer çizdireceksiniz? 1 veya 2 : "))
      if( ct%%2 == 1 ){
        if(ct==1){
          
          par(mfrow = c((ct+1)/2,(ct+1)/2))
        }else{
          x = (ct+1)/((ct+1)/2)
          par(mfrow = c(x, (ct+1)/2))
        }
      }else{
        yon = readline(prompt = "Dikey mi çizdireceksiniz Yatay mı? (Dikey/Yatay): ")
        if(yon=="Dikey"){
          x=ct/(ct/2)
          par(mfrow = c(x,ct/2))
        }else{
          x=ct/(ct/2)
          par(mfrow = c(ct/2,x))
        }
        
      }
      x <- 1 
      while (x<=ct) {
        print("Çizdirmek istediğiniz değerleri teker teker giriniz işlemden çıkmak için exit yazınız: ")
        deg = readline(prompt = "(Group/Gender/exit): ")   
        if(deg == "exit"){
          break
        }
        
        ds = df[[deg]]
        Maintitle = readline(prompt = "Başlığı giriniz: ")
        xlimit_string = readline(prompt = "x'in limitlerini seçiniz(örnegin: 0,5 (alt limit 0, üst limit 5) veya 0,3 ): ")
        xlimit <- as.numeric(strsplit(xlimit_string, ",")[[1]])
        ylimit_string = readline(prompt = "y'nin limitlerini seçiniz(örnegin: 0,30 (alt limit 0, üst limit 60) veya 0,60 ): ")
        ylimit <- as.numeric(strsplit(ylimit_string, ",")[[1]])
        xlabel= readline(prompt = "x etiketini giriniz: ")
        ylabel = readline(prompt = "y etiketini giriniz: ")
        color = readline(prompt = "rengi seçiniz'ingilizce': ")
        xcolumn_names = readline(prompt = "Barların isimlerini yazınız 'yanyana virgül ile yazabilirsiniz Female, Male veya Group1, Group2, Group3, Group4 gibi' : ")
        number_of_interval = as.numeric(readline(prompt = "y ekseni kaç aralıktan oluşsun?: "))
        TheBarplot(df = ds ,  Maintitle = Maintitle , xlimit = xlimit ,ylimit = ylimit , xlabel= xlabel ,ylabel = ylabel ,color = color, xcolumn_names = xcolumn_names, number_of_interval = number_of_interval)
        
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
draw_barplot_together(df=v1)



