# Gerekli Kutuphaneler
library(clusterSim)
library(class)
library(e1071)
library(caret)

# 1. VERI YUKLEME VE ON ISLEME
df <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Sutun isimlerini duzenleme
colnames(df) <- c(
  "Kreatinin",
  "BUN",
  "GFR",
  "Idrar_Cikisi",
  "Diyabet",
  "Hipertansiyon",
  "Yas",
  "Idrarda_Protein",
  "Gunluk_Su_Tuketimi",
  "Ilac_Turu",
  "KBH"
)

# Eksik/Hatali veri duzeltme
df$Ilac_Turu[df$Ilac_Turu == "None"] <- "X_Ilaci"

# hedef de??i??ken ve kategorik de??isken fakt??r don??????m??
df$KBH <- factor(df$KBH, levels = c(0, 1))
df$Diyabet <- factor(df$Diyabet, levels = c(0, 1))
df$Hipertansiyon <- factor(df$Hipertansiyon, levels = c(0, 1))
df$Ilac_Turu <- factor(df$Ilac_Turu)

# Sayisal degiskenler numaric d??n??????m??
df$Kreatinin <- as.numeric(df$Kreatinin)
df$BUN <- as.numeric(df$BUN)
df$GFR <- as.numeric(df$GFR)
df$Idrar_Cikisi <- as.numeric(df$Idrar_Cikisi)
df$Yas <- as.numeric(df$Yas)
df$Idrarda_Protein <- as.numeric(df$Idrarda_Protein)
df$Gunluk_Su_Tuketimi <- as.numeric(df$Gunluk_Su_Tuketimi)

# Gereksiz sutun varsa temizleme (Ornegin 'Ilac' varsa)
df <- df[, !colnames(df) %in% c("Ilac")]

head(df)
str(df)

# 2. NORMALIZASYON
sayisal_sutunlar <- c(
  "Kreatinin",
  "BUN",
  "GFR",
  "Idrar_Cikisi",
  "Yas",
  "Idrarda_Protein",
  "Gunluk_Su_Tuketimi"
)

sayisal_df <- df[, sayisal_sutunlar]

# Veriyi n4 tipinde normalize etme
sayisal_norm <- data.Normalization(sayisal_df, type = "n4")

df_norm <- df
df_norm[, sayisal_sutunlar] <- sayisal_norm

str(df_norm)

# 3.KNN MODELI
# KNN icin sadece sayisal verileri ve hedefi seciyoruz
X <- df_norm[, sayisal_sutunlar]
y <- df_norm$KBH

knn_calistir <- function(X, y, train_orani, k = 10) {
  set.seed(42)
  indeks <- sample(1:nrow(X), size = train_orani * nrow(X))
  
  X_train <- X[indeks, ]
  X_test  <- X[-indeks, ]
  y_train <- y[indeks]
  y_test  <- y[-indeks]
  
  knn_tahmin <- knn(
    train = X_train,
    test  = X_test,
    cl    = y_train,
    k     = k
  )
  
  accuracy <- mean(knn_tahmin == y_test)
  return(accuracy)
}

# Farkli oranlarda KNN calistirma
acc_knn_60 <- knn_calistir(X, y, 0.60, k = 10)
acc_knn_70 <- knn_calistir(X, y, 0.70, k = 10)
acc_knn_80 <- knn_calistir(X, y, 0.80, k = 10)
acc_knn_90 <- knn_calistir(X, y, 0.90, k = 10)

acc_knn_60
acc_knn_70
acc_knn_80
acc_knn_90


knn_sonuclar <- data.frame(
  Egitim_Orani = c("60%", "70%", "80%", "90%"),
  K_Degeri     = c(10, 10, 10, 10),
  Basari_Orani = c(acc_knn_60, acc_knn_70, acc_knn_80, acc_knn_90)
)

print(knn_sonuclar)


# 4. NAIVE BAYES MODELI
nb_calistir <- function(df, train_orani) {
  set.seed(42)
  indeks <- sample(1:nrow(df), size = train_orani * nrow(df))
  
  train_df <- df[indeks, ]
  test_df  <- df[-indeks, ]
  
  nb_model <- naiveBayes(
    KBH ~ .,
    data = train_df
  )
  
  tahmin <- predict(nb_model, test_df)
  accuracy <- mean(tahmin == test_df$KBH)
  return(accuracy)
}

# Farkli oranlarda Naive Bayes calistirma
acc_nb_60 <- nb_calistir(df_norm, 0.60)
acc_nb_70 <- nb_calistir(df_norm, 0.70)
acc_nb_80 <- nb_calistir(df_norm, 0.80)
acc_nb_90 <- nb_calistir(df_norm, 0.90)

nb_sonuclar <- data.frame(
  Egitim_Orani = c("60%", "70%", "80%", "90%"),
  Basari_Orani = c(acc_nb_60, acc_nb_70, acc_nb_80, acc_nb_90)
)

# Tabloyu ekrana yazd??r
print(nb_sonuclar)

acc_nb_60
acc_nb_70
acc_nb_80
acc_nb_90


# 5. KARSILASTIRMALI SONUC TABLOSU
karsilastirma_tablosu <- data.frame(
  Egitim_Orani = c("60%", "70%", "80%", "90%"),
  Test_Orani   = c("40%", "30%", "20%", "10%"),
  KNN_Basari   = c(acc_knn_60, acc_knn_70, acc_knn_80, acc_knn_90),
  NB_Basari    = c(acc_nb_60, acc_nb_70, acc_nb_80, acc_nb_90)
)



print("KNN ve Naive Bayes Karsilastirma Tablosu")
print(karsilastirma_tablosu)

