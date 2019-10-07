data_emo_intelligence <- readRDS(file.choose())

category <- c(rep('EA', 600), rep('EC', 600))

emo_Attention <- c(data_emo_intelligence$i1,data_emo_intelligence$i3, data_emo_intelligence$i5,
                   data_emo_intelligence$i7, data_emo_intelligence$i9,data_emo_intelligence$i11)
emo_Clarity <- c(data_emo_intelligence$i2,data_emo_intelligence$i4, data_emo_intelligence$i6,
                 data_emo_intelligence$i8, data_emo_intelligence$i10,data_emo_intelligence$i12)

scales <- c(emo_Attention, emo_Clarity)

df <- data.frame(category, scales)

plot(scales ~ category, data = df)

emotion.aov <- aov(scales ~ category, data = df)
summary(emotion.aov)
  
