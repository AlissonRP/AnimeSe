library(tidyverse)
library(fastrep)
library(betareg)
library(qqplotr)




df = read_csv("data/animes.csv")
df = df |> 
 drop_na() |> distinct()  |> 
   filter(title != "Doraemon (1979)") |> 
  select(-synopsis, -uid, -genre, -aired,  -img_url, -link) |> 
  mutate(score = score / 10) |> 
  mutate(eps = case_when(episodes < 12 ~ "very-low",
                            (12 < episodes & episodes < 352) ~ "ok",
                              TRUE ~ "OP"),
                   membros = case_when(members < 100000 ~ "low",
                                   TRUE ~ "insane"),
                   top50_pop = ifelse(popularity < 51, 1 ,0)) |> 
  filter(members > 15000)

 

fit = betareg(score ~ ., 
        data = df |> select(-title, -ranked), x=TRUE)

fit |> summary()

dlmtest::lrtest(fit, . ~ . + I(predict(fit, type = "link")^2))



residuot2 <- residuals(fit, type= "sweighted2")

yajust<-fitted.values(fit)
yhat=hatvalues(fit)
dcook<- cooks.distance(fit)

deviance<- sum(residuals(fit, tipe= "deviance")^2)



#----- INDEX vs residuals-----

df = df |> bind_cols(index = 1:nrow(df))
df |> 
ggplot(aes(x=index,
                                              y=residuot2))+
  geom_point(size=1.5) + 
  geom_hline(yintercept=3, colour="red2", 
             size=0.5,  linetype="dashed") +
  geom_hline(yintercept=0, colour="black", 
             size=0.7, linetype="dashed") +  
  geom_hline(yintercept=-3, colour="red2", 
             size=0.5,  linetype="dashed")+
  labs(x = "Índices das observações", y = "Resíduos")


# --- HAT VALUES
g1 = ggplot(df, aes(x=index, y=yhat))+
  geom_point(size=1.5) + 
  labs(x = "Índice das observações", y = "htt")

plotly::ggplotly(g1)


## ---- #--- Y ajustado vs resíduos Padronizados 2
ggplot(df, aes(x=yajust, y=residuot2))+
  geom_point(size=1.5) + 
  geom_hline(yintercept=3, colour="red2", size=0.5, 
             linetype="dashed") +
  geom_hline(yintercept=0, colour="black", size=0.7,
             linetype="dashed") +  
  geom_hline(yintercept=-3, colour="red2", size=0.5, 
             linetype="dashed")+
  labs(x = "y ajustado", y = "Resíduos")




g1 = ggplot(df, aes(x=index, 
                                y= dcook))+
  geom_point(size=1.5)+
  labs(x = "Índices", y = "Distância de Cook")

plotly::ggplotly(g1)


### Y VS Y AJUSTADO
ggplot(df, aes(x = score, y=yajust))+
  geom_point(size=1.5) + 
  geom_smooth(method = "lm", linetype ="dashed",
              size=0.5, col = "blue")+
  labs(x = "y observado", y = "y ajustado")






### ENVELOPE
residuot2_df<- data.frame(residuot2)

ggplot(data = residuot2_df, 
       mapping = aes(sample = residuot2))+
  geom_qq_band( alpha = 0.5, fill="white", col="black") +
  
  stat_qq_line(size=0.5, linetype="dashed") + 
  stat_qq_point(size=2) +
  scale_fill_discrete("Bandtype")  +
  labs(x = "Quantis teóricos", y = "Quantis amostrais")
