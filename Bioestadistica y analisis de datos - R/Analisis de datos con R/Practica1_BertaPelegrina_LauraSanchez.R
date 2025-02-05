
library(tidyverse) # Cargamos la librería tidyverse
library(dplyr) # Cargamos la librería dplyr

data_fr <- read_csv("06007.csv") # Cargamos los datos del 
# archivo csv, delimitado por comas 

# APARTADO 1
data_fr
colnames(data_fr)
ncol(data_fr)
nrow(data_fr)

data_fr %>%
  summarise_all(~sum(is.na(data_fr)))

data_fr %>%
  summarise(sum(is.na(data_fr)))


# APARTADO 2
(df <- transmute(data_fr, # se crea un nuevo dataframe, df, 
                 sexo = Sexo,          # copia del original, data_df, al
                 nivel = Nivel,        # que se le renombran las columnas
                 alimento = Alimento,  # usando transmute
                 diario = `A diario`,
                 tres = `3 o más veces a la semana, pero no a diario`,
                 uno_dos = `1 o 2 veces a la semana`,
                 uno = `Menos de 1 vez a la semana`,
                 nunca = Nunca,
                 no_consta = `No consta`
))



# APARTADO 3
df <- df %>% # en el datafra,e creado, df, se genera una nueva columna, Total
  mutate(Total = rowSums(select(df, 4:8), na.rm = TRUE)) # se utiliza mutate

df

df <- df %>%
  mutate(Total = diario+tres+uno_dos+uno+nunca)
df

# APARTADO 4
df <- mutate(df, Media_semanal = (diario * 7 +                   # en df, creamos una nueva columna llamada 
                                    tres * 4.5 + uno_dos * 1.5   # Media_semanal, que es la suma de la 
                                  + uno * 0.5 + nunca * 0+       # ponderación de los individuos según cuántas
                                    + no_consta * 0)/Total)      # veces comen cierto alimento a la semana
df                                                               # (a diario tiene más ponderación que uno; 
# y nunca, cuenta por 0 por ejemplo)

(df %>% summarise(min_media = min(Media_semanal))) # comprobamos que el valor mínimo no sea menor que 0
(df %>% summarise(max_media = max(Media_semanal))) # comprobamos que el valor máximo no sea mayor que 7


# APARTADO 5
filter(df, sexo == "MUJERES" & nivel == "I") %>% # se filtran en el dataset df, aquellas mujeres, de nivel I,
  arrange(desc(Media_semanal)) %>%               # ordenadas de mayor a menor (según la media semanal),
  select(alimento, Media_semanal) %>%            # y se seleccionan las columnas alimento y media semanal
  print(n = 20)                                  # para mostrarlas por pantalla


# APARTADO 6
comida <- df %>%                         # creamos una nueva variable llamda comida, a partir de df
  group_by(alimento) %>%                 # queremos agrupar por los tipos de alimentos, y a partir 
  summarise(                             # de ahí, calcular la media de las medias de cada tipo 
    media_alimento = mean(Media_semanal) # de alimento
  ) %>%
  arrange(desc(media_alimento))                # ordenamos de mayor a menor

comida

comida %>%
  ggplot(aes(x = reorder(alimento, -media_alimento), y = media_alimento, fill = alimento)) + # orden descendente y cada alimento un color
  geom_bar(stat = "identity") +                              # los valores en el eje y representan directamente las alturas de las barras
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +                   # para no solapar los nombres de los alimentos en el eje x
  labs(title = "Consumo medio semanal general de cada uno de los alimentos")   # título

# APARTADO 7
c_basura<-df %>%  # guardamos en una nueva variable, c_basura, y trabajamos con df
  group_by(nivel) %>% # agrupamos por los distintos niveles socioeconómicos
  filter(alimento=="Comida rápida") %>% # y aplicamos dos filtros, unos para buscar sólo aquellos alimentos que corresponden a comida rápida
  filter(nivel != "No consta") %>%      # y otro para no tener en cuenta el nivel "No consta"
  summarise(media_c_basura = mean(Media_semanal)) %>% # resumimos y aplicamos la operación mean (que realizará la media de la frecuencia 
  arrange(desc(media_c_basura))                             # en la que hombres y mujeres de cada nivel, que ingieren Comida rápida
c_basura

# APARTADO 8
comida_diferencia <- df %>%  # guardamos en una nueva variable, llamada comida_diferencia
  group_by(sexo, alimento, Media_semanal) %>%  # agrupamos por sexo, alimento y Media_semanal
  pivot_wider(names_from = sexo, values_from = Media_semanal # pivotamos "a lo ancho", generando dos nuevas columnas para 
  ) %>%               # la variable sexo (HOMBRES y MUJERES), que se rellenan con los valores de Media_semanal correspondientes
  summarise(
    Diferencia = mean(HOMBRES, na.rm = TRUE) - mean(MUJERES, na.rm = TRUE) # se resume la operación en crear una nueva variable, 
  ) # Diferencia, que es la resta entre la media de los valores de la columna HOMBRES y la media de MUJERES (agrupado por alimento)

ggplot(comida_diferencia, aes(x = Diferencia, y =reorder(alimento,Diferencia))) + # ordneamos por diferencia
  geom_bar(stat = "identity", position = "dodge", fill="pink") + # 
  labs(title = "Diferencia en el Consumo de Alimentos entre Hombres y Mujeres por Alimento", # "dodge" coloca las barras lado a lado
       x = "Diferencia Media", y = "Alimento") +
  theme_minimal() # eliminamos elementos innecesarios como líneas de fondo y cuadrículas

comida_sexo <- df %>%
  group_by(alimento, sexo) %>%
  summarise(
    frecuencia = mean(Media_semanal)
  ) %>%
  arrange(frecuencia)

ggplot(comida_sexo, aes(x = reorder(alimento, frecuencia), y = frecuencia, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# APARTADO 9
nuevo_df <- data.frame( # creamos una nueva tabla, nuevi_df con las columnas alimento y Coste_diario
  alimento = c("Fruta fresca (excluye zumos)", "Carne", "Huevos", "Pescado", "Pasta, arroz, patatas", 
               "Pan, cereales", "Verduras, ensaladas y hortalizas", "Legumbres", "Embutidos y fiambres", 
               "Productos lácteos", "Dulces", "Refrescos con azúcar", "Comida rápida", 
               "Aperitivos o comidas saladas de picar", "Zumo natural de frutas o verduras"),
  Coste_diario = c(1.12, 2.67, 0.37, 3.89, 0.17, 0.24, 1.28, 0.43, 0.36, 1.02, 1.26, 1.62, 9.45, 1.29, 0.61)
)


# Calculamos la media de Media_semanal para cada alimento y nivel socioeconómico, tomando en cuenta el sexo
presupuesto_por_nivel <- df %>%
  group_by(nivel, alimento) %>%
  summarise(media_semanal_promedio = mean(Media_semanal, na.rm = TRUE)) %>%
  inner_join(nuevo_df, by = "alimento") %>%
  mutate(semanal_alimento = media_semanal_promedio * Coste_diario) %>%
  group_by(nivel) %>%
  summarise(presupuesto_semanal = sum(semanal_alimento))%>%
  arrange(desc(presupuesto_semanal))

# Imprimimos el presupuesto semanal por nivel socioeconómico
print(presupuesto_por_nivel)

# Carga de los datos
data <- data.frame(
  nivel = c("V", "IV", "III", "I", "VI", "II", "No consta"),
  presupuesto_semanal = c(63.3, 63.2, 63.2, 63.1, 63.1, 62.7, 62.5)
)



ggplot(presupuesto_por_nivel, aes(x = presupuesto_semanal, y = reorder(nivel,presupuesto_semanal), fill = nivel)) +
  geom_bar(stat = "identity") +
  labs(title = "Presupuesto Semanal por Nivel Socioeconómico",
       x = "Gasto Semanal",
       y = "Nivel Socioeconómico") +
  theme_minimal() +
  theme(legend.position = "none")


# APARTADO 10
ggplot(df, aes(x = Media_semanal, y = alimento, color = alimento)) +
  geom_boxplot() + # para graficar de manera automática los boxplots en función de los ejes x e y
  labs(title = "Consumo semanal de alimentos",
       x = "Alimento",
       y = "Media Semanal de Consumo")


# Carga de los datos
data <- data.frame(
  nivel = c("V", "IV", "III", "I", "VI", "II", "No consta"),
  presupuesto_semanal = c(63.3, 63.2, 63.2, 63.1, 63.1, 62.7, 62.5)
)

# Verificación rápida de los datos
head(data)

# Realizar el ANOVA
anova_result <- aov(presupuesto_semanal ~ nivel, data = data)

# Mostrar los resultados
summary(anova_result)

# Realizar el ANOVA
anova_result <- aov(presupuesto_semanal ~ nivel, data = data)

# Obtener el valor p
p_value <- summary(anova_result)[[1]]$`Pr(>F)`
p_value



# APARTADO 11
umbral_bajo_glucidos <- 7
umbral_alto_proteinas <- 14
keto <- df %>%
  group_by(sexo, nivel) %>%
  summarise(
    consumo_bajo_glucidos = sum(Media_semanal[alimento %in% c("Pasta, arroz, patatas", "Pan, cereales", "Dulces", "Refrescos con azúcar")]),
    consumo_alto_proteinas = sum(Media_semanal[alimento %in% c("Carne", "Pescado", "Huevos", "Embutidos y fiambres")]),
    .groups = 'drop'  # Establecer el argumento .groups para desagrupar
  ) %>%
  print(n=100) %>%
  filter(consumo_bajo_glucidos <= umbral_bajo_glucidos & consumo_alto_proteinas >= umbral_alto_proteinas) %>%
  arrange(desc(consumo_bajo_glucidos),consumo_alto_proteinas)
keto

# APARTADO 11
ocio<-df %>%
  group_by(nivel,sexo) %>%
  filter(alimento=="Aperitivos o comidas saladas de picar") %>%
  filter(nivel != "No consta") %>%
  summarise(mediasuma = mean(Media_semanal)) %>%
  arrange(desc(mediasuma))
ocio

# APARTADO 11
dulces_data <- df %>% # Queremos que aparezcan sólo las filas relacionadas con el consumo de Dulces
  filter(alimento == "Dulces")

dulces_data <- dulces_data %>%
  group_by(nivel, sexo) %>%  # Agrupamos por nivel y sexo para promediar las diferentes
  summarise(Media_semanal_promedio = mean(Media_semanal)) # Media_semanal de cada grupo y sexo

dulces_outliers <- dulces_data %>% # Visto en el boxplot del ejercicio anterior que un outlier se encuentra
  filter(Media_semanal_promedio > 4.0)  # claramente a partir de una Media_semanal de 4, buscamos ese valor

dulces_outliers

# MEJORA DEL APARTADO 10
df_sin_no_consta <- df%>% # Creamos nuevo dataframe, sin las instancias que pertenecen a "No_consta"
  filter(nivel != "No consta")

ggplot(df_sin_no_consta, aes(x = Media_semanal, y = alimento, color = alimento)) +
  geom_boxplot(data = df_sin_no_consta) + 
  labs(title = "Consumo semanal de alimentos",
       x = "Media Semanal de Consumo",
       y = "Alimento")
