## Análisis de mis Likes :sparkling_heart: en Letterboxd

### Procesamos los URLs

#### Abrimos las librerías

```{r}
library(tidyverse)
library(lubridate)
library(readxl)
library(extrafont)
library(writexl)
library(rvest)
library(longurl)
library(dplyr)
library(purrr)
library(janitor)
```

#### Abrimos la Base de Datos

```{r}
df_letterboxd_like <- read_xlsx("Input/Peliculas con Like - Letterboxd.xlsx", 
                                na = c("N/A", " ")) |>
  janitor::clean_names()
```

#### Pasamos los URLs a tipo character y verificamos el cambio

```{r}
as.character(df_letterboxd_like$letterboxd_uri)
sapply(df_letterboxd_like, class)
```

-   El código "`as.character`" es para hacer que la columna "letterboxd_uri" esté en formato `character` (por si no estuviera en ese formato)

-   El código "`sapply`" es para verificar las clases de todas las columnas en el df.

#### Llegamos a los URLs originales (extendidos)

```{r}
df_letterboxd_like_con_urls <- df_letterboxd_like |>
  mutate(expanded_url = map_chr(letterboxd_uri, ~ longurl::expand_urls(.x)$expanded_url))
```

-   Usamos el código "`mutate`" para crear una nueva columna llamada "`expanded_url`" que contiene los URLs extendidos.

-   Usamos "`map_chr`" para aplicar una función. En este caso, usamos "`longurl::expanded_urls`" para expandir a los URLs originales de cada película en la columna "`letterboxd_uri`" (que es la columna que tiene los URLs cortos).

#### Creamos URLs donde se verán los géneros, temas y nanogéneros

```{r}
df_letterboxd_like_con_urls <- df_letterboxd_like_con_urls |>
  mutate(
    genres_url = paste0(expanded_url, "genres/"),
    themes_url = paste0(expanded_url, "themes/"),
    nanogenres_url = paste0(expanded_url, "nanogenres/")
  )
```

-   Usamos "`mutate`" para crear tres nuevas columnas: "`genres_url`", "`themes_url`" y "`nanogenres_url`".

-   Dentro de ese mutate usamos la función "`paste0`" para agregarle al URL expandido (que está en la columna "expanded_url") las palabras "genres/", "themes/" y "nanogenres/" respectivamente. Así podemos accedemos a la información de esas páginas web.

#### Extraemos los géneros, temas y nanogéneros de las películas (por URLs)

```{r}
df_letterboxd_like_con_urls <- df_letterboxd_like_con_urls |>
  rowwise() |>
  mutate(
    genres = list(
      read_html(genres_url) |>
        html_nodes(".capitalize:nth-child(2) .text-slug") |>
        html_text() |>
        paste(collapse = ", ")
    ),
    themes = list(
      read_html(themes_url) |>
        html_nodes(".title .label") |>
        html_text() |>
        paste(collapse = ", ")
    ),
    nanogenres = list(
      read_html(nanogenres_url) |>
        html_nodes(".title .label") |>
        html_text() |>
        paste(collapse = ", ")
    )
  ) 
```

-   Usamos "`rowwise()`" para que lo que hagamos se aplique fila por fila.

-   Dentro del "`mutate`" creamos tres nuevas columnas: "genres", "themes" y "nanogenres".

-   Para cada una de esas columnas usamos "`read_html`" para leer la página web correspondiente (usando los URLs que creamos antes).

-   Usamos "`html_nodes`" para seleccionar los elementos HTML que contienen la información que queremos (los géneros, temas y nanogéneros). Para poder obtener esos códigos, tuve que instalar una extensión en mi navegador web.

-   Usamos "`html_text`" para extraer el texto de esos elementos HTML.

-   Usamos "`paste(collapse = ", ")`" para juntar todos los géneros, temas y/o nanogéneros (según sea el caso) en un solo texto, y este siendo separado por comas (por eso el ", ").

#### Arreglamos el tema de las comas y separamos categorías

```{r}
df_like_separados <- df_letterboxd_like_con_urls |>
  mutate(
    genres = as.character(genres),
    themes = as.character(themes),
    nanogenres = as.character(nanogenres)
  ) |>
  drop_na(genres, themes, nanogenres) |>
  separate_rows(genres, sep = ",\\s*") |>
  separate_rows(themes, sep = ",\\s*") |>
  separate_rows(nanogenres, sep = ",\\s*") |>
  mutate(
    genres = str_trim(genres),
    themes = str_trim(themes),
    nanogenres = str_trim(nanogenres)
  )
```

-   Usamos "`mutate`" para asegurarnos de que las columnas "genres", "themes" y "nanogenres" estén en formato character.

-   Usamos "`drop_na`" para eliminar filas que tengan NA en las columnas "genres", "themes" o "nanogenres".

-   Usamos "`separate_rows`" para separar las categorías en filas individuales. El separador es una coma seguida de cualquier cantidad de espacios `(",\s*")`.

    -   En "(,\\s\*)", la "," es por las comas que hay en el texto que separa las palabras, los "\\s(asterisco)" cualquier cantidad de espacios.

### Procesamos los géneros, temáticas y nanogéneros

#### Crear dataframes separados por cada categoría (género, temática y nanogénero)

Hay muchas filas repetidas con la misma info por película. Para ello, creamos dataframes separados para cada categoría (géneros, temas, nanogéneros)

**Género:**

```{r}
df_like_genres <- df_like_separados |>
  mutate(genres = as.character(genres)) |>
  drop_na(genres) |>
  separate_rows(genres, sep = ",\\s*") |>
  distinct(name, genres, .keep_all = TRUE) |>
  mutate(genres = str_trim(genres))
```

-   Usamos "`mutate`" para asegurarnos de que la columna "genres" esté en formato character.

-   Usamos "`drop_na`" para eliminar filas que tengan NA en la columna "genres".

-   Usamos "`separate_rows`" para separar los géneros en filas individuales. El separador es una coma seguida de cualquier cantidad de espacios (",\\s\*").

-   Usamos "`distinct(name, genres, .keep_all = TRUE)`" para eliminar filas duplicadas basándonos en las columnas "name" y "genres". Esto asegura que cada combinación de película y género sea única en el dataframe resultante y no vaya a haber una película con el mismo género dos veces (lo mismo puede suceder si lo aplicamos a temáticas y a nanogéneros).

    -   Ejemplo: En los nanogéneros de la película "La Sociedad de los Poetas Muertos" tenía repetida la palabra "teacher" varias veces. Por lo que este código sirve para que se eliminen las filas donde se repita el nombre de la película y los nanogéneros duplicados.

-   Usamos "`mutate`" junto a "`str_trim`" para eliminar espacios en blanco que se nos hayan pasado.

**Temática:**

```{r}
df_like_themes <- df_like_separados|>
  mutate(themes = as.character(themes)) |>
  drop_na(themes) |>
  separate_rows(themes, sep = ",\\s*") |>
  distinct(name, themes, .keep_all = TRUE) |>
  mutate(themes = str_trim(themes))
```

**Nanogénero:**

```{r}
df_like_nanogenres <- df_like_separados |>
  mutate(nanogenres = as.character(nanogenres)) |>
  drop_na(nanogenres) |>
  separate_rows(nanogenres, sep = ",\\s*") |>
  distinct(name, nanogenres, .keep_all = TRUE) |>
  mutate(nanogenres = str_trim(nanogenres))
```

#### Calcular la cantidad de veces que se observan los géneros, temáticas y nanogéneros.

Sacamos el N para así usarlo como Eje Y:

**Géneros**

```{r}
df_like_genres_con_conteo <- df_like_genres |>
  drop_na() |>
  group_by(genres) |>
  summarise(n = n())
```

-   Usamos "`drop_na()`" para que elimine los N/A

-   Usamos "`group_by(genre)`" para agrupar por género

-   En base a la agrupación anterior, usamos "`summarise (n = n())`" para que saque la cantidad por género.

    -   El primer "n" siendo el nombre que le quiero poner a la categoría, en ese caso le pusimos n. Y el segundo "`n()`" es para que calcule la cantidad que hay del `group_by`, que en este caso sería de géneros.

**Temáticas:**

```{r}
df_like_themes_con_conteo <- df_like_themes |>
  drop_na() |>
  group_by(themes) |>
  summarise(n = n())
```

**Nanogéneros:**

```{r}
df_like_nanogenres_con_conteo <- df_like_nanogenres |>
  drop_na() |>
  group_by(nanogenres) |>
  summarise(n = n())
```

#### Pasamos ese "n" como tipo numérico y verificamos el cambio

**Géneros:**

```{r}
as.numeric(df_like_genres_con_conteo$n)
sapply(df_like_genres_con_conteo, class)
```

**Temáticas:**

```{r}
as.numeric(df_like_themes_con_conteo$n)
sapply(df_like_themes_con_conteo, class)
```

**Nanogéneros:**

```{r}
as.numeric(df_like_nanogenres_con_conteo$n)
sapply(df_like_nanogenres_con_conteo, class)
```

### Graficamos

**Géneros:**

```{r}
grafico_like_genres <- df_like_genres_con_conteo |>
  drop_na() |>
  filter(genres != "") |>
  top_n(25) |>
  ggplot(aes(x = reorder(genres, -n), y = n)) + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = scales::wrap_format(30)) +
  geom_bar(stat="identity", fill = "#FFAD5E") +
  labs(title="Películas por Género - Likes",
       x="Géneros",
       y="Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![Películas por género](/Output/plot_peliculas_genero.png)

-   Usamos "`drop_na()`" para eliminar los NA del dataframe.

-   Usamos "`filter(genres != "")`" para eliminar las variables que no son N/A en sí, sino que blancos.

-   Usamos "`top_n(25)`" para que nos aparezcan las 25 variables con mayor cantidad.

-   Usamos "`scale_y_continuous(labels = scales::comma)`" para que no se muestren la cantidad de votos como anotación científica, sino como números enteros.

-   Usamos "`ggplot(aes(x = reorder(genres, -n), y = n))`" para definir que el género sea el Eje X, mientras que la cantidad que se repite el género sería el eje Y.

    -   Usamos "`reorder(genres, -n)`" para reordenar el Eje X (parritos o coalición), según la cantidad que haya por género, al usar el "-" le pedimos que sea en orden descendiente.

-   Usamos "`geom_bar(stat="identity", fill = "#Código")`" para crear un gráfico de barras. Asimismo, usamos "#Código" como el color escogido para de las barras. En este caso, usaremos los colores del logo de Letterboxd para género, temática y nanogénero.

-   Usamos "`labs()`" para agregar los textos. En este caso, para crear un título y etiquetas a los Ejes X e Y.

-   Usamos "`theme_minimal()`" para otorgarle un tema minimalista al gráfico.

**Temáticas:**

```{r}
grafico_like_themes <- df_like_themes_con_conteo |>
  filter(themes != "") |>
  top_n(25) |>
  ggplot(aes(x = reorder(themes, -n), y = n)) + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = scales::wrap_format(100)) +
  geom_bar(stat="identity", fill = "#86FC81") +
  labs(title="Películas por Temática - Likes",
       x="Temáticas",
       y="Cantidad") +
  theme_minimal() +
  coord_flip()
```

![Películas por temática](/Output/plot_peliculas_tematicas.png)

-   Usamos "`coord_flip()`" para voltear el gráfico, de modo que las barras sean horizontales en lugar de verticales. Esto lo hicimos con el objetivo de que el texto de los partidos o coaliciones sea más legible y no se sobrelapen uno a los otros.

**Nanogéneros:**

```{r}
grafico_like_nanogenres <- df_like_nanogenres_con_conteo |>
  filter(nanogenres != "") |>
  top_n(25) |>
  ggplot(aes(x = reorder(nanogenres, -n), y = n)) + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = scales::wrap_format(100)) +
  geom_bar(stat="identity", fill = "#81C7FC") +
  labs(title="Películas por Nanogénero - Likes",
       x="Nanogénero",
       y="Cantidad") +
  theme_minimal() +
  coord_flip()
```

![Películas por nanogénero](/Output/plot_peliculas_nanogenero.png)
