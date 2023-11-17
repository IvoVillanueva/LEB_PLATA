

# 📚 librerias ---------------------------------------------------------------



library(tidyverse)
library(rvest)
library(janitor)
library(glue)
library(httr)
# ☠️ scrap and wrangling los datos -------------------------------------------


liga <- c(83084, 83085)
lebplat <- function(liga) {
  url <- "https://www.lebplata.es/Inicio.aspx?tabid=8"
pgsession <- session(url)
pgform <- html_form(pgsession)[[1]]
leb <- html_form_set(pgform,
                     "_ctl0:temporadasDropDownList" = 2023,
                     "_ctl0:fasesGruposDropDownList" = liga
)


df <- session_submit(pgsession, form = leb, POST = url)


# tabla totales por equipo

suppressWarnings({# me dan mucho por culo los warnings es como si hubieras hecho algo mal
  df  <- df %>%
    read_html() %>%
    html_element("table:nth-of-type(2)") %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    separate(t2, c("t2_c", "t2_i"), "/") %>% #seperamos los tiros en convertidos e intentados
    separate(t3, c("t3_c", "t3_i"), "/") %>%
    separate(tc, c("tot_c", "tot_i"), "/") %>%
    separate(tl, c("tl_c", "tl_i"), "/") %>%
    mutate(across(c(par:va), parse_number),
           equipo = str_squish(equipo),
           lig = paste0(liga),
           lig = ifelse(lig == "83084", "ESTE", "OESTE"))
  return(df)
})

}

totales <- map_df(liga, lebplat)
totales

#
# url <- "https://www.lebplata.es/Inicio.aspx?tabid=8"
# pgsession <- session(url)
# pgform <- html_form(pgsession)[[1]]
# leb <- html_form_set(pgform,
#                      "_ctl0:temporadasDropDownList" = 2023,
#                     "_ctl0:fasesGruposDropDownList" = 83085
# )
#
#
# df <- session_submit(pgsession, form = leb, POST = url)
#
#
# # tabla totales por equipo
#
# suppressWarnings({# me dan mucho por culo los warnings es como si hubieras hecho algo mal
#   totales  <- df %>%
#     read_html() %>%
#     html_element("table:nth-of-type(2)") %>%
#     html_table() %>%
#     row_to_names(row_number = 1) %>%
#     clean_names() %>%
#     separate(t2, c("t2_c", "t2_i"), "/") %>% #seperamos los tiros en convertidos e intentados
#     separate(t3, c("t3_c", "t3_i"), "/") %>%
#     separate(tc, c("tot_c", "tot_i"), "/") %>%
#     separate(tl, c("tl_c", "tl_i"), "/") %>%
#     mutate(across(c(par:va), parse_number),
#            equipo = str_squish(equipo),
#            liga = rep(c("ESTE", "OESTE"), each = 14))
# })
#
# totales

# links e ids que necesitamos para llegar a los datos y para hacer bonitas gráficas

enlaces <- function(liga) {
  url <- "https://www.lebplata.es/Inicio.aspx?tabid=8"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[1]]
  leb <- html_form_set(pgform,
                       "_ctl0:temporadasDropDownList" = 2023,
                       "_ctl0:fasesGruposDropDownList" = liga
  )


  df <- session_submit(pgsession, form = leb, POST = url)
  e <- df %>%
  read_html()

enlaces_logos <- tibble(#como no hay nada que nos devuelva esto como tal hacemos una tabla
  equipo = e %>% html_elements("table:nth-of-type(2) a") %>%
    html_text("href"),
  links =e %>% html_elements("table:nth-of-type(2) a") %>%
    html_attr("href"),
  logos = e %>% html_elements("table:nth-of-type(2) img") %>%
    html_attr("src")
) %>%
  mutate(id = str_extract(links, "[0-9]+"))#extrae el id del link que corresponde al equipo

}
enlaces_logos <- map_df(liga, enlaces)

#función que nos devuelve una tabla con toda la info de los jugadores útil

tm_id <- enlaces_logos$id # lista de los ids de los equipos para el mapeo

plantillas <- function(tm_id) {

  equipo <- paste0("https://baloncestoenvivo.feb.es/equipo/", tm_id) %>%
    read_html() %>%
    html_element(".wrapper-text span.titulo") %>%
    html_text() #necesitamos añadir el nombre del equpo

  foto <- paste0("https://baloncestoenvivo.feb.es/equipo/", tm_id) %>%
    read_html()

  table_foto <- tibble(
    links = foto %>% html_elements(".nombre a") %>%
      html_attr("href"), #para el número de id de los jugadores
    nombre = foto %>% html_elements(".nombre a") %>%
      html_text("href"),
    foto = foto %>% html_elements(".table-data-foto img") %>%
      html_attr("src")
  ) # seleccionamos estos datos del enlace


  df <- paste0("https://baloncestoenvivo.feb.es/equipo/", tm_id) %>%
    read_html() %>%
    html_element("table") %>%
    html_table() %>%
    clean_names() %>%
    tibble() %>%
    select(nombre, nacimiento, nacionalidad, formacion, altura, peso) %>%
    left_join(table_foto, by = c("nombre" = "nombre")) %>%
    mutate(
      nombre = str_to_title(nombre), # me parece mejor con el nombre la primera en Mayúscula
      altura = parse_number(str_replace(altura, "-", "0")), #si dejamos el "-" no podemos convertirlo en número
      equipo = equipo,
      player_id = str_extract(foto, "[0-9]+"),
      team_id = str_extract(links, "[0-9]+"),
      links = paste0("https://baloncestoenvivo.feb.es/jugador/", team_id, "/", player_id) #me aseguro así que el enlace es el correcto
    )


  return(df)
}

rosters <- map_df(tm_id, plantillas)

rosters


# 🏀 función para extraer la estadisticas de los jugadores -------------------


#extraemos los links de los jugadores para el mapeo y eliminamos los jugadores sin datos

links <- rosters %>%
  select(links) %>%
  filter(!str_detect(links, "NA")) %>%
  pull()



# 🤖 Aquí he tenido que tirar de ChatGpt porque algunos links no devolvían una html_table valida --------



verificar_tabla_html <- function(links) {
  tryCatch(
    {
      # Leer el contenido HTML del enlace
      pagina <- read_html(links)

      # Intentar extraer la tabla HTML de la página
      tabla <- html_table(pagina)[[3]] # queremos la tercera tabla

      # Verificar si la tabla es válida (por ejemplo, si tiene al menos 2 columnas)
      es_valida <- ncol(tabla) >= 2

      # Crear un data frame con el enlace y el resultado de la verificación
      resultado <- data.frame(enlace = links, es_valida = es_valida)

      return(resultado)
    },
    error = function(e) {
      # Si hay un error al procesar el enlace, devolver un data frame con FALSE
      resultado <- data.frame(enlace = links, es_valida = FALSE)
      return(resultado)
    }
  )
}


# Verificar si el enlace contiene una tabla HTML válida

jug_links <- map_df(links, verificar_tabla_html)

# seleccionar los enlaces que devuelven una tabla HTML valida

jug_id <- jug_links %>%
  filter(es_valida == TRUE) %>%
  select(enlace) %>%
  pull()

# 🏀 funcion que extrae las estadísticas de los jugadores por fin-------

stats <- function(jug_id) {
  player <- rosters %>%
    filter(links == jug_id) %>%
    select(nombre, equipo, player_id) %>%
    mutate(nombre = str_to_title(nombre)) # me parece mejor con el nombre la primera en Mayúscula

  suppressWarnings({df <- jug_id %>%
    read_html() %>%
    html_element("table:nth-of-type(3)") %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    tibble() %>%
    filter(fase == "LR") %>% # en playoffs habrá que quitar este filter y tendremos dos filas por jugador si juegan playoffs o cualquier otra competición
    mutate(player_id = str_extract(paste0(jug_id), "(?<=/)[0-9]+$")) %>% #necesitamos el último numero del link. para la unión
    separate(min, c("min", "sec"), ":") %>%
    separate(t2, c("t2_c", "t2_i"), "/") %>%
    separate(t3, c("t3_c", "t3_i"), "/") %>%
    separate(tc, c("tot_c", "tot_i"), "/") %>%
    separate(tl, c("tl_c", "tl_i"), "/") %>%
    inner_join(player, by = join_by(player_id)) %>%
    select(player_id, nombre, equipo, everything()) %>%
    mutate(across(c(part:va), parse_number))



  return(df)
  })
}
stats_df <- map_df(jug_id, stats)

stats_df

# 📊 ejemplo de gráfica ------------------------------------------------------

library(gt)
library(gtExtras)


stats_df %>%
  select(nombre, equipo, part, va) %>%
  filter(va > mean(va)) %>%
  mutate(va_m = round(va / part, 1)) %>%
  arrange(desc(va_m)) %>%
  slice(1:12) %>%
  left_join(rosters %>% select(nombre, foto)) %>%
  left_join(enlaces_logos %>% select(equipo, logos)) %>%
  select(foto, nombre, logos, equipo:va_m) %>%
  gt() %>%
  gtExtras::gt_img_rows(foto) %>%
  gtExtras::gt_img_rows(logos)

