

# 📚 librerias ---------------------------------------------------------------



library(tidyverse)
library(rvest)
library(janitor)


# ☠️ scrap and wrangling los datos -------------------------------------------

# tabla totales por equipo

suppressWarnings({# me dan mucho por culo los warnings es como si hubieras hecho algo mal
  totales  <- "https://www.lebplata.es/estadisticas.aspx" %>%
    read_html() %>%
    html_element("table:nth-of-type(2)") %>%
    html_table() %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    separate(t2, c("t2_c", "t2_i"), "/") %>% #seperamos los tiros en convertidos e intentados
    separate(t3, c("t3_c", "t3_i"), "/") %>%
    separate(tc, c("tot_c", "tot_i"), "/") %>%
    separate(tl, c("tl_c", "tl_i"), "/") %>%
    mutate(across(c(par:va), parse_number))
})

totales

# links e ids que necesitamos para llegar a los datos y para hacer bonitas gráficas


enlaces <- "https://www.lebplata.es/estadisticas.aspx" %>%
  read_html()

enlaces_logos <- tibble(#como no hay nada que nos devuelva esto como tal hacemos una tabla
  equipo = enlaces %>% html_elements("table:nth-of-type(2) a") %>%
    html_text("href"),
  links = enlaces %>% html_elements("table:nth-of-type(2) a") %>%
    html_attr("href"),
  logos = enlaces %>% html_elements("table:nth-of-type(2) img") %>%
    html_attr("src")
) %>%
  mutate(id = str_extract(links, "[0-9]+"))#extrae el id del link que corresponde al equipo

enlaces_logos

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




# propuesta Angel LLuis -----------------------------------------------------


# 🔡 text caption--------------------------------------------------------------------



twitter <- "<span style='color:#000000;font-family: \"Font Awesome 6 Brands\"'>&#xE61A;</span>"
tweetelcheff <- "<span style='font-weight:bold;'>*@elcheff*</span>"
insta <- "<span style='color:#E1306C;font-family: \"Font Awesome 6 Brands\"'>&#xE055;</span>"
instaelcheff <- "<span style='font-weight:bold;'>*@sport_iv0*</span>"
github <- "<span style='color:#000000;font-family: \"Font Awesome 6 Brands\"'>&#xF092;</span>"
githubelcheff <- "<span style='font-weight:bold;'>*IvoVillanueva*</span>"
caption <- glue("<br><br>**Datos**: *Lebplata.es* • **Gráfico**: *Ivo Villanueva* • {twitter} {tweetelcheff} • {insta} {instaelcheff} • {github} {githubelcheff}")


clasificacion <- read_html("https://www.lebplata.es/clasificacion.aspx") %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table() # p

clasificacion <- clasificacion %>% rename(equipo = Equipo)

clasificacion$equipo[clasificacion$equipo == "CB PRAT"] <- "C.B. PRAT"
clasificacion$equipo[clasificacion$equipo == "OCA GLOBAL - CB SALOU"] <- "OCA GLOBAL - CBSALOU"
totales$equipo[totales$equipo == "SANDÁ ELECTROCLIMA C.B. L´HOSPITALET"] <- "SANDA ELECTROCLIMA C.B. L'HOSPITALET"
clasificacion$equipo[clasificacion$equipo == "SANDÁ ELECTROCLIMA CBLH"] <- "SANDA ELECTROCLIMA C.B. L'HOSPITALET"

totales <- merge(totales, clasificacion, by = "equipo")



Avanzadas <- totales %>%
  mutate(
    poses = 0.96 * (tot_i + bp + (0.44 * tl_i) - ro),
    OER = 100 * (pt / poses),
    DER = 100 * (PC / poses),
    Net_Rat = OER - DER,
    EFG = 100 * (tot_c + 0.5 * t3_c) / tot_i,
    EFG_def = sum(EFG) / 14,
    Net_EFG = EFG - EFG_def,
    TOp = 100 * (bp / (tot_i + 0.44 * tl_i + bp)),
    TO_def = sum(TOp) / 14,
    Net_TO = TOp - TO_def,
    ORr = sum(ro) / 14,
    DRr = sum(rd) / 14,
    DefRp = 100 * (rd / (rd + ORr)),
    OffRp = 100 * (ro / (ro + DRr)),
    FTR = 100 * (tl_i / tot_i),
    FTRd = sum(FTR) / 14,
    Net_FTR = FTR - FTRd,
    TresPR = 100 * (t3_i / tot_i),
    equipo = ifelse(equipo == "SANDA ELECTROCLIMA C.B. L'HOSPITALET", "SANDÁ ELECTROCLIMA C.B. L´HOSPITALET", equipo)
  ) %>%
  left_join(enlaces_logos) %>% 
  mutate(logos = circle_crop(logos, border_colour = "black", border_size = 5),
         TOp= TOp/100,
         EFG = EFG/100) #para poder ponerle la libreria scales

library(ggimage)
library(magick)
library(ggtext)
library(cropcircles)
library(scales)


#hacer imagenes transparentes
transparent <- function(img) {
  
  image_fx(img, expression = "0.65*a", channel = "alpha")
}

# Net_TO VS Net_EFG

p <- ggplot(Avanzadas, aes(x = Net_TO, y = Net_EFG)) +
  ggplot2::geom_abline(slope = -3 / 4, intercept = seq(5.5, -8, -2.5), alpha = .2) +
  nflplotR::geom_mean_lines(aes(x0 = Net_TO, y0 = Net_EFG)) +
  geom_image(aes(image = logos),
    size = .12
  ) +
  ggplot2::labs(
    x = "Net_TO",
    y = "Net_EFG",
    caption = caption,
    title = "Pon lo que quieras",
    subtitle = "y aquí tambien"
  ) +
  ggplot2::theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(vjust = 5, face = "bold", size = 40, hjust = .5, family = "Roboto Condensed"),
    plot.subtitle = element_text(vjust = 10, size = 16, hjust = .5, family = "Roboto Condensed", color = "grey75"),
    plot.background = element_rect("white"),
    # panel.grid = element_blank(),
    plot.margin = unit(c(2, 1, 2, 1), "cm"),
    axis.text = element_text(
      size = 10,
      margin = margin(t = 12),
      color = "black"
    ),
    axis.title = element_text(
      size = 14,
      face = "bold",
      color = "black"
    ),
    plot.caption = element_markdown(hjust = .5, size = 7)
  ) +
  scale_y_continuous(limits = c(-7, 9))

ggsave("tovsefg.png", p, w = 8, h = 8, dpi = "retina", type = "cairo")

# DER VS OER

p2 <- ggplot(Avanzadas, aes(x = DER, y = OER)) +
  ggplot2::geom_abline(slope = -3 / 4, intercept = seq(11.5, 40.7, 5), alpha = .2) +
  nflplotR::geom_mean_lines(aes(x0 = DER, y0 = OER)) +
  geom_image(aes(image = logos),
    size = .12,
    image_fun = transparent
  ) +
  ggplot2::labs(
    x = "\nRating Defensivo",
    y = "Rating Ofensivo",
    caption = caption,
    title = "Defensa vs Ataque",
    subtitle = "LEB PLATA hasta la J7"
  ) +
  ggplot2::theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(vjust = 5, face = "bold", size = 40, hjust = .5, family = "Roboto Condensed"),
    plot.subtitle = element_text(vjust = 10, size = 16, hjust = .5, family = "Roboto Condensed", color = "grey75"),
    plot.background = element_rect("white"),
    plot.margin = unit(c(2, 1, 2, 1), "cm"),
    axis.text = element_text(
      size = 8,
      face = "bold",
      family = "Gotham",
      margin = margin(t = 12),
      color = "black"
    ),
    axis.title = element_text(
      family = "Roboto",
      size = 14,
      color = "black"
    ),
    plot.caption = element_markdown(hjust = .5, size = 7)
  ) +
  scale_x_reverse() +
  scale_y_continuous(limits = c(89, 115))

ggsave("dervsoer.png", p2, w = 8, h = 8, dpi = "retina", type = "cairo")


# TOp VS EFG

p3 <- ggplot(Avanzadas, aes(x = TOp, y = EFG)) +
  ggplot2::geom_abline(slope = -3 / 4, intercept = seq(.4235, .095, -.025), alpha = .2) +
  nflplotR::geom_mean_lines(aes(x0 = TOp, y0 = EFG)) +
  geom_image(aes(image = logos),
    size = .12,
    image_fun = transparent
  ) +
  ggplot2::labs(
    x = "TO%",
    y = "EFG%",
    caption = caption,
    title = "Tiros vs Pérdidas",
    subtitle = "LEB PLATA hasta la J7"
  ) +
  ggplot2::theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(vjust = 5, face = "bold", size = 40, hjust = .5, family = "Roboto Condensed"),
    plot.subtitle = element_text(vjust = 10, size = 16, hjust = .5, family = "Roboto Condensed", color = "grey75"),
    plot.background = element_rect("white"),
    # panel.grid = element_blank(),
    plot.margin = unit(c(2, 1, 2, 1), "cm"),
    axis.text = element_text(
      size = 8,
      face = "bold",
      family = "Gotham",
      margin = margin(t = 12),
      color = "black"
    ),
    axis.title = element_text(
      family = "Roboto",
      size = 14,
      color = "black"
    ),
    plot.caption = element_markdown(hjust = .5, size = 7)
  ) +
  scale_x_reverse(labels = percent)+
  scale_y_continuous(labels = percent, limits = c(min(Avanzadas$EFG)-.005, max(Avanzadas$EFG) + .01))

ggsave("topvsefg.png", p3, w = 8, h = 8, dpi = "retina", type = "cairo")
