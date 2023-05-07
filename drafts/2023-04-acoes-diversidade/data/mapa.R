library(ggplot2)
censo_2020 <- readr::read_rds("drafts/2023-04-acoes-diversidade/data/censo_2020.rds")

sumarizacao_onde_mora <- censo_2020 |> 
  dplyr::count(onde_mora)


# Municipio de SP
# https://geosampa.prefeitura.sp.gov.br/PaginasPublicas/_SBC.aspx
# shape_regioes <- "~/Downloads/REGIAO5/SAD69-96_REGIAO5.shp"
# regioes_muni_sp <- sf::read_sf(shape_regioes)
# regioes_muni_sp_com_respostas <- regioes_muni_sp |> 
#  # sf::
#   tibble::add_column(n_respostas = c(12, 11, 37, 36, 1)) 



# Região metropolitana de SP
# http://datageo.ambiente.sp.gov.br/geoportal/catalog/search/resource/details.page?uuid=%7B029C065B-F460-4039-AD90-09B50AC6E5A0%7D

shape_rmsp <- "drafts/2023-04-acoes-diversidade/data/REGIOES_METROPOLITANAS_2016_POL/REGIOES_METROPOLITANAS_2016_POL.shp"
rmsp <- sf::read_sf(shape_rmsp) |> 
  dplyr::filter(Nome == "Regiao Metropolitana de Sao Paulo") |> 
  tibble::add_column(n_respostas = c(21)) |> 
  dplyr::mutate(centroide = sf::st_centroid(geometry),
                long = purrr::map_dbl(centroide, ~.x[1]),
                lat = purrr::map_dbl(centroide, ~.x[2])) |> 
  dplyr::mutate(Nome_en = "Metropolitan Region of Sao Paulo")


# https://datageo.ambiente.sp.gov.br/app/?ctx=DATAGEO#
distritos_sp <- "drafts/2023-04-acoes-diversidade/data/DISTRITO_MUNICIPAL_SP_SMDU/DISTRITO_MUNICIPAL_SP_SMDUPolygon.shp"
shp_distritos_sp <- sf::read_sf(distritos_sp)

# https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/subprefeituras/Fotos%20Site/Teste_mapa_prefeituras.jpg

shp_regioes_sp <- shp_distritos_sp |> 
  dplyr::mutate(regiao = 
  dplyr::case_match(Codigo, 
                    c(51, 89, 92, 3, 61, 63, 95, 39, 42, 70,
                      86, 81, 82, 13, 21, 50, 29, 11 ) ~ "Norte",
                    c(52, 55, 30, 23, 79, 43, 46, 19, 17, 83, 22, 58, 38, 16,
                      71, 32, 77, 15, 27, 68, 34, 90 ) ~ "Sul",
                    c(65, 35, 62, 45, 2, 12, 67, 6, 48, 94, 54, 41, 
                      40, 88, 60) ~ "Oeste",
                    c(14, 49, 7, 26, 69, 9, 78, 66) ~ "Centro",
                    c(1, 4, 5, 8, 10, 18, 20, 24, 25, 28, 31, 33,
                      36, 37, 44, 47, 96, 53, 56, 57, 59, 64, 72,
                      73, 74, 75, 76, 80, 84, 85, 87, 91, 93) ~ "Leste"
                    
    
  ))


ggplot() +
  geom_sf(aes(fill = regiao), data = shp_regioes_sp)


shp_regioes_sp_resp <-  shp_regioes_sp |>
  dplyr::group_by(regiao) |>
  dplyr::summarise() |>
  sf::st_simplify(dTolerance = 0.1) |>
  dplyr::arrange(regiao) |>
  tibble::add_column(n_respostas = c(26, 12, 11, 36, 37)) |> 
  dplyr::mutate(centroide = sf::st_centroid(geometry),
                long = purrr::map_dbl(centroide, ~.x[1]),
                lat = purrr::map_dbl(centroide, ~.x[2])) |> 
  dplyr::mutate(cor = dplyr::if_else(n_respostas > 30,"white", "black")) |> 
  dplyr::mutate(
    regiao_en = dplyr::case_match(
      regiao,
      "Centro" ~ "",
      "Leste" ~ "East",
      "Norte" ~ "North",
      "Oeste" ~ "West",
      "Sul" ~ "South"
    )
  )
 

# mapa
ggplot() +
  geom_sf(aes(fill = n_respostas),  data = rmsp) +
  geom_sf(aes(fill = n_respostas), data = shp_regioes_sp_resp) +
  geom_text(aes(x = long, y = lat, label = regiao, color = cor), size = 3, 
            data = shp_regioes_sp_resp) + 
  geom_text(aes(x = long *0.992, y = lat, label = Nome), size = 3, 
            color = "black",
            data = rmsp) + 
  scale_fill_distiller(direction = 1, palette = "Purples") + 
  scale_color_identity() + 
  theme_void() +
  theme(legend.position = "top") +
  labs(fill = "Quantidade de respostas") 


# em INGLES

ggplot() +
  geom_sf(aes(fill = n_respostas),  data = rmsp) +
  geom_sf(aes(fill = n_respostas), data = shp_regioes_sp_resp) +
  geom_text(aes(x = long, y = lat, label = regiao_en, color = cor), size = 3, 
            data = shp_regioes_sp_resp) + 
  geom_text(aes(x = long *0.992, y = lat, label = Nome_en), size = 2.5, 
            color = "black",
            data = rmsp) + 
  scale_fill_distiller(direction = 1, palette = "Purples") + 
  scale_color_identity() + 
  theme_void() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, color = "#562357")) +
  labs(fill = "", title = "Number of responses in the City of Sao Paulo \n and the Metropolitan Region of Sao Paulo \n") 

