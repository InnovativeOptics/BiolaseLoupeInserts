library(purrr)

oem_data <- readxl::read_excel("data/Dental_data.xlsx",
                               sheet = 1)


our_data <- readxl::read_excel("data/Dental_data.xlsx",
                               sheet = "Lens_details") %>%
  mutate(VLT = scales::percent(as.numeric(VLT)),
         `Price(from)` = scales::dollar(as.numeric(`Price(from)`)))

loupe_data <- readxl::read_excel("data/Dental_data.xlsx", sheet = "Loupe_types")

mfg_filt <- loupe_data %>%
  filter(Mfg == "Andau")

filt_data_loupe <-
  loupe_data %>%
    filter(Mfg == "Andau" &
             Mod == "Blues LG" &
             Size == "One size")%>%
    mutate("Selected Device" = "Clarion",
           "Selected Loupes" = paste0(`Mfg`, " ", `Mod`, " (", `Size`, ") "),
           "Compatible Loupe Insert" = `Insert Part Number`,
           .keep = "none")

oem_data <- readxl::read_excel("data/Dental_data.xlsx",
                               sheet = 1)
dent_mod <- oem_data %>%
  filter(`Laser Model` == 'Clarion')
filt_data_laser <- map(unique(dent_mod$`Eyewear Lens Compatible`), ~tibble(filter(our_data, Lens == .x)))


filt_data_loupe$`Compatible Loupe Insert`

all(sapply(filt_data_laser, "[[", 'Lens') == "Pi1")


## filt_data_laser[[1]][['Price(from)']]




if(grepl("Andau", filt_data_loupe['Selected Loupes'])) {
  filt_data_laser2 <- lapply(filt_data_laser, function(x) {
    x[['Price(from)']] <- NA
    x
  })
}

tezz <- map(1:length(filt_data_laser), ~HTML(
  c(
    '<div class="shadow p-3 mb-5 bg-body rounded">
      <div class="row">
      <div class="col-sm-5">
        <div align="left">
        <a href="',
    filt_data_laser[[.x]]$Website,
    '", target = "_blank", title = "', filt_data_laser[[.x]]$Lens,'frame styles">',
    paste0(filt_data_loupe[,3], ".", filt_data_laser[[1]]$Lens),'
        </a>
        </div>
        <div align="center">
        <a href="',
    filt_data_laser[[.x]]$Website,
    '", target = "_blank", title = "', filt_data_laser[[.x]]$Lens,'frame styles">
        <img src="',
    filt_data_laser[[.x]]$Image,
    '", width = 40%>
        </a>
        <a href="',
    filt_data_laser[[.x]]$Website,
    '", target = "_blank", title = "', filt_data_laser[[.x]]$Lens,'frame styles">
        <img src="',
    filt_data_laser[[.x]]$Graph,
    '", width = 100%>
        </a>

        </div>
        </div>
        <div class="col-sm-7">

        <dl>
        <dt style="font-size:0.55em", align="left"><strong>Lens Material</strong></dt><dd style="font-size:0.55em", align="left"> ',filt_data_laser[[.x]]$Material,'</dd>
        <dt style="font-size:0.55em", align="left"><strong>Price (from)</strong></dt><dd style="font-size:0.55em", align="left"> ',filt_data_laser[[.x]]$`Price(from)`,'</dd>
        <dt style="font-size:0.55em", align="left"><strong>Optical Density Marking </strong></dt><dd style="font-size:0.55em", align="left">',filt_data_laser[[.x]]$OD,'</dd>
        <dt style="font-size:0.55em", align="left"><strong>VLT </strong></dt> <dd style="font-size:0.55em", align="left">',filt_data_laser[[.x]]$VLT,'</dd>
        </dl>
        </div>
        </div>
      </div>'
  )
))

