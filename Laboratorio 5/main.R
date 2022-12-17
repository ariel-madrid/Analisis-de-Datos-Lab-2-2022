library(dplyr)
library(signal)

library(oce)
library(TSA)

# Lectura de datos. 

read_time_series <- function(location) {
  return(
    read.csv(location, 
             header=FALSE,
             sep='\t') %>%
      select(-2) %>%
      rename_with(~ c("PAM", "VFSC"), all_of(c("V1", "V3")))
  )
}

# Contar con carpeta en el mismo directorio con los dos archivos

raw_data_normocapnia <- read_time_series("./Archivos/RP000.txt")
raw_data_hipercapnia <- read_time_series("./Archivos/RP001.txt")

# Generación de series de tiempo

f_s = 5 # en [Hz]

ts_normocapnia <- data.frame(
  PAM =ts(data = raw_data_normocapnia$PAM, frequency = f_s),
  VFSC = ts(data = raw_data_normocapnia$VFSC, frequency = f_s)
)

ts_hipercapnia <- data.frame(
  PAM =ts(data = raw_data_hipercapnia$PAM, frequency = f_s),
  VFSC = ts(data = raw_data_hipercapnia$VFSC, frequency = f_s)
)

# Graficamos las series de tiempo:
plot.ts(ts_normocapnia,
        main = "Series de Tiempo para Normocapnia",
        plot.type = "multiple",
        nc = 2)

plot.ts(ts_hipercapnia,
        main = "Series de Tiempo para Hipercapnia",
        plot.type = "multiple",
        nc = 2)

############################################################
############################################################
# Primero analizamos el caso para paciente con normocapnia:


# Ver diferencias de potencia a través de la frecuencia
pwelch(ts_normocapnia$PAM, lwd = 1, nfft = 256)
pwelch(ts_normocapnia$VFSC, lwd = 1, nfft = 256)


# Correlaciones
# Definir retardo con lag.max

r_xy_normo <- ccf(ts_normocapnia$PAM, ts_normocapnia$VFSC,
                  lag.max = 300,
                  plot = TRUE)
w_r_xy_normo <- pwelch(r_xy_normo$acf, nfft = 256, fs = 5, plot = FALSE)

r_xx_normo <- acf(ts_normocapnia$PAM, 
            lag.max = length(ts_normocapnia$PAM),
            plot = TRUE)
w_r_xx_normo <- pwelch(r_xx_normo$acf, nfft = 256, fs = 5, plot = FALSE)


# Obtenemos la fx. de transferencia
transfer_normo <- w_r_xy_normo$spec / w_r_xx_normo$spec

# Generamos un pulso, de forma que visualicemos la respuesta, ya que
# tenemos la función de transferencia

## FALTA GENERAR EL PULSO


# Tenemos que y = x * h, donde x es la entrada y h es la función de transferencia
# * es el operador de la convolución 



# Repetimos lo mismo pero para la hipercapnia

