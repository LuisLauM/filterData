################################################################################
########### SCRIPT PARA LA FILTRACI�N AUTOMATIZADA DE BASES DE DATOS ###########
################################################################################

================================================================================
Autores y Responsables: Wencheng Lau-Medrano (llau@imarpe.gob.pe), 
						Miguel P�rez (mperez@imarpe.gob.pe)
Instituci�n: DGIRDL-IMARPE
Versi�n: 0.1.0
================================================================================

================================================================================
PROGRAMAS REQUERIDOS
- R v. >=3.5.0
	- Windows: https://cran.r-project.org/bin/windows/base/R-3.5.0-win.exe
	- Mac OS : https://cran.r-project.org/bin/macosx/R-3.5.0.pkg
	- Otros  : https://cran.r-project.org/
- Instalar paquete 'openxlsx' (basta con hacerlo una sola vez en una comptadora)
	- Abrir R
	- Ejecutar: install.packages("openxlsx")

DATOS DE INGRESO (INPUTS)
- Colocar en la carpeta /data el archivo correspondiente a la data que se desea 
  filtrar en formato .xlsx
- El formato del archivo de ingreso es fijo. Como ejemplo, puede verse el 
  archivo data/input_example.xlsx
- Dar doble click al archivo "filtrarBaseDeDatos.bat"

RESULTADOS (OUTPUTS)
- La informaci�n filtrada se guardar� en un archivo .xlsx dentro de la carpeta 
  "/outputs"
- El archivo de salida tendr� una estructura similar al de entrada, con la 
  adici�n de columnas que representan 
  los resultados del filtro.
- El nombre del archivo contendr� como sufijo la fecha-hora a la que fue creado.
================================================================================