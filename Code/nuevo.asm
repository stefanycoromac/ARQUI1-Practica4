include ope.asm 

.model small
.stack 100h
.data 
; -------------------- SEGMENTO DE DATOS ---------------------------
encabezado db  0ah, 0dh, ' ============================================================================',
			   0ah, 0dh, '  UNIVERSIDAD DE SAN CARLOS DE GUATEMALA', 
			   0ah, 0dh, '  Facultad de Ingenieria',
			   0ah, 0dh, '  Ciencias y Sistemas', 
			   0ah, 0dh, '  Arquitectura de Computadores y Ensambladores 1',
			   0ah, 0dh, '  Nombre: Stefany Samantha Abigail Coromac Huezo',
			   0ah, 0dh, '  Carne: 201801182', 
			   0ah, 0dh, '  Seccion: A', 
			   0ah, '$'
menu db 0ah, 0dh, ' ============================================================================', 
		0ah, 0dh, '  1.  CARGAR ARCHIVO', 
		0ah, 0dh, '  2.  CONSOLA', 
		0ah, 0dh, '  3.  SALIR',
		0ah, 0dh, ' ============================================================================',
		0ah, 0dh, ' Ingrese Opcion:','  $'

encCargarArchivo db 0ah, 0dh, ' ============================= CARGAR ARCHIVO =============================== ', ' $'
encConsola db 0ah, 0dh, ' ===============================  CONSOLA   ================================= ','$'
inicioConsole db 0ah, 0dh, ' >>', ' $'
espacio db 20h, '$'

saltoLinea db 0ah, 0dh,  '$'
finObjeto db 0
comandoConsola db 30 dup('$')
comandoConsola2 db 30 dup('$') 
showMedia db 'media', '$' 
showMediana db 'mediana','$'
showModa db 'moda','$'
showMenor db 'menor','$'
showMayor db 'mayor','$'
showMayor2 db 'mayor','$'
media dw 0
mediana dw 0
moda dw 0
menor dw 0
mayor dw 0 

bufferMediaR dw 10 dup('$')
bufferMedianaR dw 10 dup('$')
bufferModaR dw 10 dup('$')
bufferMenorR dw 10 dup('$')
bufferMayorR dw 10 dup('$') 



; =============================VARIABLES PARA REPORTE =======================================================
comillas db '"' 
finReporte db 0ah,  '	}', 
			  0ah,  '}'
inicioReporte db '{', 
				 0ah,  '	"reporte":',
				 0ah,  '	{', 
				 0ah,  '		"alumno":', 
				 0ah,  '		{', 
				 0ah,  '			"Nombre":"Stefany Samantha Abigail Coromac Huezo",', 
				 0ah,  '			"Carne":"201801182",', 
				 0ah,  '			"Seccion":"A",', 
				 0ah,  '			"Curso":"Arquitectura de Computadores y Ensambladores 1"',
				 0ah,  '		},',
				 0ah,  '		"fecha":',
				 0ah,  '		{',
				 0ah,  '			"Dia":'
bufferMes db 0ah,      '			"Mes":'
bufferAnio db 0ah,     '			"Año":'
cerrarObject1 db 0ah,  '		}'
inicioHora db 0ah,     '		"hora":', 
			  0ah, 	   '		{', 
			  0ah, 	   '			"Hora":'
bufferMinutos db 0ah,  '			"Minutos":'
bufferSegundos db 0ah, '			"Segundos":'
inicioResultados db 0ah, '		"resultados":',
			        0ah, '		{', 
			        0ah, '			"Media": '
bufferMediana db 0ah,    ' 			"Mediana": "Aqui estuviera si tan solo lo hubiera hecho"'
bufferModa db 0ah,       '			"Moda": "Aqui estuviera si tan solo lo hubiera hecho"'
bufferMenor db 0ah,      '			"Menor": '
bufferMayor db 0ah,      '			"Mayor": '
inicioPadre db 0ah,    '		"operaciones":'
comilla db 0ah, '		"'
cierreComillas db  '":'
cierreComillas2 db '":', '$'
inicioArreglo db 0ah,  '		['
finArreglo db 0ah,     '		]'
coma db ','
coma2 db ',','$'
inicioOR db 0ah, 09h, 09h, 09h,'{',
			0ah, 09h, 09h, 09h, 09h, '"', '$'
finOR db    0ah, 09h, 09h, 09h ,'}', '$'


numeroEscribir dw 0


bufferFecha db 2 dup(' ')
handleFicheroReporte dw ?

msgErrorAbrir db 0ah, 0dh,'Error al Abrir el archivo', '$'
msgErrorLeer db 0ah, 0dh,'Error al leer el archivo', '$'
msgErrorCrear db 0ah, 0dh,'Error al crear el archivo', '$'
msgErrorEscribir db 0ah, 0dh, 'Error al escribir archivo', '$'

bufferOperaciones db 200 dup('$')

msgNoIguales db 0ah, 0dh, ' La cadena no es igual ','$'
msgIguales db 0ah, 0dh, ' La cadena es igual ','$'


;===================== Comandos =====================
CommandShowMedia db 0ah, 0dh, 'Resultado estadistico media:  ', '$'
CommandShowMediana db 0ah, 0dh, 'Comando Show Mediana', '$'
CommandShowModa db 0ah, 0dh, 'Comando Show Moda', '$'
CommandShowMayor db 0ah, 0dh, 'Resultado estadistico mayor:  ', '$'
CommandShowMenor db 0ah, 0dh, 'Resultado estadistico menor:  ', '$'
CommandID db 0ah, 0dh, 'Buscar ID ', '$'
errorConsole db 0ah, 0dh, ' No se reconoce el comando ', '$'
probando db 0ah, 0dh, 'Probando que entro aqui','$'
msgnombrePadre db 0ah, 0dh, 'Nombre del Objeto Padre: ', '$'
msgTotalOperaciones db 0ah, 0dh, 'Total de Operaciones: ', '$'


;------LECTURA DEL ARCHIVO .JSON ------------------------------------------
msgErrorOpen db 0ah, 0dh,'Error al Abrir el archivo', '$'
msgErrorRead db 0ah, 0dh,'Error al leer el archivo', '$'
msgErrorCreate db 0ah, 0dh,'Error al crear el archivo', '$'
msgErrorWrite db 0ah, 0dh, 'Error al escribir archivo', '$'
msgArchivoLeido db 0ah, 0ah, 0dh, '> Archivo Leido con exito! ', '$'
msgFinOperacion db 0ah, 0dh, 'FIN DE OPERACION ', '$'
msgCargarArchivo db 0ah, 0dh, ' INGRESE RUTA: ','$'
rutaArchivo db 100 dup(?)
bufferLectura db 10000 dup('$')
limpiarD db 21 dup('$')
bufferEscritura db 20 dup(' ')
handleFichero dw ?
dividirpor db 0

suma1 db 0ah, 0dh, 'SUMA', '$'
resta1 db 0ah, 0dh, 'RESTA', '$'
multiplicacion1 db 0ah, 0dh, 'MULTIPLICACION', '$'
division1 db 0ah, 0dh, 'DIVISION', '$'

;============Variables para Lectura de JSON (ANALIZADOR) ================
 
estado db 0
contadorPadre db 0
contadorNumero db 0
inicioArchivo db 0
contadorLlaves db 0
bufferAux db 30 dup('$')
finOpe db 0
msgRegresarPila db 0ah, 0dh, 'Regresando registros a la Pila', '$'
msgRevisarPila db 0ah, 0dh, 'Revisando pila ', '$'
msgIDoperacion db 0ah, 0dh, 'Este es el ID de una nueva operacion: ','$' 
msgResultado db 0ah, 0dh, 'Resultado ', '$'
msgnumeroNegativo db 0ah, 0dh, 'Es un numero negativo', '$'
negativo db 0
auxiliar dw 0


;==============OPERACIONES==============================================
resultados db 30 dup(0)
totalOperaciones dw 0
operaciones dw 20 dup('$')
total dw 0
imprimirNumero db 30 dup('$')
numero1 db 100 dup('$')
numero2 db 100 dup('$')
nombrePadre db 20 dup('$'), '$' ; AQUI LO TENGO COMO LO GUARDO DESDE EL ARCHIVO 
extension db '.json', '$'
nombreReporte db 20 dup('$'),'$'  ; NOMBRE DEL REPORTE CON LA EXTENSION 
nombreOperacion db 12 dup('$')
finnnn db '$'
namePadre db 30 dup('$'), '$'
contadorbufferPadre db 0
auxMayor dw 0
auxMenor dw 0





.code 
main proc 

		MOV ah,09h
		MOV dx,@data
		MOV ds, dx		
		print encabezado
		
	Ingresar:		
		print menu
		XOR al, al
		MOV ah, 01h 
		INT 21h 
		CMP al, 31h  ; 1
		JE CargarArchivo
		CMP al, 32h  ; 2
		JE Consola
		CMP al, 33h  ; 3
		JE Salir
		XOR al, al 
		JMP Ingresar

	CargarArchivo:
		print encCargarArchivo
		print msgCargarArchivo
		opcion1
		calcularMedia operaciones
		calcularMayor operaciones
		calcularMenor operaciones
		JMP Ingresar

	Consola:
		print encConsola 
		obtenerComandos
		JMP Ingresar

	Salir:
		MOV ah,4ch
		INT 21h	

main endp
end

		;crearReporte
		;calcularMedia 
		;getNumero numero1
		;getNumero numero2
		;ConvertirAscii numero1
		;MOV bx, ax 
		;PUSH bx 
		;ConvertirAscii numero2
		;POP bx 

		;ADD ax,bx 
		;ConvertirString imprimirNumero
		;print imprimirNumero