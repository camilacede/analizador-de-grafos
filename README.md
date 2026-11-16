# Analizador de grafos

Aplicación de consola desarrollada en Racket que analiza grafos y determina a qué categorías pertenecen según sus vértices y aristas.

El programa lee los grafos desde un archivo de entrada y evalúa diferentes propiedades para clasificarlos.

## Funcionalidades

- Identificación de grafos nulos, vacíos y unitarios.
- Verificación de grafos simples y conexos.
- Detección de grafos completos.
- Identificación de grafos bipartitos y bipartitos completos.
- Verificación de propiedades de planaridad.
- Identificación de árboles y grafos eulerianos.
- Procesamiento de múltiples grafos desde un archivo de entrada.

## Tecnologías utilizadas

- Racket

## Estructura del proyecto

```text
analizador-de-grafos/
├── src/
│   └── main.rkt
├── data.io
└── README.md
```

## Ejecución

1. Tener instalado Racket.
2. Abrir una terminal en la carpeta principal del proyecto.
3. Ejecutar el programa con el comando:

   ```bash
   racket src/main.rkt
   ```

El programa leerá automáticamente los grafos almacenados en `data.io` y mostrará su clasificación en la consola.