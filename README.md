# Compiladores
Código para la materia Compiladores de [LCC](https://dcc.fceia.unr.edu.ar), [FCEIA](https://www.fceia.unr.edu.ar), [UNR](https://www.unr.edu.ar).

Este es el código a partir del cual los estudiantes empiezan a desarrollar un compilador.

Para fijar la versión de GHC y de los paquetes se usa la herramienta [stack](https://docs.haskellstack.org/en/stable/README/).

Cómo dependencia se necesita instalar la librería para desarrolladores de ncurses (para compilar [haskeline](https://hackage.haskell.org/package/haskeline)):
```code
sudo apt-get install libncurses-dev
```

Los pasos para instalar el compilador son:

```code
stack setup
stack build
```

Luego se puede ejecutar con 
```code
stack run
```
o cargar el entorno interactivo GHCi
```code
stack ghci

stack ghci src/TypeChecker.hs
```
