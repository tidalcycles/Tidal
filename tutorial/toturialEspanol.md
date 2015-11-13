...

Bienvenido al tutorial de Tidal, Tidal es un mini lenguaje de progaramación para explorar 
patrones, diseñado para ser usado durante interpretaciones de código en vivo.
En este tutorial, pasaremos por diferentes niveles de abstracción, empezando con sonidos
y filtros, luego secuencias de sonidos y filtros, llegando así a las funciones para manipular
estas secuencias, para terminar mirando las funciones que manipulan otras funciones.
¡Mucha diversión!

# Sonidos y efectos

Con un poco de tacto, Tidal puede ser usado para controlar cualquier dispositivo que reciba MIDI
o mesajes OSC (open sound control), aunque por defecto es Dirt el software usado. Si siguió
el proceso de instalación debería tener Dirt instalado y funcionando.

Para probarlo, ejecute el siguiente código escribiéndolo en su editor de texto, luego 
presionando `ctrl-c,crtl-s` para iniciar Tidal (emacs) y luego `ctrl-enter`.

```haskell
d1 $ sound "can"
```

Usted debe escuchar un sample que se repite, es el sonido de alguien golpenado una lata.
Tidal esta diseñado pensando en música dance que es repetitiva, y se repetirá por siempre,
aunque usted puede obtener gran variedad de un solo patrón y cambiarlo durante la ejecución
(live code).

La palabra `can` en el ejemplo anterior es el nombre del sample que esta sonando, en realidad
es el nombre de una carpeta llena de samples.
Se pueden encontrar en la subcarpeta llamada samples dentro de dirt. Es posible especificar
un sonido diferente usando dos puntos seguidos por números.

```haskell
d1 $ sound "can:1"
```

Pruebe otros números para escuchar los diferentes samples que incluye dirt.

Dirt viene con una gran variedad de samples para trabajar, algunos de ellos son:

```
    flick sid can metal future gabba sn mouth co gretsch mt arp h cp
    cr newnotes bass crow hc tabla bass0 hh bass1 bass2 oc bass3 ho
    odx diphone2 house off ht tink perc bd industrial pluck trump
    printshort jazz voodoo birds3 procshort blip drum jvbass psr
    wobble drumtraks koy rave bottle kurt latibro rm sax lighter lt
    arpy feel less stab ul    
```

Reemplaze `can` con una de estas palabras para explorar.

## Silence

En este punto se preguntará como hacer que el sonido se detenga, esto se puede hacer de la 
siguiente forma:

```haskell
d1 $ silence
```

Más rápido aún, se puede silenciar todo evaluando la palabra `hush` (oprimiedo `ctrl-enter`):

```haskell
hush
```

Esto será muy útil cuando sea posible ejecutar mas de un sonido a la vez, y se quiera 
silenciarlos, `hush` los silenciará todos al instante.

## Efectos

También se pueden aplicar efectos para cambiar el sonido, o el cómo suena, por ejemplo los
filtros formantes tipo vocales:

```haskell
d1 $ sound "can:1" |+| vowel "a"
```

El operador |+| en el ejemplo es el que mezcla el sonido con el parámetro.

Pruebe cambiando la "a" por otras vocales. También se puede ejecutar el sample más rápido,
lo que hará que suba de tono:

```haskell
d1 $ sound "can:1" |+| speed "2"
```

o lento:

```haskell
d1 $ sound "can:1" |+| speed "0.5"
```

también al revés:

```haskell
d1 $ sound "can:1" |+| speed "-1"
```

Es posible aplicar varios efectos al mismo tiempo:

```haskell
d1 $ sound "can:1" |+| vowel "a" |+| speed "-1"
```

Acá esta la lista completa de efectos con los que podrá jugar:

-------------------------------------------------------------------------------------
Nombre		| Descripción
----------------|--------------------------------------------------------------------
`accelerate`	| patrón de números que acelera o frena los samples mientras se ejecutan.
`bandf`		| patrón de números de 0 a 1, establece la frecuencia central de un BPF.
`bandq`		| patron de números de 0 a 1, establece Q en  un BPF.
`begin`		| patrón de números de 0 a 1, Salta el principio de cada sample, por ejemplo, 0.25 corta el primer cuarto de cada sample.
`coarse`	| seudo-remuestreo, un patrón de números para bajar el muestreo, por ejemplo, 1 es original, 2 la mitad, 3 la tercera parte.
`crush`		| bit crushing, patrón de números de 1 a 16, con 1 reduciendo el bit depth al máximo y 16 dejándolo igual.   
`cutoff`	| patrón de números de 0 a 1, establece la frecuencia de corte de un LPF.
`delay`		| patrón de números de 0 a 1, establece en nivel del delay.
`delayfeedback`	| patrón de números de 0 a 1, establece la cantidad de feedback del delay.
`delaytime`	| patrón de números de 0 a 1, establece el largo del delay.
`end`		| igual a begin, pero aplica al final de cada sample acortándolo.
`gain`		| patrón de números para especificar el volumen, valores inferiores a 1 lo hacen mas suave, valores superiores a 1 lo hacen mas fuerte.
`hcutoff`	| patrón de números de 0 a 1, establece la frecuencia de corte de un HPF.
`hresonance`	| patrón de números de 0 a 1, establece la resonancia de un HPF.
`pan`		| patrón de números de 0 a 1, panea  de izquierda a derecha (si hay estéreo).
`resonance`	| patrón de números de 0 a 1, establece la resonancia de un LPF.
`shape`		| distorsión de onda, 0 no distorsiona, 1 para máxima distorsión.
`sound`		| patrón de strings que representan nombres de samples (requerido siempre)
`speed`		| patrón de números de 0 a 1, cambian la velocidad del sample, también es una forma económica de cambiar la altura del sample.
`unit`		| patrón de strings que representan la unidad en la que `speed`esta expresado. Puede ser, 'rate', 'cycle', (cycle/n), o 'secs'
`vowel`		| filtro para hacer que los samples suenen como vocales, el patrón puede tener cualquier vocal, use (~) para que no tenga efecto.

---------------------

# Patrones continuos 

Hay patrones continuos incorporados, lo que permite aplicar cosas como ondas sinusoidales a los patrones que reciben números como parámetros.

Por ejemplo, para usar una onda sinusoidal como parámetro de `pan`:

```haskell
d1 $ sound "can:1" |+| vowel "a" |+| speed "-1"
```

`sine1` es un patrón tipo onda sinusoidal que viaja desde 0 hasta 1 y de vuelta a 0 cada ciclo. También está `tri1`,`saw1` y `square1`, para ondas triangulares, diente de sierra y onda cuadrada respectivamente. Si no escribe el '1' al final de los nombres, el patrón oscilará entre -1 y 1. lo que a veces puede ser útil, mas adelante veremos como manipular estos patrones para que recorran cualquier rango.

# Secuencias

Seguramente ya se cansó de escuchar el mismo sample una y otra vez, asi que avancemos a las secuencias. Las secuencias de Tidal le permiten unir varios samples, estirar la secuencia o superponerla de varias formas interesantes, al igual que mezclarlas aleatoriamente.
Puede hacer un ciclo de Tidal con mas de un sample así:


```haskell
d1 $ sound "drum drum:1"
```

¡Bombo y redoblante para siempre!

Ya habrá notado que sin importar cuantas cosas ponga en un patrón de Tidal, recorrerlo tomo la misma cantidad de tiempo. Por ejemplo, agrupando tres sonidos en el mismo ciclo:


```haskell
d1 $ sound "drum drum:1 can"
```

Por cierto, de vez en cuando, ilustraremos los conceptos con patrones de color, por ejemplo:


<example>
"blue orange green"
</example>

Puede entender este patrón en términos de sonido, se lee de izquierda a derecha. El eje horizontal representa el tiempo.

El símbolo `~` representa un silencio o pausa.


```haskell
d1 $ sound "drum drum:1 ~"
```
Puede jugar un poco con patrones no convencionales, como este, que tiene siete pasos:

```haskell
d1 $ sound "drum ~ can ~ ~ drum:1 ~"
```

## Subdividiendo secuencias

Cada paso de un patrón puede ser subdividido, por ejemplo en el siguiente código, tres samples `can` son ejecutados en la misma cantidad de tiempo que toma un `drum`:

```haskell
d1 $ sound "drum drum [can can:4 can:5] drum"
```

De nuevo, acá esta el equivalente visual, para aclarar lo que significa subdividir un evento en tres subeventos:

<example>
"blue green [purple grey black] orange"
</example>

Como puede ver, los corchetes señalan el inicio y el fila de la subdivisión. Otro ejemplo,
con dos eventos, subdivididos en diferentes números de subdivisiones.

<example>
"[blue green] [purple grey black]"
</example>

De hecho se puede seguir subdividiendo dentro de una subdivisión:

```haskell
d1 $ sound "drum drum [can [can:4 can:6 can:3] can:5] drum"
``` 

Y su equivalente visual:

<example>
"orange purple [red [green grey brown] yellow] pink"
</example>

## Patrones en capas

Los corchetes le permiten especificar mas subpatrones separándolos por comas:

```haskell
d1 $ sound "drum [can cp, can bd can:5]"
```
Como puede escuchar, los dos patrones están en capas. Dado que tienen diferente extensión
(uno tiene dos sonidos, el otro tres), Se escucha un efecto polirítmico muy interesante. Se puede apreciar mejor en una solo subdivision así:

```haskell
d1 $ sound "[can cp, can bd can:5]"
```
Se puede visualizar superponiendo las diferentes partes del patrón, así se ve claramente como van simultáneas:

<example>
"[orange purple, red green pink]"
</example>

Si usa llaves en vez de corchetes los subpatrones se acomodan de manera diferente en las 
capas, de tal forma que el sonido se alinea y la extensión diferente de los patrones da una 
sensación diferente:

```haskell
d1 $ sound "{can can:2, can bd can:5}"
```
Así se ve esto:

<example>
"{orange purple, red green pink}"
</example>

El problema con el código anterior es que el patrón esta estructurado en varios 'ciclos'
(en este caso, tres) y solo podemos ver el primer ciclo. Saltemos un poco y usemos la función `density` para crear mas ciclos:

<example>
density 3 "{orange purple, red green pink}"
</example>

De nuevo, se pueden poner en capas mas de un subpatrón:

```haskell
d1 $ sound "[can cp, can bd can:5, arpy arpy:2 ~ arpy:4 arpy:5]"
```

Y subdividir mas:

```haskell
d1 $ sound "{[can can] cp, can bd can:5, arpy arpy:2 ~ [arpy:4 arpy:5] arpy:5}"
```

¡Esto puede ponerse realmente complicado y eso que no hemos llegado a las funciones aun!

## Trucos y detalles sobre Secuenciar

Un poco mas sobre secuencias, dado que es posible hacer otras cosas con ellas aún.

### Repeticiones y divisiones

Sí lo que quiere es repetir el mismo sample muchas veces, puede usar `*` para decir cuantas
veces. Por ejemplo:

```haskell
d1 $ sound "bd [can can can]"
```
Puede ser escrito así:

```haskell
d1 $ sound "bd can*3"
```
Cuando se hace live coding es importante usar bien el tiempo. Puede experimentar con números 
altos para lograr sonidos extraños:

```haskell
d1 $ sound "bd can*32 bd can*16"
```
Este patrón ejecuta tan rápido los samples que su oído no puede oír sonidos individuales,
de hecho los escucha como una frecuencia, una nota musical.

Si tiene un patrón que no tiene subpatrones, como este:

```haskell
d1 $ sound "bd can can can"
```
Puede repetir eventos sucesivos con `!`:

```haskell
d1 $ sound "bd can ! !"
```
Tambien puede 'desacelerar' un subpatrón, por ejemplo el siguiente código ejecuta `[bd arpy sn:2 arpy:2]` a la mitad de la velocidad:

```haskell
d1 $ sound "bd [bd arpy sn:2 arpy:2]/2"
```
En el primer ciclo tenemos `bd [bd arpy]`, en el segundo `bd [sn:2 arpy:2]`. Es un poco dificil
de entender, pero básicamente si no se alcanza a recorrer todo un subpatrón durante un ciclo, en el siguiente continúa donde se dejó. Esto es mas comprensible mirando un patrón de color.

<example>
density 4 "red [blue orange purple green]/2"
</example>

De nuevo usamos `density` para agrupar mas ciclos (cuatro en este caso) y así pueder ver los
cambios que ocurren de un ciclo al siguiente.

Puede obtener cosas extrañas si por ejemplo repite cuatro tercios de un subpatrón por ciclo:

```haskell
d1 $ sound "bd [bd arpy sn:2 arpy:2]*4/3"
```
Si disfruta de las métricas extrañas, ya debe estar divirtiéndose.

### Drops aleatorios

Si quiere que algo pase 'a veces', puede poner un interrogante ( ? ) al lado del evento:

```haskell
d1 $ sound "bd can? bd sn"
```
En el ejemplo anterior, el sample  `can` se ejecutará con un promedio del 50% cada vez.
Si añade un interrogante a un subpatrón se aplicará independientemente a cada elemento del
subpatrón. En el siguiente, puede no escuchar nada, puede ser el primero o el segundo:

```haskell
d1 $ sound "bd [can can:4]? bd sn"
``` 
### Bjorklund (y Euclides)

Si pone dos números en paréntesis luego de un elemento en un patrón, entonces Tidal distribuirá el primer número de sonidos equitativamente a lo largo del segundo número de pasos:

```haskell
d1 $ sound "can(5,8)"
``` 
Pero, como no es posible distribuir tres elementos a lo largo de 8 pasos discretos, el 
algoritmo hace lo mejor que puede. El resultado es un campaneo, pruebe este:

```haskell
d1 $ sound "can(5,8)"
```
Este código usa el "Algoritmo Bjorklund", el cual no fue hecho para la música sino en aplicaciones para física nuclear, lo que es bastante emocionante. Mas emocionante aún es que este algoritmo es muy similar estructuralmente a uno de los primeros algoritmos conocidos, escrito en el Libro de los elementos de Euclides en el 300 ac. Puede leer mas sobre esto en el paper [The Euclidean Algorithm Generates Traditional Musical Rhythms](http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf) por Toussaint. Ejemplos de este paper se incluyen acá, aunque algunos requieren rotación sobre un pulso particular, lea el paper para ver los detalles y las referencias.

---------------------------------------------------------------------------------------
Patrón	| Descripción
--------|------------------------------------------------------------------------------
(2,5)	| Ritmo persa del siglo trece llamado Khafit-e-ramal.
(3,4)	| El patrón básico de la Cumbia Colombiana, al igual que del Calypso de Trinidad.
(3,5)	| Si se inicia en el segundo pulso, es otro ritmo persa al igual que un ritmo popular de baile de Rumania.
(3,7)	| Ritmo Ruchenitza usado en la música popular bailable de Bulgaria.
(3,8)	| Patrón de tresillo cubano.
(4,7)	| Otro ritmo Ruchenitza usado en la música popular bailable de Bulgaria.
(4,9)	| Aksak, ritmo Turco.
(4,11)	| Patrón usado por Frank Zappa en su tema titulado Outside Now.
(5,6)	| Produce el patrón York-Samai, un ritmo árabe popular, si se empieza en el segundo beat.
(5,7)	| Patrón Nawakhat, otro popular ritmo árabe.
(5,8)	| Patrón de cinquillo cubano.
(5,9)	| Un ritmo popular en Arabia llamado Agsag-Samai.
(5,11) 	| Patrón métrico usado por Moussorgsky en Cuadros de una exposición.
(5,12)	| Patrón de palmas sudafricano llamado Venda, de canciones infantiles.
(5,16) 	| Patrón circular del Bossa-Nova Brasilero.
(7,8)	| Ritmo que se toca sobre el Bendir.
(7,12)	| Patrón del occidente africano.
(7,16)	| Patrón circular de la Samba Brasilera.
(9,16)	| Ritmo circular usado en la República de África central.
(11,24)	| Ritmo circular de los Pigmeos Aka en África central.
(13,24)	| Otro ritmo circular de los Pigmeos Aka de Sangha.
----------------------------------------------------------------------------------------

# Funciones

Hasta ahora hemos trabajado casi exclusivamente en construir secuencias, aunque se han incluido poliritmos, y manipulaciones algorítmicas simples que estan incluídas dentro de la sintáxis de Tidal. Es momento de ascender en el nivel de abstracción y ver que encontramos en el camino.

Primero miremos de cerca las funciones que hemos usado hasta ahora.

## Enviando patrones a Dirt

`d1` es una función que toma un patrón como entrada para luego enviarla a dirt. Por defecto hay diez de estos definidos, de `d1` a `d10`, lo que permite empezar y parar patrones múltiples al tiempo.

Por ejemplo, intente ejecutar cada una de estas líneas por turnos:

```haskell
d1 $ sound "bd sn"

d2 $ sound "arpy arpy:2 arpy"

d1 $ silence

d2 $ silence
``` 
La primera línea inicia un patrón bombo-redoblante, el segundo inicia un patrón un poco melodioso, luego el tercero silencia el primero (lo detiene), y el cuarto silencia el segundo (todo esta en silencio).

Es importante notar que el código escrito y el que se ejecuta al oprimir `ctrl-enter` cambia los patrones que se ejecutan en el fondo. Los patrones no cambian al momento que se edita el código, hasta que se oprime `ctrl-enter` de nuevo. Hay una pequeña desconexión  entre el código y el proceso al que hay que acostumbrarse.

## Signo de dolar `$`

Se preguntará que hace el signo de dolar `$`. Si no se lo a preguntado, puede saltarse esta sección.

El signo de dolar no hace casi nada; simplemete toma todo lo que esté a su derecha y se lo envía a la función a su izquierda. Si intentamos evaluar el siguiente ejemplo obtendremos un error:

```haskell
d1 sound "bd sn"
```
Esto ocurre porque Tidal^[bueno, Haskell, el lenguaje que esta detrás de Tidal] lee el código de izquierda a derecha y entonces le envía `sound`a `d1` antes de que `sound` reciba a `"bd sn"` como entrada, lo que produce confusión. Se puede lograr el comportamiento adecuado usando paréntesis:

```haskell
d1 (sound "bd sn")
```
Esto asegura que `sound` reciba su patrón antes de ser pasada a `d1`. El dolar es conveniente porque así no hay que preocuparse por cerrar el paréntesis, cosa que puede ser dificil si hay muchos paréntesis anidados uno dentro de otro.


```haskell
d1 $ sound "bd sn"
```
De cualquier manera, dejemos ir esta diversión sintáctica.

## Acomodar patrones en capas con `stack`

Se pueden ejecutar muchos patrones al mismo tiempo usando la función `stack` enviándole 
una lista de patrones envueltos en corchetes y separándolos con comas. Esto es muy similar a la sintaxis que vimos antes, pero que estaba fuera del mundo de las funciones.


```haskell
d1 $ stack [sound "bd sn:2" |+| vowel "[a e o]/2",
            sound "casio casio:1 casio:2*2"
           ]
```

## Pegar patrones usando `cat` y `slowcat`

Si reemplaza `stack` con `cat`, los patrones serán unidos uno después del otro en vez de uno sobre el otro:


```haskell
d1 $ cat [sound "bd sn:2" |+| vowel "[a o]/2",
          sound "casio casio:1 casio:2*2"
         ]
```

La función `cat` acomoda todos los patrones en el espacio de uno, pero `slowcat` mantendrá 
la velocidad de reproducción:

```haskell
d1 $ slowcat [sound "bd sn:2" |+| vowel "[a o]/2",
              sound "casio casio:1 casio:2*2"
             ]
```
## Desacelerar o acelerar un patrón con `slow`y `density`

Desacelerar un patrón de la forma simple hace que cambie sustancialmente su caracter en formas sorpresivas. Use `slow`para desacelerar un patrón.

```haskell
d1 $ slow 2 $ sound "bd ~ sn bd ~ [~ bd]/2 [sn [~ bd]/2] ~"
```
Y nuestro amigo `density` para acelerarlo de nuevo.


```haskell
d1 $ density 2 $ sound "bd ~ sn bd ~ [~ bd]/2 [sn [~ bd]/2] ~"
```
Juegue un poco con los números y descubra que `density 0.5` de hecho es igual a `slow 2`

## Reversa con `rev`

La función `rev` reversa cada ciclo en un patrón:

```haskell
d1 $ rev $ sound "bd ~ sn bd ~ [~ bd]/2 [sn [~ bd]/2] ~"
```
## `chop` 

La función `chop` pica cada sample en un número dado de bits:

```haskell
d1 $ chop 16 $ sound "bd ~ sn bd ~ [~ bd]/2 [sn [~ bd]/2] ~"
```
Esto hace que suene realmente granulado. Suena mas extraño si se reversa después de picarlo:

```haskell
d1 $ rev $ chop 16 $ sound "bd ~ sn bd ~ [~ bd]/2 [sn [~ bd]/2] ~"
```

Por el uso de `$` este código está trabajando de derecha a izquierda; primero hace la secuencia, luego pasa esta secuencia a `chop 16`, luego la pasa a `rev`, y finalmente al sintetizador Dirt usando `d1`. Si se cambia el orden entre `chop` y `rev` suena diferente:

```haskell
d1 $ chop 16 $ rev $ sound "bd ~ sn bd ~ [~ bd]/2 [sn [~ bd]/2] ~"
```

Esto ocurre porque estamos reversando el patrón primero, se pican los samples después, entonces los bits no son reversados. (Ojalá esto que dije tenga sentido).

`chop` funciona muy bien con samples largos:

```haskell
d1 $ rev $ slow 4 $ chop 16 $ sound "breaks125"
```
Esto se pondrá muy divertido con las meta-funciones.

# Meta-funciones

Hay muchas mas funciones que las que se muestran hasta acá, pero antes de verlas, saltemos un nivel para ojear algunas meta-funciones.

## `every` 

Por meta-funciones nos referimos a funciones que toman otras funciones como entrada. Por ejemplo,¿qué tal si no quiero reversar un patrón cada vez, si no cada cierta vez?

```haskell
d1 $ every 2 rev $ sound "bd can sn can:4"
```
En vez de aplicar `rev` directamente a `sound "bd can sn can:4"`, el código anterior envía 
`rev`a `every`, diciéndole que lo haga cada `2` repeticiones. Intente cambiar `2` por `4`para descubrir una sensación muy distinta.

Miremos un ejemplo visual:

<example>
density 8 $ every 4 rev "black darkblue blue lightblue white"
</example>

Esto funciona con otras funciones que reciben patrones. Así se hace un patrón el doble de denso cada cuatro repeticiones:


```haskell
d1 $ every 4 (density 2) $ sound "bd can sn can:4"
```
... y visualmente:

<example>
density 8 $ every 4 (density 3) "orange red pink purple"
</example>

Note que se debe encerrar `density 2` en paréntesis, para poder empaquetarla y pasarla a `every` como la función que se aplicará selectivamente a `"bd can sn can:4"`. Si esto no parece tener sentido, sientase libre de jugar un poco, y tranquilícese porque esta técnica involucra algo llamado "currying", asi que no puede ser tan malo.

Intentemos con ejemplos mas largos:

```haskell
d1 $ every 2 (density 1.5) $ every 3 (rev) $ slow 4 $ chop 16 $ sound "breaks125"
``` 
## `sometimes`

La función `sometimes` se parece un poco a `every` excepto que a veces aplica la función y a veces no, y lo hace de manera impredecible.

```haskell
d1 $ sometimes (density 2) $ sound "bd can*2 sn can:4"
```
Hay otras funciones similares a estas como `often`y almostAlways` que aplican la función mas a menudo, `rarely` y `almostNever`, que la aplican menos veces.

Siempre es posible amontonar funciones, por ejemplo:

```haskell
d1 $ sometimes (density 2) $ every 4 (rev) $ sound "bd can sn can:4"
```
En general, Tidal se pone mas interesante cuando toma partes simples y se combinan de esta manera.

## `jux`

La meta-función `jux` se aplica a un solo canal del estéreo. Por ejemplo, el siguiente patrón suena en reversa por uno de los altavoces y normal por el otro.

```haskell
d1 $ jux rev $ sound "bd sn*2 can [~ arpy]"
```
En este el patrón es ejecutado 25% mas rápido en uno de los altavoces:

```haskell
d1 $ jux (density 1.25) $ sound "arpy:2 arpy:4 arpy:1 [~ arpy]"
```

## `weave`

Weave es una función extraña, ya que toma diferentes parámetros de síntesis y los superpone desfasados uno de otro, al principio del patrón base. Esta bien, esto necesita un ejemplo:

```haskell
d1 $ weave 16 (pan sine1)
  [sound "bd sn", sound "arpy ~ arpy:3", sound "can ~ ~ can:4"]
```

En el código anterior los tres patrones tienen el parámetro `pan sine1`, pero estan espaciados alrededor del ciclo de `pan`, el cual esta estirado sobre un ciclo de `16` ciclos. Como resultado, los tres patrones se mueven uno alrededor de otro, persiguiéndose alrededor de los altavoces. Esto es especialmente agradable si es usando Dirt en modo multicanal con mas de dos altavoces.

Todo puede ser reacomodado a partir del patrón de `sound`; los patrones que van a ser aplicados a diferentes partes y como van a ser afectados:


```haskell
d1 $ weave 16 (sound "arpy arpy:7 arpy:3")
  [vowel "a e i", vowel "o i", vowel "a i e o", speed "2 4 ~ 2 1"]
```

# Funciones, parte 2

Ya que vimos como las meta-funciones pueden afectar las funciones, es momento de ver mas.

## Rotación con `~>` y `<~`

El operador `~>` `rota un patrón por un número dado de ciclos, por ejemplo el siguiente código rota el patrón hacia adelante en tiempo un cuarto de ciclo:

```haskell
d1 $ 0.25 ~> sound "arpy arpy:1 arpy:2 arpy:3"
```
Obviamente el operador `<~` hace lo mismo en la otra dirección:

```haskell
d1 $ 0.25 <~ sound "arpy arpy:1 arpy:2 arpy:3"
```
A menos que otro patrón suene al mismo tiempo, sólo se oirá la diferencia cuando cambie el número, ahí se percibe como un salto adelante o atrás. Es acá donde entran las meta-funciones:

```haskell
d1 $ every 4 (0.25 <~) $ sound "arpy arpy:1 arpy:2 arpy:3"
```
En este patrón, hay un salto cada cuarto ciclo. De nuevo, como la salida de todas estas funciones es un patrón, pueden ser usadas como entrada de otra función.


```haskell
d1 $ jux ((1/8) ~>) $ every 4 (0.25 <~) $ sound "arpy*3 arpy:1*2 arpy:4 [~ arpy:3]"
```

# Rotación compuesta con `iter`

Dado un número `n`, la función `iter`rota un patrón `1/n` pasos a la izquierda para cada ciclo. Un ejemplo hace maravillas:

```haskell
d1 $ iter 4 $ sound "arpy:1 arpy:2 arpy:3 arpy:4"
```
Acá su equivalente visual:

<example>
density 4 $ iter 4 $ "blue green purple orange"
</example>

Funciona muy bien con `jux`:

```haskell
d1 $ jux (iter 8) $ sound "arpy:1 arpy:2 arpy:3 arpy:4"
```
## Escalando patrones numéricos

La función `scale` es útil para tomar un patrón como `sine1` que va de `0` a `1`, y lo hace retornar un rango diferente, en el próximo ejemplo, desde `1` a `1.5`.


```haskell
d1 $ jux (iter 4) $ sound "arpy arpy:2*2"
  |+| speed (slow 4 $ scale 1 1.5 sine1)
```
Usar esta función es particularmente importante como parámetro para `shape` porque si usted envía valores demasiado altos (cercanos a 1) se torna muy fuerte, en el próximo ejemplo lo cortamos en 0.8.

```haskell
d1 $ jux (iter 4) $ sound "drum drum:1*2"
  |+| shape (slow 4 $ scale 0 0.8 sine1)
```

## Escojer los samples

Puede tener un patrón con los nombres de los samples, y otro con los números de los samples, luego combinarlos para escoger el que quiera.


```haskell
d1 $ sound (samples "drum arpy newnotes" "0 1 2")
```

Esto no es realmente emocionante hasta que empieze a manipular patrones antes de combinarlos y así agregar un poco mas de control:

```haskell
d1 $ jux (density 2) $ sound (samples "drum can can" (slow 2.5 "0 1 2 4 5 6"))
```

# Eso es todo por ahora

Hay mas, pero debe ser excavado desde [Tidal website](http://tidal.lurk.org/), los videos  (e.g. en
youtube y vimeo) y los patrones de Tidal que comparte la gente.
También  puede ingresar al foro de Tidal
[http://lurk.org/groups/tidal](http://lurk.org/groups/tidal) y unirse a la comunidad de discusión.¡Diviértete!








