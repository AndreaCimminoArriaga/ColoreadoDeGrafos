ColoreadoDeGrafos
=================

El lenguaje usado para los algoritmos es LISP.

El presente trabajo tiene como objeto implementar unos algoritmos para resolver el problema de coloreado de grafos.
Mi elección fue basar mi entrega en los algoritmos genéticos, porque me fascina su modularidad, gracias a las funciones que los componen. Precisamente esta característica les da mucho juego y potencia, pudiendo obtener muchos algoritmos distintos, modificando ligeramente alguno de esos módulos. Mi primera elección fue la del genético híbrido, descrito en el trabajo de Musa M. Hindi y Roman V. Yampolskiy, que está en este mismo repositorio. Mi segunda elección fue el modelo de islas, me interesó sobre manera debido a la simplicidad de la idea, y al mismo tiempo a la potencia que aporta.
En el algoritmo híbrido genético he realizado varias implementaciones, porque me pareció muy interesante ver cómo mejoraba su eficiencia, en base a como se iba adaptando al estilo de programación funcional (estructuras de datos, uso de funciones nativas, etc) de LISP. Haber hecho tres versiones distintas da una visión de una evolución real, que es el proceso que realicé a la hora de completar el trabajo. Podría haber incluido sólo la versión final, pero me pareció adapto e interesante ver cómo puede afectar el uso de unas estructuras de datos y el uso de funciones nativas a la eficiencia.
Por otro lado, en el modelo de islas podría haber implementado un algoritmo genético más simple, pero realmente como ya tenía bastante trabajo y funciones hechas de la primera parte, me pareció más interesante jugar con el algoritmo de Musa M. Hindi y Roman V. Yampolskiy. Eso me llevó a redisenñar, en parte, su algoritmo adaptándolo al modelo de islas, pero quedándome con las partes que me parecían más eficientes y mejores.
El resultado de todo esto configura el presente trabajo.
