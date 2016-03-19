# Parece que la proporcion de personas felices es un poco mas alta que la de personas que no lo son.
# 43 % infelices y 56 % felices.
# El porcentaje de gente feliz en los que responden a todas las preguntas es bajo. 
# Solo el 16 % de las personas responden a todas la preguntas.
# nos podría interesar la distribucion de felices segun las preguntas que responden.
# parece que la proporción de felices parece igual a la distribucion de los que responden a todas las 
# preguntas.
# Todos los NA que se encuentran en estos valores se encuentran en el año de nacimiento
# utilizando todas las variables podemos comprobar que en el training set que nuestra acc es del 0.6634705
# utilizando el modelo de linea base, nuestra accuracy en este modelo obtenemos 0.5638726
# luego aunque mejoramos en nuestro modelo sobre la linea base, no tenemos unos valores muy buenos de acc.
# teniendo en cuenta que estos valores corresponden al trainset, parece que no tendríamos un overfitting 
# si no un underfitting.
# si realizamos un cross validation para seleccionar el parametro de complejidad, vemos que es un parametro
# muy pequeño, con lo cual no es necesario simplificar el modelo. Lo cual nos puede hacer pensar en un problema
# de underfitting. Se trata de un modelo muy sencillo y si siquiera tiene la complejidad suficiente para 
# aplicarse al grupo de training set.

# en el grupo de test obtenemos una acc de 0.6471861

# Hemos añadido una variable de clustering al ajuste y eso no parece ser muy significativo
# a la hora de ajustar el sistema. Podriamos intentar hacer solo un clustering de los datos
# que no se responden y a partir de este punto ver que mas se podría hacer.

# Lo siguiente que realizamos es un clustering de los clientes por sus datos personales.
# para cada grupo que hemos creado de cluster, realizamos un modelo. De esta forma estamos
# añadiendo complejidad al sistema y somos mas propensos a un overfitting. Nuestro parametro
# de complejidad sería entonces el numero de cluster que realizamos.

# lo que pasa que cuando utilizamos un ramdom forest, obtenemos los mismos resultados. Luego
# lo que sucede es que tenemos un caso de overfitting salvaje.

# Quizas podriamos encontrar un grupo donde podamos encontrar una alta accuracy y a partir
# de ese punto clasificar a los demas. Pero esto parece muy propenso al over fitting

#  Cosas a probar. Boruta para la seleccion de features. 
#  como podríamos obtener otros elementos importantes. 
#  realizar un clustering pero normalizando los datos.
#  cross validation para la seleccion de los parametros que tenemos.

# 24-04-2014. Podemos guardar los modelos para utilizarlos luego. EL AUC de un random forest en el train es,
# utilizando todos las variables de un 0.72. Tambien en el conjunto del test elaborado. Luego, quizas, realizando
# una cross validation para encontrar un parametro mas adecuado, podriamos tener un  valor que no sea del todo malo

# model/ramdom_forest_with_cluster.RData. Es un modelo que incluye todos las preguntas, un numero asignando el cluster
# income para intentar predecir Happy. No incluye el resto de preguntas demograficas.

# ramdom_forest_with_cluster_2.RData incluye todos los datos demograficos y la clasificacion de los clusters, como 
# un numero del 1 al 7.

# Se podría comprobar que pasa si creamos un cluster con todos los elementos.


# 28-04-2014. Probamos a ver si podemos utilizar la biblioteca caret para validar la parte de los ramdonForest 
# y de esta manera crear un modelo mas ajustado. De momento las pruebas que estamos realizando tardan y no llegan a
# a dar ningun resultado.

# 30-04-2014. Probamos crear las learning curves para ver si tenemos un caso de overfitting o un caso de 
# de underfitting. Cuando solo incluimos una features los errores del grupo de validation y del grupo de 
# training son altos y acaban pareciendose, sin existir un gap entre ellos. 
