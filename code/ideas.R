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

# como utilizar data.frames y which

df <- data.frame (a = 1:3, b = 71:73)

df[df == ""] <- "empty"

