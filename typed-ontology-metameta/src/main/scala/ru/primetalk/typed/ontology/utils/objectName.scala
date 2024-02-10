package ru.primetalk.typed.ontology.utils

def objectName(o: Any): String =
  val simpleName = o.getClass.getSimpleName
  val l          = simpleName.length
  if simpleName(l - 1) == '$' then simpleName.substring(0, l - 1)
  else simpleName
