Looking for a type-safe and principled way to enable seamless transition between tagless final and free inital encodings

Subcategory (incorrectly named) allows you to defer your interpretation of a free program through a partially derived
natural transformation by linking it (excluding its target type) to a given tagless final encoding

With an instance of of Subcategory[GADT, TaglessFinal], a natural transformation from GADT to any unary type constructor G
can be provided for which there is an instance of TaglessFinal targeting G

