(in-package :toolbox)
(annot:enable-annot-syntax)


@export
(deftype byte-specifer-list ()
  `(and list (satisfies ordered-with-car)))
