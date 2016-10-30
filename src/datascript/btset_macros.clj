(ns datascript.btset-macros)

(defmacro half [x]
  `(unsigned-bit-shift-right ~x 1))

(defmacro not== [x y]
  `(not (== ~x ~y)))
