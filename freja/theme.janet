(defn rgba
  [r g b & [a]]
  [(/ r 255)
   (/ g 255)
   (/ b 255)
   (or a 1)])

(def colors
  @{:text (rgba 71 93 101)
   :border [0.396 0.478 0.514]
   :background (rgba 253 246 227)
   :textarea [0.992 0.965 0.89]
   :selected-text [0.992 0.965 0.89]
   :selected-text-background :blue
   :caret [0.396 0.478 0.514]

   :game-bg (rgba 134 173 173)

   :call (rgba 38 139 210)
   :special-symbol (rgba 133 153 0)
   :string (rgba 42 161 151)
   :keyword (rgba 181 137 0)})