(defn rgba
  [r g b & [a]]
  [(/ r 255)
   (/ g 255)
   (/ b 255)
   (or a 1)])

(comment
  # lighter mode
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
  #
)

(def colors
  @{:text (rgba 248 248 243)
    :border [0.396 0.478 0.513]
    :background (rgba 39 40 33)
    :textarea [0.992 0.965 0.88]
    :selected-text [0.992 0.965 0.88]
    :selected-text-background :blue
    :caret [0.396 0.478 0.513]

    :game-bg (rgba 134 173 172)

    :call (rgba 166 226 45)
    :special-symbol (rgba 102 217 238)
    :string (rgba 230 219 115)
    :keyword (rgba 174 128 255)})

(def font-size 22)

(def comp-cols {:background 0x882491ff
                :text/color 0xffffffee
                :caret/color 0xffffff80})
