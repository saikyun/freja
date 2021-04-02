
(def colors
  {:text [0.396 0.478 0.514]
   :border [0.396 0.478 0.514]
   :background (map |(/ $ 255) [230 227 213]) #[0.992 0.965 0.89]
   :textarea [0.992 0.965 0.89]
   :selected-text [0.992 0.965 0.89]
   :selected-text-background :blue
   :caret [0.396 0.478 0.514]

   :game-bg (map |(/ $ 255) [134 173 173])

   :special-symbol (map |(/ $ 255) [133 153 0])
   :string (map |(/ $ 255) [42 161 151])
   :keyword (map |(/ $ 255) [38 138 210])})

wat