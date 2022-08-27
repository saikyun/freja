(use freja-jaylib)
(import spork/path)
(import ./fonts :as f)

# TODO: probably should be defonce
(def assets @{})

# TODO: probably should be defonce
(def fonts @{})

(comment
  fonts
  #=>
  @{"Texturina" @{20 ...
                  22 ...
                  :path "..."
                  :data "..."}}
  #
)

(defn asset
  ```
Gets asset if it's loaded,
or loads the asset, puts it in assets, then returns it.
```
  [path]
  (if-let [a (assets path)]
    a
    (let [data (slurp path)]
      (put assets path data)
      data)))

(defn register-font
  ``
Puts a font in the fonts assets,
this makes it available for usage with a simple font name,
rather than having to load the font manually.
``
  [name &keys {:path path
               :data data
               :ext ext

               :style style
               :default-style default-style}]

  (assert (or path (and data ext)) "Need :path or :data and :ext")

  (default style :unspecified)
  (default default-style (get-in fonts
                                 [name :default-style]
                                 style))

  (def data (if data data (asset path)))

  (update fonts name
          |(-> (or $ @{})
               (update style |(-> (or $ @{})
                                  (put :ext (or ext (path/ext path)))
                                  (put :path path)
                                  (put :data data)))
               (put :default-style default-style)
               (update :styles |(put (or $ @{}) style style)))))

(defn load-font-with-size
  ``
Loads a font with a registered name and a size.
Puts the loaded font into the `assets/fonts` table.
Returns the loaded font.

The font must already be registered using `register-font`.
``
  [name size &keys {:style style}]
  (def font-info (fonts name))

  (assert font-info (string "font with name " name " is not registered, use `register-font` first"))

  (default style (font-info :default-style))

  (assert (font-info style)
          (string "font with name " name
                  " does not have style " style
                  " registered, use `register-font` first.\n"
                  "the currently available styles are: "
                  (string/format "%.40M" (keys (font-info :styles)))
                  "\nthe default style is: "
                  (font-info :default-style)))

  # if the fonts exists, unload it
  (when-let [existing-font (get-in font-info [style size])]
    (unload-font existing-font))

  (def lf (f/default-load-font-from-memory
            (get-in font-info [style :ext])
            (get-in font-info [style :data])
            size))

  (put-in fonts [name style size] lf)
  (put-in fonts [name style :glyph-sizes size]
          (f/default-glyphs->size-struct lf size))
  (update-in fonts [name style :sizes] |(array/push (or $ @[]) size))

  lf)

(defn font-info
  [name]
  ``
Shows information about a font, such as which styles are loaded and at which sizes.
``
  (var info @{})
  (def styles (get-in fonts [name :styles]))
  (loop [[k v] :in (pairs (fonts name))]

    (if (styles k)
      (->> (from-pairs (seq [[k v] :in (pairs v)
                             :when (not= k :data)] # we hide the data since it's so much bloat
                         [k v]))
           (put info k))
      (put info k v)))
  info)

(import spork/test)

(defn font
  ``
Given a font `name` and `size`, returns the loaded font to be used with jaylib font rendering.

To get information about a font (e.g. loaded styles), use `font-info`.

## Optional keys

`:style` -- choose style such as `:regular` or `:italic`  
Leaving `:style` empty will use the `:default-style` for the font.
``
  [name size &keys {:style style}]
  (def font (fonts name))

  (assert font
          (string "font with name " name " is not registered, use `register-font` first"))

  (default style (font :default-style))

  (or (get-in font [style size])
      (load-font-with-size name size :style style)))

(defn glyph-sizes
  ``
Given a font `name` and `size`, returns a table with each glyph and their size for the given font.

To get information about a font (e.g. loaded styles), use `font-info`.

## Optional keys

`:style` -- choose style such as `:regular` or `:italic`  
Leaving `:style` empty will use the `:default-style` for the font.
``
  [name size &keys {:style style}]
  (def font (fonts name))

  (assert font
          (string "font with name " name " is not registered, use `register-font` first"))

  (default style (font :default-style))

  (or (get-in font [style :glyph-sizes size])
      (do (load-font-with-size name size :style style)
        (get-in font [style :glyph-sizes size]))))

(comment
  (do
    (register-font "Poppins"
                   :style :regular
                   :path "../freja/fonts/Poppins-Regular.otf")
    (font "Poppins" 22 :style :regular))

  (get-in @{:regular @{22 "a"}} [:regular 22] "b")

  (font-info "Poppins")
  #
)


(defn register-default-fonts
  []
  (register-font "Poppins"
                 :style :regular
                 :ext ".otf"
                 :data f/poppins)

  (register-font "MplusCode"
                 :style :regular
                 :ext ".otf"
                 :data f/mplus)

  (register-font "EBGaramond"
                 :style :regular
                 :ext ".otf"
                 :data f/ebgaramond))

