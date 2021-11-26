(defmacro shim
  [& syms]
  ~(upscope
     ,;(seq [d :in syms]
         (match d
           [sym ret]
           ~(defn ,sym
              [& _]
              #(print ,(string sym))
              ,ret)

           sym
           ~(defn ,sym
              [& _]
              #(print ,(string sym))
)))))

(defn measure-text-ex
  [_ text _ _]
  [(length text) 1])

(shim init-window
      set-clipboard-text
      get-clipboard-text #r
      draw-text-ex
      load-font-ex #r
      load-font-from-memory #r
      unload-font
      [get-screen-height 200] #r
      [get-screen-width 428] #r
      draw-rectangle-rec
      begin-texture-mode
      rl-push-matrix
      rl-load-identity
      clear-background
      rl-pop-matrix
      end-texture-mode
      unload-render-texture
      load-render-texture #r
      rl-translatef
      draw-texture-pro
      get-render-texture
      draw-line-ex
      [get-char-pressed 0] #r
      key-released? #r
      key-pressed? #r
      [get-mouse-wheel-move 0] #r
      [get-mouse-position [0 0]] #r
      window-resized? #r
      mouse-button-released? #r
      mouse-button-pressed? #r
      get-time #r
      key-down? #r
      mouse-button-down? #r
      draw-rectangle
      show-cursor
      [get-frame-time 16.6] #r
      begin-drawing
      end-drawing
      window-should-close #r
      close-window
      set-config-flags
      set-exit-key
      [get-window-scale-dpi [1 1]] #r
      set-target-fps
      #
)
