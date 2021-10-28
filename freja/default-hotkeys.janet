(import freja/new_gap_buffer :as gb)
(import freja/render_new_gap_buffer :as render-gb)
(import freja/code_api)
(import freja/state)
(import freja/file-handling :as fh)
(import freja/input)
(import freja/checkpoint)
(import ./evaling)

(varfn reset-blink
  [props]
  (put props :blink 0))

(def undo!2 (comp reset-blink gb/undo!))
(def paste! (comp reset-blink gb/paste!))
(def cut! (comp reset-blink gb/cut!))
(def redo! (comp reset-blink gb/redo!))
(def format! (comp reset-blink code_api/format-code))
(def select-backward-word (comp reset-blink gb/select-backward-word))
(def select-forward-word (comp reset-blink gb/select-forward-word))
(def delete-word-backward! (comp reset-blink gb/delete-word-backward!))
(def delete-word-forward! (comp reset-blink gb/delete-word-forward!))
(def backward-word (comp reset-blink gb/backward-word))
(def forward-word (comp reset-blink gb/forward-word))
(def select-backward-char (comp reset-blink gb/select-backward-char))
(def select-forward-char (comp reset-blink gb/select-forward-char))
(def backward-char (comp reset-blink gb/backward-char))
(def forward-char (comp reset-blink gb/forward-char))
(def move-to-start-of-line (comp reset-blink render-gb/move-to-start-of-line))
(def move-to-end-of-line (comp reset-blink render-gb/move-to-end-of-line))
(def delete-after-caret! (comp reset-blink gb/delete-after-caret!))
(def delete-before-caret! (comp reset-blink gb/delete-before-caret!))
(def move-up! (comp reset-blink render-gb/move-up!))
(def move-down! (comp reset-blink render-gb/move-down!))
(def page-up! render-gb/page-up!)
(def page-down! render-gb/page-down!)
(def beginning-of-buffer gb/beginning-of-buffer)
(def end-of-buffer gb/end-of-buffer)

(defn show-checkpoints
  [_]
  (checkpoint/show-checkpoints))

(var global-keys
  @{:alt @{:shift @{:left select-backward-word
                    :right select-forward-word
                    #
}

           :backspace delete-word-backward!
           :delete delete-word-forward!

           :left backward-word
           :right forward-word
           #
}

    :control @{:shift @{:left select-backward-word
                        :right select-forward-word
                        :c show-checkpoints
                        #
}

               :backspace delete-word-backward!
               :delete delete-word-forward!

               :left backward-word
               :right forward-word

               :a gb/select-all
               :x cut!
               :c gb/copy
               :v paste!
               :z undo!2
               :y redo!
               :home beginning-of-buffer
               :end end-of-buffer}

    :shift @{:home render-gb/select-to-start-of-line
             :end render-gb/select-to-end-of-line
             :left select-backward-char
             :right select-forward-char
             :up render-gb/select-move-up!
             :down render-gb/select-move-down!

             #
}

    :left backward-char
    :right forward-char
    :up move-up!
    :down move-down!

    :page-up page-up!
    :page-down page-down!

    :home move-to-start-of-line
    :end move-to-end-of-line

    :delete delete-after-caret!
    :backspace delete-before-caret!

    #
})

(defn quit
  [props]
  (set state/quit true))

(defn open-file
  [props]
  (:open-file props))

(defn goto-line
  [props]
  (:goto-line props))

(defn save-file
  [props &opt note]
  (checkpoint/save-file-with-checkpoint props note))

(defn search
  [props]
  (:search props))

(defn eval-it
  [props]
  (evaling/eval-it state/user-env
                   (evaling/gb-get-last-sexp props)))

(defn eval-expr
  [props]
  (:eval-expr props))

(var gb-binds @{:control @{:shift @{:f format!
                                    :e eval-expr
                                    #
}

                           :f search
                           :g goto-line
                           :o open-file
                           :l fh/save-and-dofile
                           :s save-file
                           :q quit
                           :enter eval-it}
                :enter (comp reset-blink |(gb/insert-char! $ (chr "\n")))})

(table/setproto gb-binds global-keys)

(def file-open-binds @{:load-file checkpoint/load-file-with-checkpoints
                       :escape (fn [props] (:escape props))
                       :enter (fn [props] (:enter props))})

(table/setproto file-open-binds global-keys)

(def eval-binds
  @{:escape |(:escape $)
    :enter |(:eval-expr $)})

(table/setproto eval-binds global-keys)

(def search-binds
  @{:escape |(:escape $)
    :enter |(:search $)
    :control @{:f |(:search $)
               :b |(:search-backwards $)}})

(table/setproto search-binds global-keys)

(def global-set-key (partial input/set-key global-keys))
