(import freja/new_gap_buffer :as gb)
(import freja/render_new_gap_buffer :as render-gb)
(import ./code_api)
(import freja/state)
(import freja/file-handling :as fh)
(import freja/input)
(import freja/checkpoint)
(import ./echoer)
(import ./evaling)
(import ./open-file)
(import ./find-file)
(import ./file-explorer)

(import freja/event/subscribe :as s)

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
(def move-up! (comp reset-blink |(render-gb/move-up! $)))
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
                        #


                        :l echoer/toggle-console
                        :c echoer/clear-console}

               :alt @{:c show-checkpoints
                      :e file-explorer/toggle}

               :backspace delete-word-backward!
               :delete delete-word-forward!

               :left backward-word
               :right forward-word

               :p find-file/find-file-dialog

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

(defn open-file-dialog [props]
  (:open-file props))

(defn goto-line-dialog
  [props]
  (:goto-line props))

(defn save-file
  [props &opt note]
  (checkpoint/save-file-with-checkpoint props note))

(defn search-dialog
  [props]
  (:search props))

(defn replace-dialog
  [props]
  (:replace props))

(defn eval-it
  [props]
  (evaling/eval-it state/user-env
                   (evaling/gb-get-last-sexp props)))

(defn eval-expr-dialog
  [props]
  (:eval-expr props))

(defn close-buffer
  [_]
  (if (>= 1 (length (state/editor-state :stack)))
    (print "Can't close a lonely buffer.")
    (let [comp (last (state/editor-state :stack))
          state (in comp 1)]
      (defn cb
        []
        (state/remove-buffer-stack comp)
        (when-let [[_ top-state] (last (state/editor-state :stack))]
          (when (top-state :freja/focus)
            (:freja/focus top-state))))
      (when (state :freja/quit)
        (:freja/quit state cb)))))

(defn swap-top-two-buffers
  [_]
  (cond (and (get-in state/editor-state [:other 1 :freja/focus])
             (:freja/focus? (in (last (state/editor-state :stack)) 1)))
    (:freja/focus ((state/editor-state :other) 1))

    (>= 1 (length (state/editor-state :stack)))
    (if (get-in state/editor-state [:other 1 :freja/focus])
      (:freja/focus (in (last (state/editor-state :stack)) 1))
      (print "Can't swap, only one buffer open."))

    (let [s (state/editor-state :stack)]
      (state/push-buffer-stack (s (- (length s) 2)))
      (when-let [[_ top-state] (last (state/editor-state :stack))]
        (when (:freja/focus top-state)
          (:freja/focus top-state))))))

# if you want to add more binds, it's preferable to use `set-key`, see at the end of this file
(var gb-binds @{:control @{:shift @{:f format!
                                    :e eval-expr-dialog
                                    #
}

                           :w close-buffer
                           :tab swap-top-two-buffers

                           :f search-dialog
                           :r replace-dialog
                           :g goto-line-dialog
                           :o open-file-dialog
                           :l fh/save-and-dofile
                           :s save-file
                           :q quit
                           :enter eval-it}
                :enter (comp reset-blink |(gb/insert-char! $ (chr "\n")))})

# might want to solve this differently...
(set state/gb-binds gb-binds)

(table/setproto gb-binds global-keys)

(def file-open-binds
  @{:load-file
    (fn [props path]
      (open-file/open-file ;(fh/string->path-line-column path)))
    # checkpoint/load-file-with-checkpoints
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

(def replace-binds
  @{:escape |(:escape $)
    :enter |(:replace $)
    :tab |(:next-field $)
    :control @{:f |(:replace $)
               :b |(:replace-backwards $)}})

(table/setproto search-binds global-keys)
(table/setproto replace-binds global-keys)

(def global-set-key (partial input/set-key global-keys))

# re-exporting

# sets keys without being dependend on having the modifiers in the right order
# if you were to do `put-in gb-binds` you'd need to be aware of the order
# modifiers are sorted, i.e. [:control :shift] is not the same as [:shift :control]
(def set-key input/set-key)

(comment
  # example usage
  (set-key gb-binds [:control :shift :f] format!))
