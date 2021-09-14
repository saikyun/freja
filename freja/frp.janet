(setdyn
  :doc
  ````
## Overview

frp4 is used to:

* display things on screen
* react to input from keyboard and mouse
* prioritize which ui elements get inputs

frp4 works by going through a table of
"things that can happen"
and
"things that want to know what happened"
e.g.

```
@{mouse [flash-cursor-position]
```
This means
"whenever something mouse related happens,
tell flash-cursor-position what happened".

In this context, mouse is an "emitter"
as in "mouse emits things that happen".
flash-cursor-position is a "subscriber"
as in "flash-cursor-position subscribes to things that happens".

## Example usage

The easiest way to figure out what "things that can happen" actually are, is to call:
```
(use ./misc/frp4)
(subscribe! mouse pp)
```
If you are reading this in Freja, you can put the cursor after each ) and hit Ctrl+Enter. Then try clicking somewhere and look in your terminal.

This means "the subscriber pp will be run whenever mouse emits something".

In the above case, you would see these sorts of things in the terminal:
```
@[:press (234 472)]
@[:release (234 472)]
```

The above describe what the physical mouse did, like pressing a button, and at what pixel relative to the window the mouse did what it did.
So @[:press (234 472)] means "the mouse was pressed on x position 234 and y position 472". (0 0) would be the top left corner.

A "thing that can happen" is called "event".


## Available emitters

Freja's built in emitters are listed below.

### `mouse`

Emits events related to the physical mouse.
In the form of `@[kind-of-event mouse-position]` if not specified otherwise.

`kind-of-event` can be one of the following:
- :press
- :release
- :drag
- :double-click
- :triple-click
- :scroll
-- in the form of `@[:scroll scroll-amount mouse-position]`, where scroll amount is how many pixels of scrolling occured

`mouse-position` is a tuple of the x/y coordinates relative to the window's top left corner.

####

```
(use ./misc/frp4)
(subscribe! mouse pp)
```


### `keyboard`

Emits events related to the physical keyboard.
Always in the form of `@[kind-of-event key]`

`kind-of-event` can be one of the following:
- :key-down
-- is emitted when a key is pressed, or repeatedly when held for a certain period of time

`key` is a keyword corresponding to the physical button. This will not account for locale, e.g. if using dvorak and hitting physical n, you will get `:n`, not b. This is due to how raylib works internally.

#### Example

```
(use ./misc/frp4)
(subscribe! keyboard pp)
```



### `chars`

Emits events related to physical keyboard presses, respecting the locale of the user.
Always in the form of `@[:char char]`

`char` is a number corresponding to an ascii character. E.g. writing a emits `@[:char 97]`.

#### Example

```
(use ./misc/frp4)
(subscribe! chars pp)
```

### `callbacks`
A stack of key -> callbacks, which will always only call the last callback for each key.

### `frame-chan`
Emits an event each new frame.

### `rerender`
Emits events when rerendering is needed.

````)

(use freja-jaylib)

(import ./events :as e :fresh true)
(import ./state :as state)
(import ./keyboard :as kb :fresh true)
(import ./vector-math :as v :fresh true)
(import ./theme)
(import ./fonts)
(import ./input :as i)
(import ./collision :prefix "")
(import ./render_new_gap_buffer :prefix "")
(import ./new_gap_buffer :prefix "")

(setdyn :freja/ns "freja/frp")

(var mouse nil)
(var chars nil)
(var keyboard nil)
(var frame-chan nil)
(var rerender nil)
(var out nil)
(var screen-size @{})

(var delay-left @{})

(defn handle-keys
  [dt]
  (var k (get-char-pressed))

  (while (not= 0 k)
    (e/push! chars @[:char k])
    (set k (get-char-pressed)))

  (loop [[k dl] :pairs delay-left
         :let [left ((update delay-left k - dt) k)]]
    (when (<= left 0)
      (e/push! keyboard @[:key-repeat k])
      (put delay-left k i/repeat-delay)))

  (loop [k :in kb/possible-keys]
    (when (key-released? k)
      (put delay-left k nil)
      (e/push! keyboard @[:key-release k]))

    (when (key-pressed? k)
      (put delay-left k i/initial-delay)
      (e/push! keyboard @[:key-down k]))))

(varfn handle-scroll
  []
  (let [move (get-mouse-wheel-move)]
    (when (not= move 0)
      (e/push! mouse @[:scroll (* move 30) (get-mouse-position)]))))

# table of callbacks, eg @{@[:down [10 10]  [|(print "hello") |(print "other")]}
#                          ^ a mouse event  ^ queued callbacks
#                                           ^ is actually a ev/chan
#                                           ^ but using struct to visualise
(def callbacks @{:event/changed false})

(varfn handle-resize
  []
  (when (window-resized?)
    (-> screen-size
        (e/put! :screen/width (get-screen-width))
        (e/put! :screen/height (get-screen-height)))))

(defn push-callback!
  [ev cb]
  (e/update! callbacks ev (fn [chan]
                            (default chan (ev/chan 1))
                            (e/push! chan cb)
                            chan)))

(defn handle-callbacks
  [callbacks]
  (loop [[ev cbs] :pairs callbacks
         :when (not= ev :event/changed)]
    (e/pull-all cbs [apply]))

  (loop [k :in (keys callbacks)]
    (put callbacks k nil)))

(def mouse-data (i/new-mouse-data))

(varfn handle-mouse
  [mouse-data]
  (def pos (get-mouse-position))
  (def [x y] pos)

  (put mouse-data :just-double-clicked false)
  (put mouse-data :just-triple-clicked false)

  (when (mouse-button-released? 0)
    (put mouse-data :just-down nil)
    (put mouse-data :recently-double-clicked nil)
    (put mouse-data :recently-triple-clicked nil)
    (put mouse-data :up-pos [x y])

    (e/push! mouse @[:release (get-mouse-position)]))

  (when (mouse-button-pressed? 0)
    (when (and (mouse-data :down-time2)
               # max time to pass for triple click
               (> 0.4 (- (get-time) (mouse-data :down-time2)))
               # max distance to travel for triple click
               (> 200 (v/dist-sqr pos (mouse-data :down-pos))))
      (put mouse-data :just-triple-clicked true)
      (put mouse-data :recently-triple-clicked true))

    (when (and (mouse-data :down-time)
               # max time to pass for double click
               (> 0.25 (- (get-time) (mouse-data :down-time)))
               # max distance to travel for double click
               (> 100 (v/dist-sqr pos (mouse-data :down-pos))))
      (put mouse-data :just-double-clicked true)
      (put mouse-data :recently-double-clicked true)
      (put mouse-data :down-time2 (get-time))))

  (cond (mouse-data :just-triple-clicked)
    (e/push! mouse @[:triple-click (get-mouse-position)])

    (and (mouse-data :just-double-clicked)
         (not (key-down? :left-shift))
         (not (key-down? :right-shift)))
    (e/push! mouse @[:double-click (get-mouse-position)])

    (or (mouse-data :recently-double-clicked)
        (mouse-data :recently-triple-clicked))
    nil # don't start selecting until mouse is released again

    (mouse-button-down? 0)
    (do
      (put mouse-data :down-time (get-time))

      (if (= nil (mouse-data :just-down))
        (do (put mouse-data :just-down true)
          (put mouse-data :last-pos pos)
          (put mouse-data :down-pos pos)
          (e/push! mouse @[:press (get-mouse-position)]))
        (do (put mouse-data :just-down false)
          (unless (= pos (mouse-data :last-pos))
            (put mouse-data :last-pos pos)
            (e/push! mouse @[:drag (get-mouse-position)])))))

    # no mouse button down
    (not= pos (mouse-data :last-pos))
    (do (put mouse-data :last-pos pos)
      (e/push! mouse @[:mouse-move (get-mouse-position)]))))

(def deps @{})

(varfn render-deps
  [dt]
  (loop [d :in (deps :draws)]
    (d)))

(def finally
  @{frame-chan [render-deps]})

(varfn init-chans
  []
  (print "initing chans")
  (set mouse (ev/chan 100))

  (set chars (ev/chan 100))
  (set keyboard (ev/chan 100))
  (set frame-chan (ev/chan 1))
  (set rerender (ev/chan 1))
  (set out (ev/chan 100))
  (set state/eval-results (ev/chan 100))

  (def dependencies
    @{mouse @[]
      keyboard @[|(:on-event (state/focus :focus) $)]
      chars @[|(:on-event (state/focus :focus) $)]
      state/focus @[]
      callbacks @[handle-callbacks]
      out @[|(with-dyns [:out stdout]
               (print $))]})

  (def draws @[])

  (merge-into deps @{:deps dependencies
                     :draws draws
                     :finally finally}))

(varfn trigger
  [dt]
  (handle-keys dt)
  (handle-scroll)
  (handle-resize)

  (e/push! frame-chan @[:dt dt])

  (handle-mouse mouse-data)

  (comment
    (when (mouse-button-pressed? 0)
      # uses arrays in order to have reference identity rather than value identity
      # relevant for callback handling
      (e/push! mouse @[:press (get-mouse-position)])))

  (e/pull-deps (deps :deps) (deps :finally)))

(comment
  (e/push! mouse [:down [10 10]]))

(defn subscribe-first!
  "Take an event emitter (e.g. a ev/channel)
and a callback (e.g. single arity function).
Creates a regular subscription."
  [emitter cb]
  (unless (find |(= $ cb) (get-in deps [:deps emitter] []))
    (update-in deps [:deps emitter] (fn [$] @[cb ;(or $ [])]))))


(defn subscribe!
  "Take an event emitter (e.g. a ev/channel)
and a callback (e.g. single arity function).
Creates a regular subscription."
  [emitter cb]
  (unless (find |(= $ cb) (get-in deps [:deps emitter] []))
    (update-in deps [:deps emitter] |(array/push (or $ @[]) cb))
    :ok))


(defn subscribe-finally!
  "Take an event emitter (e.g. a ev/channel)
and a callback (e.g. single arity function).
Creates a finally subscription."
  [emitter cb]
  (unless (find |(= $ cb) (get-in deps [:finally emitter] []))
    (update-in deps [:finally emitter] |(array/push (or $ @[]) cb))
    :ok))


(defn unsubscribe-finally!
  "Take an event emitter (e.g. a ev/channel)
and a callback (e.g. single arity function).
Removes a finFally subscription."
  [emitter cb]
  (update-in deps [:finally emitter]
             (fn [subs] (filter |(not= $ cb) subs)))
  :ok)
