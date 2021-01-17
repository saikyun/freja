### example init.janet

# init.janet is run after raylib is inited, and before the loop starts
# put this in the same directory as where you start Fri from
# e.g. if running `janet src/main.janet`
# then init.janet goes into ./



## keybindigs

# (doc add-bind)
# kw->f is a map, check possible values by e.g. (keys kw->f)

(add-bind :v :paste)
