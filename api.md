# High level api

gb-length
gb-slice
gb-nth

put-caret
update-caret

delete-selection!
delete-before-caret!
delete-word-forward!
delete-word-backward!
backspace!

insert-char!
insert-char-upper!

forward-word
backward-word
forward-char
backward-char

copy
cut!
paste!

undo!
redo!


# Undo / redoable

delete-selection!
delete-before-caret!
delete-word-forward!
delete-word-backward!
backspace!

insert-char!
insert-char-upper!

cut!
paste!





Here's a list of all functions in `src/new_gap_buffer.janet`.

### iterators

gb-iterate
index-char-backward
index-char-backward-start
index-char-start

### committing
fresh?
commit!

### string functions
gb-length
gb-slice
start-stop-text-gap-nth*
gb-nth

### bounds-check
bounds-check

### caret movement
put-caret
update-caret

### c->s
c->s

### searching
word-delimiter?
end-of-next-word
start-of-previous-word

### selection funcs
deselect
select-all
select-forward-word
delete-word-forward!
delete-word-backward!
select-backward-char

### gap movement
put-gap-pos!
update-gap-pos!
move-gap-to-caret!

### removal
delete-region*
delete-region!
delete-selection!
delete-before-caret!
delete-word-forward!
delete-word-backward!
backspace!
replace-content

### insertion
insert-char-at-caret
insert-string-at-caret
insert-char!
ascii-upper-chr*
insert-char-upper!

### navigation
forward-word
backward-word
forward-char
backward-char

### cut / copy / paste
get-selection
copy
cut!
paste!

### undo
undo!
redo!

### render
render
