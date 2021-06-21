# freja

Self-modifiable text editor implemented in Janet.

## try it

### prerequisites

Janet -- https://janet-lang.org/

### installation

```
[sudo] jpm install https://github.com/Saikyun/freja
```

If you get an old version, try running `[sudo] jpm clear-cache` before re-running the above command.

### steps to run from source

```
git clone https://github.com/Saikyun/freja
cd freja
mkdir janet_libs
JANET_PATH=./janet_libs jpm deps
JANET_PATH=./janet_libs jpm build
JANET_PATH=./janet_libs janet src/main.janet
```

### changing the theme

NOTE: Use `Ctrl` on Windows / Linux and `Cmd` on MacOS.

1. Start the editor
   1. Linux / MacOS: `mkdir ~/.freja && freja ~/.freja/init.janet`
1. Copy the following into the file
```
(import freja/theme :prefix "")

(merge-into colors
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
```
3. Press `Ctrl/Cmd+L`
4. Press a button to force the text area to rerender

The above is a port of the Monokai theme by Wimer Hazenberg.

## Evaluation environment

Whenever you run hit `Ctrl/Cmd+L` you run `freja/file_handling/save-and-dofile`.  
This saves the file, and then runs the file using janet's `dofile`.  
This leads to a new environment table being created (using `make-env`).  
This environment table is then used whenever you hit `Ctrl/Cmd+Enter`,  
which calls `freja/input/eval-it`.  
`eval-it` will run the code to the left of the cursor, specifically,  
a symbol, keyword, string, number, struct, table, tuple (including a function call), or array.  

Some examples:
```
# | is the cursor
1 2| 3
# eval-it
#=> 2

"a b c"|
# eval-it
#=> "a b c"

"a b| c"
# eval-it
#=> b is undefined

(+ 1 2 3)|
# eval-it
#=> 6
```
This can be very useful when trying to run example code in files,  
or just play around with the code.

The main way to use this is to open a file, hit `Ctrl/Cmd+L`  
which will make Freja look at the environment of that file.  
Successive calls to `Ctrl/Cmd+Enter` will then act in that environment.  
If you want to reset the environment (i.e. clear away `def` calls  
you made using `Ctrl/Cmd+Énter`), you can reload the file using  
`Ctrl/Cmd+L` again.

## Thanks

Thanks to sogaiu and rhine for initial testing. <3

## License

Copyright 2020 Jona Ekenberg

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
