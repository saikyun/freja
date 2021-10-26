# freja

Self-modifiable text editor implemented in Janet.

## social

[![image](https://user-images.githubusercontent.com/2477927/138828275-273ca9a8-b531-41ba-b387-0918c19b5489.png)  
Join us on Discord!](https://discord.gg/YYKr25uDhj)

## try it

### prerequisites

* Janet -- https://janet-lang.org/
  * needs to use this commit or later: `04ca945ecf0598e069caadb35a3c3089187a8186`
* on newer versions of janet, you need to install jpm separately:
  * https://github.com/janet-lang/jpm
* libglfw3-dev
  * (X)ubuntu: `sudo apt-get install libglfw3-dev`
* Raylib dependencies -- https://github.com/raysan5/raylib#installing-and-building-raylib-on-multiple-platforms

### installation

```
[sudo] jpm install https://github.com/Saikyun/freja
```

If you get an old version, try running `[sudo] jpm clear-cache` before re-running the above command.

### steps to run from source

```
git clone https://github.com/Saikyun/freja
cd freja
sudo jpm deps
jpm build
janet freja/main.janet
```

NOTE: When running freja from source, you must start it from the project directory.
If you start it from another directory, you will get errors like:
`could not open file fonts/MplusCodeLatin60-Medium.otf`

If you want to run freja anywhere, it's better to `jpm install` it, or `jpm build` it.

If you want to use PREFIX as to not litter system wide libs, check out [sogaiu's post about it](https://github.com/saikyun/freja/issues/30#issuecomment-907937626).

### Some examples

```
# | is the cursor
1 2| 3
# hit Ctrl+Enter
#=> 2

"a b c"|
# Ctrl+Enter
#=> "a b c"

"a b| c"
# Ctrl+Enter
#=> b is undefined

(+ 1 2 3)|
# Ctrl+Enter
#=> 6
```
This can be very useful when trying to run example code in files,  
or just play around with the code.

The main way to use this is to open a file, hit `Ctrl+L`  
which will make Freja look at the environment of that file.  
Successive calls to `Ctrl+Enter` and `Ctrl+L` will then act in that environment.


## Evaluation environment

Whenever you run hit `Ctrl+L` you run `freja/file_handling/save-and-dofile`.  
This saves the file, and then runs the file using a variant of janet's `dofile`.  
This leads to a new environment table being created (using `make-env`).  
This environment table is then used whenever you hit `Ctrl+Enter`,  
which calls `freja/input/eval-it`.  
`eval-it` will run the code to the left of the cursor, specifically,  
a symbol, keyword, string, number, struct, table, tuple (including a function call), or array.  

## Thanks

Thanks to sogaiu and rhine for initial testing. <3

## License

Copyright 2020 Jona Ekenberg

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
