# text-experiment

Textarea for Janet.

## try it

### prerequisites

Janet -- https://janet-lang.org/

### steps

```
git clone https://github.com/Saikyun/freja
cd freja
mkdir janet_libs
JANET_PATH=./janet_libs jpm deps
JANET_PATH=./janet_libs jpm build
JANET_PATH=./janet_libs janet src/main.janet
```

### changing the theme

NOTE: `Meta` means `Ctrl` on Windows / Linux and `Cmd` on MacOS.

1. Start the editor
2. Press `Meta+O`, write `init.janet`, hit `Enter`
3. Copy the following into the file
```
(import ./src/theme :prefix "")

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
4. Press `Meta+L`

The above is a port of the Monokai theme by Wimer Hazenberg.

## Thanks

Thanks to sogaiu and rhine for initial testing. <3

## License (applies to all but the font which is from google fonts)

Copyright 2020 Jona Ekenberg

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
