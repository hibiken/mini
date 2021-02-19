# mini

Mini is a small text editor, inspred by [antirez's kilo](http://antirez.com/news/108) editor.  
It aims to [_Keep it simple, stupid_](https://en.wikipedia.org/wiki/KISS_principle).

[![asciicast](https://asciinema.org/a/392547.svg)](https://asciinema.org/a/392547)

## Features

Mini has a small set of features just enough to be a usable code editor.

- Syntax Highligting
- Searching

## Installation

    $ go get github.com/hibiken/mini

## Usage

    $ mini <filename>

## Key bindings

    Ctrl-Q: quit
    Ctrl-S: save
    Ctrl-F: find

## Limitations

Syntax highlight is enabled for C, C++, and Go, but it can be extended for other languages.
Currently mini editor only supports Unix-like OS (e.g. Linux, MacOS).

## License

Mini editor is released under MIT license. See [LICENSE](https://github.com/hibiken/mini/blob/master/LICENSE).
