# sdrow
**sdrow is a simple and easy-to-use cli learning program inspired by Anki and Wordle.**

## Features
- Simple configuration
- No special file format required. Just text files
- Language-agnostic*
- Can imitate Wordle or Anki
- Full CLI
- Code size less than 200 lines
- Default memory footprint less than 10 Mib
- There is no hard-coded limit on file size**
- The file is completely loaded into RAM, so the program does not access your disk while it is running.

*More testing is needed for RTL languages and languages with complex characters. But if your terminal emulator supports these languages, the program will most likely work correctly.

**Most likely your RAM will run out before the program starts to slow down.
## Requirments
- [Chicken Scheme](https://www.call-cc.org/)
- [GNU Make](https://www.gnu.org/software/make/)
- `getopt-long` Chicken egg
## Installation
To install `getopt-long`:
```sh
chicken-install getopt-long
```
This will download anything needed to compile and install the library. If your extension repository is placed at a location for which you don't have write permissions, then run `chicken-install` with the `-sudo` option or run it as root (not recommended). For a more detailed explanation, see the [official website](https://eggs.call-cc.org/5/).

To install the program:
```sh
make
sudo make install
```
To uninstall the program:
```sh
sudo make uninstall
```
## Usage
For more information, run `sdrow --help`.
### Exit
Ctrl+C for exit.
### Examples
#### Basic
```sh
sdrow -i examples/basic4.txt 
?> ____
@> some
?> ____
@> text
?> ___t
@> list

***list***

?> ____
@> some
?> _o__
@> word

***word***

?> ____
@> 
```
#### Hiragana vowels
CJK characters should work properly.
```sh
sdrow -m alist examples/hiragana-vowels.txt 
あ
@> o
@> a

---a---

う
@> i
@> e
@> u

---u---

お
@> 
```
#### Greetings
With case sensitivity enabled and hidden answer.
```sh
sdrow -sam ralist examples/greetings.txt
नमस्ते
@> hindi
@> Hindi

Hola
@> Spanish

Bonjour
@> French
```
#### Quiz
```sh
sdrow -m alist examples/fluids.txt 
What liquid does the ocean consist of?
@> water

---Water---

What liquid does mammals produce?
@> milk

---Milk---

```