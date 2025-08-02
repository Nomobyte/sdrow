# sdrow
**sdrow is a simple and easy-to-use cli learning program inspired by Anki and Wordle.**

## Features
- Simple configuration
- No special file format
- Can imitate Wordle or Anki
- Full CLI
- Code size less than 200 lines
## Requirments
- [Guile](https://www.gnu.org/software/guile/)
- Make
## Installation
```sh
make
sudo make install
```
To uninstall, do the following:
```sh
sudo make uninstall
```
## Usage
### Exit
Ctrl+C for exit
### Examples
#### Basic
```sh
sdrow -ri examples/basic4.txt 
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
```sh
sdrow -rm alist examples/hiragana-vowels.txt 
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
```sh
sdrow -rsam ralist examples/greetings.txt
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
sdrow -rm alist examples/fluids.txt 
What liquid does the ocean consist of?
@> water

---Water---

What liquid does mammals produce?
@> milk

---Milk---

```