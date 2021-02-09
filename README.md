# Lite  v.1.0.5
### An interpreted programming language made in python3


Lite is a programming language made in `python3`, by Rayyan Cyclegar. Lite was made using the `lark` parsing library, and is still in development stages. The following is an example of a calculator made using Lite.
```
print("Welcome To My Calculator!");

x = input("Enter an operation: ");

if x == "plus" {
    num1 = input("Num1: ");
    num2 = input("Num2: ");
    print(num1 + num2);
}
if x == "minus" {
    num1 = input("Num1: ");
    num2 = input("Num2: ");
    print(num1 - num2);
}
if x == "mul" {
    num1 = input("Num1: ");
    num2 = input("Num2: ");
    print(num1 * num2);
}
if x == "div" {
    num1 = input("Num1: ");
    num2 = input("Num2: ");
    print(num1 / num2);
}
```
Lite uses a basic Tree-Walk interpreter, which partly results in its slow interpreting speeds, this is also partly because it was coded in `python3`, which isn't the fastest language in general. I've started a more complex, faster rewrite version of this programming language, which compiles itself to bytecode and is interpreted from there. This should increase the performance and speed of the language, though it may not be extremely different. At the moment, the rewrite is in python, but there are plans to use C++ in the future. You can check out the rewrite repository [Here](https://github.com/AbooMinister25/Lite-Bytecode). This repository is mostly experimental, and I don't reccomend installing it for actual usage, since it barely has any feature or functions implemented yet. Install this version for a working, operational version of the language. If you want to see the code for the language, its under the `lite` folder in this repository.

#### Lite currently has support for the following
* print statements
* input statements
* if statements
* else statements
* functions
* variables
* addition
* subtraction
* multiplication
* division
* loops
* booleans
* builtin modules
* dicts
* other small built in functions, full reference can be found at the documentation

Functions are very limited at the moment, Lite does not support arguments and doesn't have return statements yet, all of this is planning to be added in a later release.
You can find more examples and a complete and more detailed reference at the documentation. (Coming Soon), or at the REFERENCE.md file.

# Installation
The installation for Lite is pretty straightforward, just follow the steps below.
* The following is the current stable release of lite, download it. [Lite 1.0.0](https://github.com/AbooMinister25/Lite)
* Once lite has been installed, you can run it by calling its full path, type in the following command (formatted) to run Lite`C:\Users\{user}\{Where the folder is located}\Lite-Master\Lite-Master\lite run` . Warning, this script may not work, if it gives an error, which it might, instead run the following: `C:\Users\{user}\{Where the folder is located}\Lite-Master\Lite-Master\lite\lite.py run`

And there you go! Lite is up and running, if something went wrong, or if your having any problems, please contact me, my contact info will be at the bottom.


# Usage
The usage is pretty straightforward, as I stated above, run the following command in a terminal to open the Lite shell: `C:\Users\{user}\{Where the folder is located}\Lite-Master\Lite-Master\lite run`, format your data into that, if that doesn't work, use the following command: `C:\Users\{user}\{Where the folder is located}\Lite-Master\Lite-Master\lite\lite.py run`. Lites file extension is `.lite`. If you want to run a file, use one of the following commands in the terminal, `C:\Users\{user}\{Where the folder is located}\Lite-Master\Lite-Master\lite run -f filename.lite` or, again, if that doesn't work, then use `C:\Users\{user}\{Where the folder is located}\Lite-Master\Lite-Master\lite\lite.py run -f filename.py`. The full documentation, as well as an API reference is coming soon, as well as a tutorial for beginners, keep in mind that Lite is still under heavy development, so be sure to report any bugs you happen to come across.

# Contact
If your experiencing any bugs or problems, or have any questions, feel free to open an issue or email me at my email, aboominister@gmail.com
