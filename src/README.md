# HOHO Mini Programming Language 

**HOHO** is a small, Christmas-themed programming language written in **Java**.  
Despite its playful syntax, HOHO follows the same core principles used in real-world languages.

## Features
- `HOHO` → print
- `MERRY / JINGLE` → if / else
- `SLEIGH` → for loop
- Integer arithmetic
- String literals
- String + number concatenation
- Fully interpreted on the JVM
- Programs loaded from `.hoho` files

## Structure
```bash
merry-java/
│
├── src/
│   └── HOHO.java        
│
├── out/                 
│
├── test.hoho            
│
└── README.md
```


## How To Run Your Code

1. Clone merry-java
2. Write an .hoho code by using HOHO syntax.

3. Compile: 
```bash
rm -rf out
mkdir out
javac -d out src/HOHO.java
```
4. And then execute:

```bash
java -cp out src.HOHO test.hoho
```