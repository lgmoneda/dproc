# dproc
Dproc programming language compiler

## Index

1. [About](#about)
2. [File structure](#file-structure)
3. [User manual](#user-manual)
    1. [Install](#install)
    2. [Variable definition](#var-def)
    3. [Built-in functions](#builtin-functions)
    4. [Running a dproc file](#running-a-dproc-file)
    5. [Analysing results](#results)
4. [Examples](#examples)
5. [References](#references)

### About

Dproc is a data processing oriented language. It's intended to process and auto document its operations. 

### Files structure

``` 
src/dproc.lex: token definition
src/dproc.grm: grammar 
src/interpreter.sml: execution
```

### User Manual

#### Install

In order to run it, you need to install SML:

Linux:
``` 
apt-get install smlnj
```

MacOS:

``` 
brew update
brew install smlnj
```

#### Bultin Functions

- Arithmetic: sum (+), subtraction (-), product (*) and division (/);
- Logical: greater (>), greater or equal (>=), less (<), less or equal (<=), equal (==) and different (!-);
- Boolean: and (and), or (or).

#### Running a dproc file

You can run dproc files as follows:

``` 1c-enterprise
./dproc my_program.dproc
```

It's going to be executed and it will generate a log file called `my_program.log`.

### Examples

``` 
table tab;
column comp;
column monthly_income;
column monthly_income_percapita;
float average_income;

tab = load("data.csv", name="Demographic data");

average_income = avg(tab["Income"]);
comp = logic_comp(tab["renda"], >, average_income, 1, 0);

monthly_income = div(tab["Income"], 12);

insert(tab, monthly_income, "Monthly Income");
insert(tab, comp, "Comp to avg income");

monthly_income_percapita = div(tab["Monthly Income"], tab["Family Size"]);

insert(tab, monthly_income_percapita, "Monthly Income percapita");

sav(tab, "data_processed.csv", "data_processed.log")
```

The log would be:

``` 
---> "Monthly Income":
     1. Division of the column ["Income"] from ["Demographic data"] by [12];
	 
---> "Comp to avg income": 
     1. Logic operation [>] between column ["Income"] from ["Demographic data"] and [average of the column ["Income"] from ["Demographic Data"]], [1] if true, [0] otherwise;

---> "Monthy Income percapita":
     1. Division of the column ["Monthly Income"] from ["Demographic Data"] by the column ["Family Size"] from ["Demographic data"];
```


### References

[SML Language Processing Tools User Guide])(https://www.smlnj.org/doc/ml-lpt/manual.pdf)
