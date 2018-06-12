# dproc
Dproc programming language compiler

## Index

1. [About](#about)
2. [File structure](#file-structure)
3. [Examples](#examples)

### About

Dproc is a data processing oriented language. It's intended to process and auto document its operations. 

### Files structure

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


