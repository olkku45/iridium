# Demo for the syntax of iridium

Let's get some general things out of the way first. In iridium, we have a main function at the start of every program, similarly to language like C.
You have to end every line of code with a semicolon, like in C, and unlike with Python, for example, where you don't have to do so. 
Things like functions and loops go inside of curly brackets, instead of using a colon, like in Python. If you want to declare a single character,
you do that by declaring a string (str). We don't have pointers in iridium or any memory management. Iridium is a dynamically typed language as well.
Indentation does matter in iridium, like in Python, but not as strictly. For example, you need to indent everything inside curly brackets by one tab.
I will elaborate on this further on in the document. In iridium, you use the same comparison operators as in Python.
 And also, I'll update this document as I develop the language further. I'll have a list of keywords that cannot be used as names for variables etc. at the bottom.
 

## Comments
```
// this is a comment in iridium  
// you don't need a semicolon at the end of a comment
```
## Declaring variables
```
int a = 3;
str b = "My name goes here";
bool c = True;
float variable_name = 3.14;
arr e = [0,1,1,2,3,5];
```
## Conditionals
```
if i_am_human = True {
  // something here
}	

if i_am_human {
  // this is the same thing as above
}

if i_am_human = False {
  // something
}

elif i_am_a_dog {
  // something
}

else {
  // something
}
```
## Functions
```
func example_function {
  int number = 42;
	
  if number == 42 {
    print("This is everything!");
  }
	
  else {
    print("This must not be true!");
  }
}
```

## q
