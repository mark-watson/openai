# Common Lisp library to access OpenAI GPT3 APIs

From my book URI: https://leanpub.com/lovinglisp

There is a **Makefile** in the repo https://github.com/mark-watson/loving-common-lisp that can be copied
to your **~/quicklisp/local-projects** directory. Then in **~/quicklisp/local-projects** run:

    make fetch

to get all of the library examples from my book.


## setting your OpenAI API key
 
 Define the  "OPENAI_KEY" environment variable with the value of your OpenAI API key
 
## Example:

```lisp
cl-user> (ql:quickload :openai)
cl-user> (openai:completions "The President went to Congress" 200)
" instead. The Congress was not oblivious of what the Supreme Court's
majority had ruled. Even four Justices had found nothing to criticize
in the President's requirement that the Federal Government's four-year
contract with Microsoft be extended by twice as much, that the burden
of proof be put on Microsoft to show, not a violation, but that it
should be allowed to compete in any open industry, that the
combination of a powerful operating system with a powerful PC
operating system also give to Microsoft a PC-compatible
interface-vendor neutral, the Court called it- which other operating
systems could match, based on progress and development of time. Why
should that not be the case? Should the Court declare that any
operating system which antedated Microsoft's dominant operating system
would not be free to enter the market? The Justices on the Court, the
Chief Justice included, found it an extraordinarily tough question.\"
In a minority decision, Chief Justice Rehnquist, for instance, writes
this: \"Justice Kennedy's"
```

Also try **openai:summarize** and **openai:answer-question**. See my book for examples and example output.

```
(setf z "Pizzas are made with whole wheat. The standard topping is red sauce but white sauce is also available. Large pizzas are $20 and medium are $12. Toppings include mushrooms, extra cheese, garlic, and sausage. How much is a large Pizza?")

(openai:answer-question z 200)
"$20.00"
CL-USER 20 > (setf z "Pizzas are made with whole wheat. The standard topping is red sauce but white sauce is also available. Large pizzas are $20 and medium are $12. Toppings cost $1 each and include mushrooms, extra cheese, garlic, and sausage. How much is a large Pizza with mushroom topping?")
"Pizzas are made with whole wheat. The standard topping is red sauce but white sauce is also available. Large pizzas are $20 and medium are $12. Toppings cost $1 each and include mushrooms, extra cheese, garlic, and sausage. How much is a large Pizza with mushroom topping?"

CL-USER 21 > (openai:answer-question z 22)
"$20 + $1 + $1 = $22"
```

Not quite right.

This works with ChatGPT, but not this API (tested 2023/02/04):

```
(setf z "Pizzas are made with whole wheat. The standard topping is red sauce but white sauce is also available. Large pizzas are $20 and medium are $12. Toppings cost $1 each and include mushrooms, extra cheese, garlic, and sausage. How much is a large Pizza with mushroom topping?")
```

Another try (this works very well with ChatGPT):

```
CL-USER 23 > (openai:answer-question "What should I pack for a road trip to Arizona?" 150)
"Arizona is a great place to visit in the winter, spring, and fall. The summers are very hot, so pack light and bring lots of water."
```

## Embeddings

```
(setf e1 (openai::embeddings "John bought a new car"))

(setf e2 (openai::embeddings "who bought what"))

CL-USER 23 > (openai::dot-product e1 e1)
0.9999999

CL-USER 24 > (openai::dot-product e1 e2)
0.7841768
```
