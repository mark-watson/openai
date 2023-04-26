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

## Experiments with prompts

### Experiment 1: find entities and relationshiops

```
The task is to extract as many relevant entities and the relationships between these entities.\n
When possible, return the type of an entity using the Wikipedia class system.\n
Extract all relevant relationships between identified entities.\n
The relationships should follow the Wikipedia schema type.
The output of a relationship should be in a form of a triple Head, Relationship, Tail, for example
Bill Gates, BORN_AT, Seatle.\n
As an example "Bill Gates worked at Microsoft" should have an output with the following format:\n
entity
Bill Gates, person
Microsoft, organization

relationships
Bill Gates, WORKET AT, Microsoft\n
Task: process this text:
Steve Jobes founded Apple Computer and Bill Gates started Microsoft. Seattle is in the state of Washington.
Steve lived in Palo Alto. Palo Alto is located in California.
```

ChatGPT 4 returns:

```
entities
Steve Jobs, person
Apple Computer, organization
Bill Gates, person
Microsoft, organization
Seattle, city
Washington, administrative territorial entity

relationships
Steve Jobs, FOUNDED, Apple Computer
Bill Gates, FOUNDED, Microsoft
Seattle, IS_IN, Washington
```

### Experiment 2: disambigualte relation ship names

```
Given a list of relationships like:

brother
located in
sister
sibling
found at

Then return a list with an index for each unique relationship, like:

brother, 1
located in, 2
sister, 1
sibling, 1
found at, 2

Task: process the following text:

Sam has a brother Mike. Suzy has a sister Sally, a mother Jill, and a father Bill.
Mike is in San Diego. Sam is located is Oakland. Suzy's family alllive in Los Angeles.
```

Results are:

```
entities
Steve Jobs, person
Apple Computer, organization
Bill Gates, person
Microsoft, organization
Seattle, city
Washington, administrative territorial entity
Palo Alto, city
California, administrative territorial entity

relationships
Steve Jobs, FOUNDED, Apple Computer
Bill Gates, STARTED, Microsoft
Seattle, IS_IN, Washington
Steve Jobs, LIVED_IN, Palo Alto
Palo Alto, IS_LOCATED_IN, California
```

ChatGPT 4 results are:

```
entities
Sam, person
Mike, person
Suzy, person
Sally, person
Jill, person
Bill, person
San Diego, city
Oakland, city
Los Angeles, city

relationships
Sam, HAS_A, brother (Mike)
Suzy, HAS_A, sister (Sally)
Suzy, HAS_A, mother (Jill)
Suzy, HAS_A, father (Bill)
Mike, IS_IN, San Diego
Sam, IS_LOCATED_IN, Oakland
Suzy, FAMILY_LIVE_IN, Los Angeles
```

### Experiment 3: topic modeling (or document classification)

```
Topic modeling involves reading text and assigning a topic name (also known as a document classification).
The topics that you know how to classify are:

sports
health
chemistry
economy
politics

Task: what is the best topic name for the following text:

John and Sam went to the playing field yesterday. Sam sparined his ankle.
```

The result is:

```
sports
```

