
def cons(s1, s2):
    return lambda p: p(s1, s2)

def car(s):
    return s(lambda x, y:  x)

def cdr(s):
    return s(lambda x, y:  y)

def delay(e):
    return eval(e)

def inc(x):
    print(f"Inc: {x}")
    return x + 1

(delay "inc 2")

c1 = cons(3, 4)
car(c1)
cdr(c1)

def cadr(s):
    return car(cdr(s))


ones = cons(1, ones)

def integer_from(n):
    yield cons(n, integer_from(n + 1))

def integers():
    n = 0
    while True:
        yield n
        n += 1

integers = integer_from(0)

car(integers)

while True: 
    print(integers())

def infinite_sequence():
    num = 0
    while True:
        yield num
        num += 1

twos =  cons(2, lambda: twos)  

