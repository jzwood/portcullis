# PORTCULLIS INTERNAL
def _plus_(a):
    return lambda b: a + b

def _minus_(a):
    return lambda b: a - b

def _mult_(a):
    return lambda b: a * b

def _div_(a):
    return lambda b: a / b

def _rem_(a):
    return lambda b: a % b

def _gt_(a):
    return lambda b: 1 if a > b else 0

def _gte_(a):
    return lambda b: 1 if a >= b else 0

def _lt_(a):
    return lambda b: 1 if a < b else 0

def _lte_(a):
    return lambda b: 1 if a <= b else 0

def _eq_(a):
    return lambda b: 1 if a == b else 0

def _cons_(a):
    return lambda b: [a] + b

# USER CODE
