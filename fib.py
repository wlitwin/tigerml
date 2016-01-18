def fib(i):
    if i <= 0:
        return 0
    elif i <= 2:
        return 1
    else:
        return fib(i-1) + fib(i-2)

print(fib(20))
