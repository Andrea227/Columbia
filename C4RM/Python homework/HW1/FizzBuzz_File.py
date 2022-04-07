

def FizzBuzz(start, finish):
    n = finish - start + 1
    v = []
    x = list(range(start, finish+1))
    for i in x:
        if i % 15 == 0:
            v.append('fizzbuzz')
        elif i % 3 == 0:
            v.append('fizz')
        elif i % 5 == 0:
            v.append('buzz')
        else:
            v.append(i)
    return v
