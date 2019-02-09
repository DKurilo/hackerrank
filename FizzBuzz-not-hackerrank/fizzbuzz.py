print('\n'.join(map(
    (lambda x: ''.join(
        ({0:['Fizz']}.get(x%3,[])+{0:['Buzz']}.get(x%5,[])+['',str(x)])[:2]
    )), 
    range(1, 101))))

