# Comments start with a hash
# files are .py

x = 5
# string interpolation
print(f'{x} is five')

null_val = None

def function(arg):
    double_arg = arg + arg

    if double_arg > 10:
        print("double_arg is a bigg!")
    else:
        return "no"

    items = [1,2,3,4,5]

    for x in items:
        if x % 2 == 0:
            print(f"{x} is even")
        else:
            print(f"{x} is odd")

        if x > 2:
            # this is just to demonstrate code indentation
            x = x * 10


    return "yes"
