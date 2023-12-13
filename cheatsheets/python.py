# Python Cheatsheet

# Comments
# Print
print("Hello World!")
# string interpolation
place = "Universe"
print(f'Hello {place}!')

# Built-ins
# true/false
t = True
f = False
# null value
n = None
# if/else-if/else, and/or
if not t:
    print('if branch')
elif f and True:
    print('elif branch')
else:
    print('else branch')
# switch, Python 3.10+
status = 200
match status:
    case 200:
        str = 'Ok'
    case 400:
        str = 'Err'
    case _:
        str = 'Error'
# exceptions
if True == False:
    raise 'IMPASTABOWL!'
# function syntax
def double(x):
    return x * 2
# assert
assert True

# Math
x = 5 + 4 - 3 * 2 / 1
if x % 2 == 0:
    print(f'{x} is even')
else:
    print(f'{x} is odd')
# exponentiation
x_cubed = x ** 3

# Strings
str = 'foobar'
# split
letters = list(str)
# sort
ordered = sorted(letters)
# join
result = ''.join(ordered)
print(f'foobar sorted is {result}')
# substring
target = 'tar-jay'
if target in 'I shop at tar-jay for the sales':
    print(f'found {target} in substring')

# Regex syntax

# Arrays/Lists
# literal
items = [3,1,2,5,4]
# iteration
for x in items:
    if x % 2 == 0:
        print(f"{x} is even")
    else:
        print(f"{x} is odd")
# sort
sorted_items = sorted(items)
# custom sorting
# splicing
# merge sort
# quick sort

# Hash (python calls them dictionaries)
# literal
d = {}
# default value
assert d.get('not_there') == None
# key existence
d['key'] = 'value'
d['key'] square brackets raise KeyError if not found
assert 'key' in d
# value existence
assert 'value' in d.values()
# group by
things = [{'year': 2010, 'name': 'foo'},
          {'year': 2010, 'name': 'bar'},
          {'year': 2012, 'name': 'qux'}]
#---
from collections import defaultdict
names_by_year = defaultdict(list)
for thing in things:
    names_by_year[thing['year']].append(thing['name'])
#---
assert names_by_year[2010] == ['foo', 'bar']
assert names_by_year[2012] == ['qux']
# combining 2 hashes

# Class syntax
class Node(object):
    def __init__(val, next=None):
        self.val = val
        self.next = next
# methods
dir([1,2,3])

# Node
# Linked list example
# Tree example
# Graph example
