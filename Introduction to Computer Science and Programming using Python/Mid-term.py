#randomnly selected questions from different people that posted mid-term question on their github
#Some are similar, some are different, I think this is the case because the questions changed over the years

# Problem 4
# 10/10 points (graded)
# Implement a function called closest_power that meets the specifications below.

# def closest_power(base, num):
#     '''
#     base: base of the exponential, integer > 1
#     num: number you want to be closest to, integer > 0
#     Find the integer exponent such that base**exponent is closest to num.
#     Note that the base**exponent may be either greater or smaller than num.
#     In case of a tie, return the smaller value.
#     Returns the exponent.
#     '''
#     # Your code here

#For example,
# closest_power(3,12) returns 2
# closest_power(4,12) returns 2
# closest_power(4,1) returns 0

def closest_power(base, num):
  exponent = 0
  for exponent in range(0, num):
    value = base**exponent
    if value == num:
        return exponent
    elif value < num:
        neg_difference = num - value
        neg_exp = exponent
        exponent += 1
        continue
    else:
        pos_difference = abs(num - value)
        pos_exp = exponent
    if neg_difference <= pos_difference:
        return neg_exp
    else:
        return pos_exp

#Exercise 4:

#Write a Python function that returns the sublist of strings in aList
#that contain fewer than 4 characters.
#For example, if aList = ["apple", "cat", "dog", "banana"],
#your function should return: ["cat", "dog"]
#This function takes in a list of strings and returns a list of strings.
#Your function should not modify aList.
#"""
def lessThan4(anlist):
    for word in anList:
        if len(word) >= 4:
            anList.remove(word)
    return anList

anList = ["apple", "cat", "dog", "banana"]
print(lessThan4(anList))

#Excercise 3

#Write a recursive Python function, given a non-negative integer N,
#to calculate the no. of occurrences of digit 7 in N.
#Hint: Mod (%) by 10 gives you the rightmost digit (126 % 10 is 6),
#while doing integer division by 10 removes the rightmost digit (126 / 10 is 12).
#This function has to be recursive; you may not use loops!
#This function takes in one integer and returns one integer.

N = 737617

def Recur7(N):
    if N == 0:
        return 0
    elif N % 10 == 7:
        return 1 + Recur7(N // 10)
    else:
        return 0 + Recur7(N // 10)

print(Recur7(N))


# Problem 5
# 10/10 points (graded)
# Write a Python function that returns the sum of the pairwise products of listA and listB. You should assume that listA and listB have
# the same length and are two lists of integer numbers. For example, if listA = [1, 2, 3] and listB = [4, 5, 6], the dot product is
# 1*4 + 2*5 + 3*6, meaning your function should return: 32

# Hint: You will need to traverse both lists in parallel.

# This function takes in two lists of numbers and returns a number.

# def dotProduct(listA, listB):
#     '''
#     listA: a list of numbers
#     listB: a list of numbers of the same length as listA
#     '''
#     # Your code here
def dotProduct(listA, listB):
    total = 0
    for element in range(0, len(listA)):
        product = listA[element] * listB[element]
        total += product
    return total

#listA = [1, 2, 3]
#listB = [4, 5, 6]
#print(dotProduct(listA, listB))

# Problem 6
# 15/15 points (graded)
# Implement a function that meets the specifications below.

# def deep_reverse(L):
#     """ assumes L is a list of lists whose elements are ints
#     Mutates L such that it reverses its elements and also
#     reverses the order of the int elements in every element of L.
#     It does not return anything.
#     """
#     # Your code here
# For example, if L = [[1, 2], [3, 4], [5, 6, 7]] then deep_reverse(L) mutates L to be [[7, 6, 5], [4, 3], [2, 1]]

def deep_reverse(L):
    L.reverse()
    for sub_list in L:
        sub_list.reverse()
    return L

#L = [[1, 2], [3, 4], [5, 6, 7]]
#print(deep_reverse(L))

# Problem 7
# 20/20 points (graded)
# Assume you are given two dictionaries d1 and d2, each with integer keys and integer values. You are also given a function f, that takes
# in two integers, performs an unknown operation on them, and returns a value.

# Write a function called dict_interdiff that takes in two dictionaries (d1 and d2).
# The function will return a tuple of two dictionaries: a dictionary of the intersect of d1 and d2 and a dictionary of the difference of d1 and d2, calculated as follows:
# intersect: The keys to the intersect dictionary are keys that are common in both d1 and d2. To get the values of the intersect
# dictionary, look at the common keys in d1 and d2 and apply the function f to these keys' values -- the value of the common key in d1 is
# the first parameter to the function and the value of the common key in d2 is the second parameter to the function. Do not implement f
# inside your dict_interdiff code -- assume it is defined outside.
# difference: a key-value pair in the difference dictionary is (a) every key-value pair in d1 whose key appears only in d1 and not in d2
# or (b) every key-value pair in d2 whose key appears only in d2 and not in d1.

# Here are two examples:
# If f(a, b) returns a + b
# d1 = {1:30, 2:20, 3:30, 5:80}
# d2 = {1:40, 2:50, 3:60, 4:70, 6:90}
# then dict_interdiff(d1, d2) returns ({1: 70, 2: 70, 3: 90}, {4: 70, 5: 80, 6: 90})
# If f(a, b) returns a > b
# d1 = {1:30, 2:20, 3:30}
# d2 = {1:40, 2:50, 3:60}
# then dict_interdiff(d1, d2) returns ({1: False, 2: False, 3: False}, {})

# def dict_interdiff(d1, d2):
#    '''
#    d1, d2: dicts whose keys and values are integers
#    Returns a tuple of dictionaries according to the instructions above
#    '''
#    # Your code here

def function(a,b):
    return a > b

def dict_interdiff(d1, d2):
    intersect = {}
    difference = {}
    for key in d1.keys():
        if key in d2.keys():
            intersect[key] = function(d1[key], d2[key])
        else:
            difference[key] = d1[key]
    for key in d2.keys():
        if key not in d1.keys():
            difference[key] = d2[key]
    return (intersect, difference)

#d1 = {1:30, 2:20, 3:30, 5:80}
#d2 = {1:40, 2:50, 3:60, 4:70, 6:90}

#print(dict_interdiff(d1, d2))

# Problem 8
# 20/20 points (graded)
# Implement a function that meets the specifications below.

# def applyF_filterG(L, f, g):
#    """
#    Assumes L is a list of integers
#    Assume functions f and g are defined for you.
#    f takes in an integer, applies a function, returns another integer
#    g takes in an integer, applies a Boolean function,
#        returns either True or False
#    Mutates L such that, for each element i originally in L, L contains
#        i if g(f(i)) returns True, and no other elements
#    Returns the largest element in the mutated L or -1 if the list is empty
#    """
#    # Your code here

# For example, the following functions, f, g, and test code:
# def f(i):
#     return i + 2
# def g(i):
#    return i > 5

# L = [0, -10, 5, 6, -4]
# print(applyF_filterG(L, f, g))
# print(L)
# Should print:6
# [5, 6]

# For this question, you will not be able to see the test cases we run. This problem will test your ability to come up with your own
# test cases.

def f(i):
    return i + 2

def g(i):
    return i > 5

def applyF_filterG(L, f, g):
    new_L = L.copy()
    for element in L:
        if g(f(element)) is False:
           new_L.remove(element)
    if len(new_L) == 0:
        return -1
    else:
        return max(new_L)

#L = [-0, -10, -5, -6, -4]
#L = []

#print(applyF_filterG(L, f, g))

# Problem 9
# 15/15 points (graded)
# Write a function to flatten a list. The list contains other lists, strings, or ints. For example, [[1,'a',['cat'],2],[[[3]],'dog'],4,5]
# is flattened into [1,'a','cat',2,3,'dog',4,5] (order matters).

# def flatten(aList):
#    '''
#    aList: a list
#    Returns a copy of aList, which is a flattened version of aList
#    '''


def flatten(aList):
    new_list = []
    for element in aList:
        if type(element) != list:
            new_list.append(element)
        else:
            new_list.extend(flatten(element))
    return new_list

#aList = [[1,'a',['cat'],2],[[[3]],'dog'],4,5]
#print(flatten(aList))
