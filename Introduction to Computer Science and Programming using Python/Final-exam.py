#randomnly selected questions from different people that posted final exam questions on their github
#Not all the questions in the posts were the same, I think this is the case because the questions changed over the years

# Problem 3
#
# Write a function is_triangular that meets the specification below. A triangular
# number is a number obtained by the continued summation of integers starting
# from 1. For example, 1, 1+2, 1+2+3, 1+2+3+4, etc., corresponding to
# 1, 3, 6, 10, etc., are triangular numbers.
#

def is_triangular(k):
    """
    k, a positive integer
    returns True if k is triangular and False if not
    """
    def triangular(k):
        if k == 1:
            return 1
        else:
            return k + triangular(k-1)

    def list_triangulars(k):
        list = []
        for numbers in range(1, k):
            list.append(triangular(numbers))
        return list

    return k in list_triangulars(k)

# print(is_triangular(6))
# print(is_triangular(7))
# print(is_triangular(10))

# Problem 3
# 10/10 points (graded)
# Numbers in Mandarin follow 3 simple rules.

# There are words for each of the digits from 0 to 10.
# For numbers 11-19, the number is pronounced as "ten digit", so for example, 16 would be pronounced (using Mandarin) as "ten six".
# For numbers between 20 and 99, the number is pronounced as “digit ten digit”, so for example, 37 would be pronounced (using Mandarin) as
# "three ten seven". If the digit is a zero, it is not included.
# Here is a simple Python dictionary that captures the numbers between 0 and 10.

# We want to write a procedure that converts an American number (between 0 and 99), written as a string, into the equivalent Mandarin.

# Example Usage
# convert_to_mandarin('36') will return san shi liu
# convert_to_mandarin('20') will return er shi
# convert_to_mandarin('16') will return shi liu
trans = {'0':'ling', '1':'yi', '2':'er', '3':'san', '4': 'si', '5':'wu', '6':'liu', '7':'qi', '8':'ba', '9':'jiu', '10': 'shi'}

def convert_to_mandarin(us_num):
    '''
    us_num, a string representing a US number 0 to 99
    returns the string mandarin representation of us_num
    '''
    if us_num == '10':
        return trans[us_num]
    elif len(us_num) == 2:
        ten_num = us_num[0]
        num = us_num[1]
        if us_num[1] == '0':
            return trans[ten_num] + ' ' + 'shi'
        else:
            return trans[ten_num] + ' ' + 'shi' + ' ' + trans[num]
    else:
        num = us_num[0]
        return trans[num]

# print('36= ' + convert_to_mandarin('36'))
# print('20= ' + convert_to_mandarin('20'))
# print('16= ' + convert_to_mandarin('16'))
# print('7= ' + convert_to_mandarin('7'))
# print('99= ' + convert_to_mandarin('99'))
# print('0= ' + convert_to_mandarin('0'))
# print('10= ' + convert_to_mandarin('10'))

# Problem 4
# 20.0/20.0 points (graded)
# You are given the following definitions:
# A run of monotonically increasing numbers means that a number at position k+1 in the sequence is greater than or equal to the number at
# position k in the sequence.
# A run of monotonically decreasing numbers means that a number at position k+1 in the sequence is less than or equal to the number at
# position k in the sequence.
# Implement a function that meets the specifications below.

# For example:
# If L = [10, 4, 3, 8, 3, 4, 5, 7, 7, 2] then the longest run of monotonically increasing numbers in L is [3, 4, 5, 7, 7] and the longest
# run of monotonically decreasing numbers in L is [10, 4, 3]. Your function should return the value 26 because the longest run of
# monotonically increasing integers is longer than the longest run of monotonically decreasing numbers.
# If L = [5, 4, 10] then the longest run of monotonically increasing numbers in L is [4, 10] and the longest run of monotonically
# decreasing numbers in L is [5, 4]. Your function should return the value 9 because the longest run of monotonically decreasing integers
# occurs before the longest run of monotonically increasing numbers.


def longest_run(L):
    """
    Assumes L is a list of integers containing at least 2 elements.
    Finds the longest run of numbers in L, where the longest run can
    either be monotonically increasing or monotonically decreasing.
    In case of a tie for the longest run, choose the longest run
    that occurs first.
    Does not modify the list.
    Returns the sum of the longest run.
    """
    current_inc = []
    current_decr = []
    longest = []

    for number in range(len(L)-1):
        if L[number] <= L[number + 1]:
            current_inc.append(L[number])
            if len(current_inc)+1 > len(longest): #len + 1 because only the first number of the pair is appended
                longest = current_inc
                longest.append(L[number+1]) #append last number in the sequence
        else:
            current_inc = []
        if L[number] >= L[number + 1]:
            current_decr.append(L[number])
            if len(current_decr)+1 > len(longest): #len + 1 because only the first number of the pair is appended
                longest = current_decr
                longest.append(L[number+1]) #append last number in the sequence
        else:
            current_decr = []
    return sum(longest)

#print(longest_run([10, 4, 3, 8, 3, 4, 5, 7, 7, 2]))
#print(longest_run([5, 4, 10]))


# Problem 5
# 15.0/15.0 points (graded)
# In this problem, you will implement a class according to the specifications in the template file usresident.py. The file contains a
# Person class similar to what you have seen in lecture and a USResident class (a subclass of Person). Person is already implemented for
# you and you will have to implement two methods of USResident.

# For example, the following code:
# a = USResident('Tim Beaver', 'citizen')
# print(a.getStatus())
# b = USResident('Tim Horton', 'non-resident')

# will print out:
# citizen
# ## will show that a ValueError was raised at a particular line

## DO NOT MODIFY THE IMPLEMENTATION OF THE Person CLASS ##
class Person(object):
    def __init__(self, name):
        #create a person with name name
        self.name = name
        try:
            firstBlank = name.rfind(' ')
            print(firstBlank)
            self.lastName = name[firstBlank+1:]
        except:
            self.lastName = name
        self.age = None
    def getLastName(self):
        #return self's last name
        return self.lastName
    def setAge(self, age):
        #assumes age is an int greater than 0
        #sets self's age to age (in years)
        self.age = age
    def getAge(self):
        #assumes that self's age has been set
        #returns self's current age in years
        if self.age == None:
            raise ValueError
        return self.age
    def __lt__(self, other):
        #return True if self's name is lexicographically less
        #than other's name, and False otherwise
        if self.lastName == other.lastName:
            return self.name < other.name
        return self.lastName < other.lastName
    def __str__(self):
        #return self's name
        return self.name

class USResident(Person):
    """
    A Person who resides in the US.
    """
    def __init__(self, name, status):
        """
        Initializes a Person object. A USResident object inherits
        from Person and has one additional attribute:
        status: a string, one of "citizen", "legal_resident", "illegal_resident"
        Raises a ValueError if status is not one of those 3 strings
        """
        self.status = status
    def getStatus(self):
        #assumes that self's status has been set
        #returns self's status and valueerror if no valid input
        if self.status not in ["citizen", "legal_resident", "illegal_resident"]:
            raise ValueError
        return self.status

#test cases
# x = Person('Virgil van Dijk')
# print(x.getLastName())
# y = USResident('Joe Biden', 'citizen')
# print(y.getStatus())
# z = USResident('Donald Trump', 'president')
# print(z.getStatus())

# Problem 7
# 20.0/20.0 points (graded)
# Write a function called general_poly, that meets the specifications below.

# For example, general_poly([1, 2, 3, 4])(10) should evaluate to 1234 because
# 1 * 10^3 + 2 * 10^2 + 3 * 10^1 + 4 * 10^0

# So in the example the function only takes one argument with general_poly([1, 2, 3, 4]) and it returns a function that you can apply to a
# value, in this case x = 10 with general_poly([1, 2, 3, 4])(10).

# Paste your code here
def general_poly (L):
    """
    L, a list of numbers (n0, n1, n2, ... nk)
    Returns a function, which when applied to a value x, returns the value
    n0 * x^k + n1 * x^(k-1) + ... nk * x^0
    """
    x = 10
    total = 0
    for item in L:
        total += item * x**(len(L)-item)
    return total

#test
#print(general_poly([1, 2, 3, 4]))
