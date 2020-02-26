import collections
from collections import Iterable


def opgave1(mylist):
    return sorted(mylist) == list(range(1, max(mylist)+1))


def opgave2(mylist):
    return (i for i in range(1, len(mylist)+1) if not(i in mylist))


def opgave3a(filename):
    with open(filename) as f:
        list = []
        for line in f:
            list.append([int(x) for x in line.split()])
    return list


def opgave3b(mylist):
    for i in mylist:
        for n in i:
            print(n, end=" ")
        print()


def opgave3(filename):
    opgave3b(opgave3a(filename))


def sum_nested_it(mylist):
    # While any element of i is an instance of Iterable.
    # Will not loop if no elements in mylist are an instance of Iterable.
    while any(isinstance(i, Iterable) for i in mylist):
        # Append all values from outer Iterables in mylist to mylist.
        # If element is not an instance of Iterable, convert it to list(iterable) 
        # and append the value.
        mylist = [j for i in mylist for j in (
            i if isinstance(i, Iterable) else [i])]
        # Return to top of loop with -1 dimension list.
        # Continues till the whole list is flattend.
    return sum(mylist)

# print(opgave1([1,2,3,4,5]))
# print(opgave1([1,4,5]))

# gen = opgave2([2, 8])
# print(gen)
# for i in gen:
#     print(i)

# print(opgave3a('list.txt'))
# opgave3('list.txt')

# print(sum_nested_it([[[[[67, 18, 63, 25, 2]]]], [[[61, 40, 9]]], [[[[6, 8, 58, 24]]]], 10, 10, 7, [[[[[[84]]]]]]]))
# print(sum_nested_it([1, 2, 3, {29, 79}, {32, 93}]))
# print(sum_nested_it([1,2,3,4,56]))
