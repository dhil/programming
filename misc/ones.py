# Given an array of ones and zeroes. Write a function that given an
# interval of the array returns the number of ones contained within
# that interval.

arr = [1,0,1,0,1,1,0,1]

tbl = [0,0,0,0,0,0,0,0]

def precompute():
    for i in range(0, len(arr)):
        if i == 0:
            tbl[i] = arr[i]
        else:
            tbl[i] = tbl[i-1] + arr[i]

def query(i,j):
    if i > 0 and i <= j and j < len(arr):
        x = tbl[i-1]
        y = tbl[j]
        return y - x
    elif i == 0 and i <= j and j < len(arr):
        return tbl[j]
    else:
        return None

precompute()

print(query(0,0))
print(query(0,1))
print(query(0,2))
print(query(0,7))
print(query(2,4))
print(query(6,7))
print(query(7,6))
print(query(-3,6))
