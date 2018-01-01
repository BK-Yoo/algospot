def quick_sort_bf(arr):
    if not arr or len(arr) == 1:
        return arr

    pivot_idx = len(arr) // 2
    pivot_value = arr[pivot_idx]

    equal_arr, less_arr, more_arr = [], [], []
    for e_val in arr:
        if e_val == pivot_value:
            equal_arr.append(e_val)
        elif e_val > pivot_value:
            more_arr.append(e_val)
        else:
            less_arr.append(e_val)

    return quick_sort_bf(less_arr) + equal_arr + quick_sort_bf(more_arr)


def partition(arr, lo, hi):
    start = lo
    end = hi - 1
    pivot = arr[hi]
    while start < end:
        while start <= end and arr[start] <= pivot:
            start += 1
        while start <= end and arr[end] > pivot:
            end -= 1
        if start < end:
            arr[start], arr[end] = arr[end], arr[start]
    arr[hi], arr[start] = arr[start], arr[hi]
    return start


# in-place quick sort
def quick_sort_ip(arr, lo, hi):
    if lo < hi:
        pivot_index = partition(arr, lo, hi)
        quick_sort_ip(arr, lo, pivot_index - 1)
        quick_sort_ip(arr, pivot_index + 1, hi)

def insertion_sort(arr):
    for i in range(0, len(arr)):
        for j in range(i, -1, -1):
            if arr[i] >= arr[j]:
                arr[i], arr[j] = arr[i]


if __name__ == "__main__":
    test = [32, 7, 14, 100, 4, 5, 3, 2, 1, 1, 1]
    quick_sort_ip(test, 0, len(test) - 1)
    print(test)
