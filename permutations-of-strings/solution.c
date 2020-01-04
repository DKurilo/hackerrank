#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int lexicographic_cmp(const char* a, const char* b) {
    int alen = strlen(a), blen = strlen(b), i = 0;
    while (i < alen || i < blen) {
        if (i >= alen) {
            return -1;
        }
        if (i >= blen) {
            return 1;
        }
        if (a[i] > b[i]) {
            return 1;
        }
        if (a[i] < b[i]) {
            return -1;
        }
        i++;
    }
    return 0;
}

void swap(char **arr, const int i, const int j) {
    char *temp = arr[i];
    arr[i] = arr[j];
    arr[j] = temp;
}

void string_sort(char** arr,const int len,int (*cmp_func)(const char* a, const char* b)){
    if (len <= 1) {
        return;
    }
    int left = 0, middle = len - 1, right = middle;
    while (left < middle) {
        if (cmp_func(arr[left], arr[middle]) > 0) {
            while (left < middle && cmp_func(arr[right], arr[middle]) > 0) {
                if (right == middle) {
                    swap(arr, middle, middle - 1);
                    middle--;
                }
                right--;
            }
            if (left < middle) {
                swap(arr, left, right);
            }
        }
        left++;
    }
    string_sort(arr, middle, cmp_func);
    string_sort(&arr[middle + 1], len - middle - 1, cmp_func);
}

int next_permutation(int n, char **s)
{
    for (int i = n - 1; i > 0; i--) {
        if (lexicographic_cmp(s[i], s[i - 1]) > 0) {
            int j = n - 1;
            while (lexicographic_cmp(s[i - 1], s[j]) >= 0) {
                j--;
            }
            swap(s, i - 1, j);
            string_sort(&s[i], n - i, lexicographic_cmp);
            return 1;
        }
    }
    return 0;
}

int main()
{
	char **s;
	int n;
	scanf("%d", &n);
	s = calloc(n, sizeof(char*));
	for (int i = 0; i < n; i++)
	{
		s[i] = calloc(11, sizeof(char));
		scanf("%s", s[i]);
	}
	do
	{
		for (int i = 0; i < n; i++)
			printf("%s%c", s[i], i == n - 1 ? '\n' : ' ');
	} while (next_permutation(n, s));
	for (int i = 0; i < n; i++)
		free(s[i]);
	free(s);
	return 0;
}
