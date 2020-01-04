#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int lexicographic_sort(const char* a, const char* b) {
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

int lexicographic_sort_reverse(const char* a, const char* b) {
    return -lexicographic_sort(a, b);
}

int find_char(char c, char *cs, int n) {
    for (int i = 0; i < n; i++) {
        if (cs[i] == c) {
            return i;
        }
    }
    return -1;
}

int get_distinct_char_count(const char *cs) {
    int len = strlen(cs), distinct_number = 0;
    char *distinct = malloc(sizeof(*distinct) * len);
    for (int i = 0; i < len; i++) {
        if (find_char(cs[i], distinct, distinct_number) < 0) {
            distinct[distinct_number] = cs[i];
            distinct_number++;
        }
    }
    return distinct_number;
}

int cmp_with_fallback(const char* a, const char* b,
                      int (*weight_func)(const char* a),
                      int (*fallback_cmp_func)(const char* a, const char* b)) {
    int cmp = weight_func(a) - weight_func(b);
    return cmp == 0 ? fallback_cmp_func(a, b) : cmp;
}

int sort_by_number_of_distinct_characters(const char* a, const char* b) {
    return cmp_with_fallback(a, b, get_distinct_char_count, lexicographic_sort);
}

int str_len(const char *a) {
    return strlen(a);
}

int sort_by_length(const char* a, const char* b) {
    return cmp_with_fallback(a, b, str_len, lexicographic_sort);
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
    string_sort(arr + (sizeof(char *) * (middle + 1)), len - middle - 1, cmp_func);
}


int main() 
{
    int n;
    scanf("%d", &n);
  
    char** arr;
	arr = (char**)malloc(n * sizeof(char*));
  
    for(int i = 0; i < n; i++){
        *(arr + i) = malloc(1024 * sizeof(char));
        scanf("%s", *(arr + i));
        *(arr + i) = realloc(*(arr + i), strlen(*(arr + i)) + 1);
    }
  
    string_sort(arr, n, lexicographic_sort);
    for(int i = 0; i < n; i++)
        printf("%s\n", arr[i]);
    printf("\n");

    string_sort(arr, n, lexicographic_sort_reverse);
    for(int i = 0; i < n; i++)
        printf("%s\n", arr[i]); 
    printf("\n");

    string_sort(arr, n, sort_by_length);
    for(int i = 0; i < n; i++)
        printf("%s\n", arr[i]);    
    printf("\n");

    string_sort(arr, n, sort_by_number_of_distinct_characters);
    for(int i = 0; i < n; i++)
        printf("%s\n", arr[i]); 
    printf("\n");
}
