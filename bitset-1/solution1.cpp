#pragma GCC optimize ("O3")
#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

#define SIZE 2147483648ul
#define MASK 2147483647ul

int main() {
    unsigned long n, s, p, q;
    cin >> n >> s >> p >> q;
    vector<bool> ds;
    ds.assign(SIZE, false);
    unsigned int count = 0;
    for(;count < n && !ds[s];count++) {
        ds[s] = true;
        s = (s * p + q) & MASK;
    }
    cout << count << endl;
    return 0;
}
