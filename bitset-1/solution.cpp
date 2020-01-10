#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

unsigned int next(unsigned int x, unsigned int p, unsigned long q, unsigned long mask, int n) {
    for (int i = 0; i < n; i++) {
        q += ((p & 1) == 1 ? x : 0);
        p >>= 1;
        x <<= 1;
    }
    return q & mask;
}

int main() {
    unsigned long n, s, p, q;
    cin >> n >> s >> p >> q;
    vector<bool> ds;
    unsigned long modulo = pow(2, 31), mask = modulo - 1;
    ds.assign(modulo, false);
    unsigned long x = s;
    int count = 0;
    for (;count < n && !ds[x]; count++) {
        ds[x] = true;
        x = next(x, p, q, mask, 31);
    }
    cout << count << endl;
    return 0;
}
