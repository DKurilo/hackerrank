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
    for(int i = 0;i < n;i++) {
        ds[s] = true;
        s = (s * p + q) & MASK;
    }
    cout << count(ds.begin(), ds.end(), true) << endl;
    return 0;
}
