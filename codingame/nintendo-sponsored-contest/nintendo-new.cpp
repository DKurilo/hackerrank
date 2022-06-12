#include <iostream>
#include <iomanip>

using namespace std;

void mult(const unsigned int* a, int size) {
    unsigned int* b = new unsigned int[size / 16]; // <- output tab
    for (int i = 0; i < size / 16; i++) {   // Write size / 16 zeros to b
        b[i] = 0;
    }

    for (int i = 0; i < size; i++)
        for (int j = 0; j < size; j++) {
            b[(i + j) / 32] ^= ( (a[i / 32] >> (i % 32)) &
                    (a[j / 32 + size / 32] >> (j % 32)) & 1 ) << ((i + j) % 32);   // Magic centaurian operation
        }

    for(int i = 0; i < size / 16; i++) {
        if (i > 0) {
            cout << ' ';
        }
        cout << setfill('0') << setw(8) << hex << b[i];       // print result
    }
}

int main()
{
    int size;

    cin >> size;

    unsigned int* a = new unsigned int[size / 16]; // <- input tab to encrypt

    for (int i = 0; i < size / 16; i++) {   // Read size / 16 integers to a
        cin >> hex >> a[i];
    }

    mult(a, size);
    cout << endl;

    // int size = 32;

    // for (int i = 0; i < 34; ++i){
    //     for (int j = 0; j < 34; ++j) {
    //         unsigned int* a = new unsigned int[2];
    //         a[0] = i;
    //         a[1] = j;
    //         mult(a, 32);
    //         cout << "  ";
    //     }
    //     cout << endl;
    // }
    // cout << endl;

    /*
       Good luck humans
       */
    return 0;
}

