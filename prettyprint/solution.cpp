#include <iostream>
#include <iomanip> 
using namespace std;

int main() {
	int T; cin >> T;
	cout << setiosflags(ios::uppercase);
	cout << setw(0xf) << internal;
	while(T--) {
		double A; cin >> A;
		double B; cin >> B;
		double C; cin >> C;

        cout << setw(0) << nouppercase;
        ios_base::fmtflags state(cout.flags());
        cout << hex << showbase << (long)A << endl;
        cout.flags(state);
        cout << fixed << setw(0xf) << setfill('_') << right << setprecision(2) << showpoint << showpos << B << endl;
        cout.flags(state);
        cout.unsetf(ios_base::floatfield);
        cout << setw(15) << uppercase << scientific << setprecision(9) << C << endl;

	}
	return 0;

}
