// #include<bits/stdc++.h>
#include <iostream>

using namespace std;

struct Workshop {
    Workshop() {}
    void init(int start, int duration) {
        this->start = start;
        this->duration = duration;
        end = start + duration;
    }
    int start;
    int duration;
    int end;
};
struct Available_Workshops {
    Available_Workshops(Workshop *ws, int n) : ws(ws), length(n) {}
    ~Available_Workshops() {
        for (int i = 0; i < length; i++) {
            delete &ws[i];
        }
    }
    int length;
    Workshop *ws;
};

Available_Workshops* initialize (int start_time[], int duration[], int n) {
    Workshop* ws = new Workshop[n];
    for (int i = 0; i < n; i++) {
        ws[i].init(start_time[i], duration[i]);
    }
    return new Available_Workshops(ws, n);
}

void fillMaxWorkshops(int n, int *possible, Available_Workshops* ptr) {
    int max_count = 0;
    for (int i = 0; i < ptr->length; i++) {
        if (possible[i] == 0 || i == n) {
            continue;
        }
        if (ptr->ws[i].start <= ptr->ws[n].start && ptr->ws[i].end >= ptr->ws[n].end) {
            possible[i] = 0;
            continue;
        }
        if (ptr->ws[n].start <= ptr->ws[i].start && ptr->ws[n].end >= ptr->ws[i].end) {
            possible[n] = 0;
            return;
        }
        if (ptr->ws[n].end <= ptr->ws[i].start) {
            if (possible[i] == -1) {
                fillMaxWorkshops(i, possible, ptr);
            }
            if (possible[i] > max_count) {
                max_count = possible[i];
            }
        }
    }
    possible[n] = max_count + 1;
}

int CalculateMaxWorkshops(Available_Workshops* ptr) {
    int *possible = new int[ptr->length];
    for (int i = 0; i < ptr->length; i++) {
        possible[i] = -1;
    }
    int max_count = -1, n = ptr->length;
    for (int i = 0; i < n; i++) {
        if (possible[i] == -1) {
            fillMaxWorkshops(i, possible, ptr);
        }
        if (possible[i] > max_count) {
            max_count = possible[i];
        }
    }
    return max_count;
}

int main(int argc, char *argv[]) {
    int n; // number of workshops
    cin >> n;
    // create arrays of unknown size n
    int* start_time = new int[n];
    int* duration = new int[n];

    for(int i=0; i < n; i++){
        cin >> start_time[i];
    }
    for(int i = 0; i < n; i++){
        cin >> duration[i];
    }

    Available_Workshops * ptr;
    ptr = initialize(start_time,duration, n);
    cout << CalculateMaxWorkshops(ptr) << endl;
    return 0;
}

