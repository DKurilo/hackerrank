#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <list>
#include <map>
#include <unordered_map>
#include <set>
#include <algorithm>

using namespace std;

#define MINY 10000

int cnt = 0;

class Equation;
ostream & operator << (ostream &out, Equation& e);

void addMulToSum(const set<int> mul, set<set<int> > *sum) {
    set<set<int> >::iterator found = sum->find(mul);
    if (found == sum->end()) {
        sum->insert(mul);
    } else {
        sum->erase(found);
    }
}

class Equation {
    bool result;
    set<set<int> > sums;
    public:
    Equation(int n, int size, bool r) {
        result = r;
        int ymin = n < size ? 0 : n;
        int x = n < size ? n : size - 1;
        int y = n < size ? 0 : n - size + 1;
        while (x >= 0 && y < size) {
            set<int> muls;
            muls.insert(x);
            muls.insert(y + MINY);
            sums.insert(muls);
            x--;
            y++;
        }
    }
    Equation(bool r, set<set<int> > s) {
        result = r;
        sums = s;
    }
    bool getResult() {
        return result;
    }
    set<set<int> > *getSums() {
        return &sums;
    }
    bool isGood() {
        if (sums.size() == 0) {
            return !result;
        }
        return true;
    }
    bool setVar(int var, bool val) {
        set<set<int> > newSums = sums;
        for (set<set<int> >::iterator it = sums.begin(); it != sums.end(); ++it) {
            set<int>::iterator found = it->find(var);
            if (found != it->end()) {
                newSums.erase(newSums.find(*it));
                if (val) {
                    set<int> mul = *it;
                    mul.erase(mul.find(var));
                    if (mul.size() == 0) {
                        result = !result;
                    } else {
                        addMulToSum(mul, &newSums);
                    }
                }
            }
        }
        sums = newSums;
        return isGood();
    }
    bool applyDef(int var, Equation e) {
        set<set<int> > newSums;
        set<set<int> > defSums = *e.getSums();
        for (set<set<int> >::iterator it = sums.begin(); it != sums.end(); ++it) {
            set<int>::iterator found = it->find(var);
            if (found != it->end()) {
                if (it->size() == 1) {
                    result ^= e.getResult();
                    for (set<set<int> >::iterator it1 = defSums.begin(); it1 != defSums.end(); ++it1) {
                        set<int> muls= *it1;
                        addMulToSum(muls, &newSums);
                    }
                } else {
                    if (e.getResult()) {
                        set<int> muls = *it;
                        muls.erase(muls.find(var));
                        addMulToSum(muls, &newSums);
                    }
                    for (set<set<int> >::iterator it1 = defSums.begin(); it1 != defSums.end(); ++it1) {
                        set<int> muls = *it1;
                        for (set<int>::iterator it2 = it->begin(); it2 != it->end(); ++it2) {
                            if (*it2 != var) {
                                muls.insert(*it2);
                            }
                        }
                        addMulToSum(muls, &newSums);
                    }
                }
            } else {
                addMulToSum(*it, &newSums);
            }
        }
        sums = newSums;
        return isGood();
    }
    bool isFinal() {
        return sums.size() == 1 && (sums.begin()->size() == 1 || result);
    }
    bool canDelete() {
        return sums.size() == 0;
    }
    map<int, bool> &finals() {
        map<int, bool> *ds = new map<int, bool>;
        if (sums.size() == 0) {
            return *ds;
        }
        set<int> muls = *sums.begin();
        if (muls.size() == 1) {
            ds->insert(pair<int, bool>(*muls.begin(), result));
        } else if (result) {
            for (set<int>::iterator it = muls.begin(); it != muls.end(); ++it) {
                ds->insert(pair<int, bool>(*it, true));
            }
        }
        return *ds;
    }
    map<int, Equation> &getDefs() {
        map<int, Equation> *d = new map<int, Equation>;
        for (set<set<int> >::iterator it = sums.begin(); it != sums.end(); ++it) {
            if (it->size() == 1) {
                bool isIn = false;
                for (set<set<int> >::iterator it1 = sums.begin(); it1 != sums.end(); ++it1) {
                    if (it1 != it && it1->find(*it->begin()) != it1->end()) {
                        isIn = true;
                        break;
                    }
                }
                if (isIn) {
                    continue;
                }
                set<set<int> > newSums = sums;
                set<set<int> >::iterator found1 = newSums.find(*it);
                if (found1 != newSums.end()) {
                  newSums.erase(found1);
                }
                Equation eq (result, newSums);
                d->insert(pair<int, Equation>(*it->begin(), eq));
                return *d;
            }
        }
        return *d;
    }
};

ostream & operator << (ostream &out, Equation& e) {
    set<set<int> > *sums = e.getSums();
    for (set<set<int> >::iterator it = sums->begin(); it != sums->end(); ++it) {
        if (it != sums->begin()) {
            out << "+";
        }
        for (set<int>::iterator it1 = it->begin(); it1 != it->end(); ++it1) {
            if (it1 != it->begin()) {
                out << "*";
            }
            if (*it1 < MINY) {
                out << "x" << *it1;
            } else {
                out << "y" << (*it1 - MINY);
            }
        }
    }
    out << "=" << (e.getResult() ? "1" : "0");
    return out;
}

class EquationSystem;
ostream & operator << (ostream &out, EquationSystem &es);
class EquationSystem {
    list<Equation> equations;
    map<int, Equation> definitions;
    map<int, bool> finals;
    int size;
public:
    EquationSystem(unsigned int* a, int s) {
        size = s;
        for (int i = 0; i < 2 * size - 1; i++) {
            bool r = (a[i / 32] >> (i % 32)) & 0x1;
            equations.push_back(Equation(i, size, r));
        }
    }
    EquationSystem(list<Equation> eqs, map<int, Equation> defs, map<int, bool> fs, int s) {
        equations = eqs;
        definitions = defs;
        finals = fs;
        size = s;
    }
    list<Equation> *getEquations() {
        return &equations;
    }
    map<int, Equation> *getDefinitions() {
        return &definitions;
    }
    map<int, bool> *getFinals() {
        return &finals;
    }
    int getSize() {
        return size;
    }
    void solve(list<string> &results) {
        cnt++;
        // cout << "solving:\n" << "last: " << equations.back() << "\nfinals: " << finals.size() << "\ndefs: " << definitions.size() << "\nequations: " << equations.size() << "\nend of\n";
        // while no result (finals length < size * 2) and something was applied
        bool applied = true;
        while (finals.size() < size * 2 && applied) {
            applied = false;
            // find finals in equations and apply them
            for (list<Equation>::iterator it = equations.begin(); it != equations.end(); ++it) {
                if (it->isFinal()) {
                    applied = true;
                    map<int, bool> newFinals = it->finals();
                    equations.erase(it);
                    for (map<int, bool>::iterator it1 = newFinals.begin(); it1 != newFinals.end(); ++it1) {
                        map<int, bool>::iterator found = finals.find(it1->first);
                        if (found != finals.end()) {
                            if (found->second != it1->second) {
                                // system doesn't converge
                                return;
                            }
                            break;
                        }
                        for (list<Equation>::iterator it2 = equations.begin(); it2 != equations.end(); ++it2) {
                            if (!it2->setVar(it1->first, it1->second)) {
                                // system doesn't converge
                                return;
                            }
                            if (it2->canDelete()) {
                                it2 = prev(equations.erase(it2));
                            }
                        }
                        for (map<int, Equation>::iterator it2 = definitions.begin(); it2 != definitions.end(); ++it2) {
                            it2->second.setVar(it1->first, it1->second);
                        }
                        finals[it1->first] = it1->second;
                    }
                    break;
                }
            }
            if (applied) {
                continue;
            }
            // find finals in definitions and apply them
            for (map<int, Equation>::iterator it = definitions.begin(); it != definitions.end(); ++it) {
                if (it->second.canDelete()) {
                    applied = true;
                    for (list<Equation>::iterator it2 = equations.begin(); it2 != equations.end(); ++it2) {
                        if (!it2->setVar(it->first, it->second.getResult())) {
                            // system doesn't converge
                            return;
                        };
                        if (it2->canDelete()) {
                            it2 = prev(equations.erase(it2));
                        }
                    }
                    for (map<int, Equation>::iterator it2 = definitions.begin(); it2 != definitions.end(); ++it2) {
                        if (it2 != it) {
                            it2->second.setVar(it->first, it->second.getResult());
                        }
                    }
                    finals[it->first] = it->second.getResult();
                    definitions.erase(it);
                    break;
                }
            }
            if (applied) {
                continue;
            }
            // find and apply definitions
            for (list<Equation>::iterator it = equations.begin(); it != equations.end(); ++it) {
                map<int, Equation> newDefs = it->getDefs();
                if (newDefs.size() > 0) {
                    applied = true;
                    for (map<int, Equation>::iterator it1 = newDefs.begin(); it1 != newDefs.end(); ++it1) {
                        for (list<Equation>::iterator it2 = equations.begin(); it2 != equations.end(); ++it2) {
                            if (!it2->applyDef(it1->first, it1->second)) {
                                // system doesn't converge
                                return;
                            };
                            if (it2->canDelete()) {
                                it2 = prev(equations.erase(it2));
                            }
                        }
                        for (map<int, Equation>::iterator it2 = definitions.begin(); it2 != definitions.end(); ++it2) {
                            it2->second.applyDef(it1->first, it1->second);
                        }
                        Equation e = it1->second;
                        pair<int, Equation> p (it1->first, e);
                        definitions.insert(p);
                    }
                    break;
                }
            }
        }
        if (finals.size() < size * 2) {
            cout << "we are here" << endl;
            // if still not solved (finals length < size * 2) define some not defined to 0 and 1 and solve with additional definitions
            int toDefine = equations.begin() == equations.end() ?
                *definitions.begin()->second.getSums()->begin()->begin() :
                *equations.begin()->getSums()->begin()->begin();
            set<int> muls1;
            muls1.insert(toDefine);
            set<set<int> > sums1;
            sums1.insert(muls1);
            Equation eq1(false, sums1);
            equations.push_front(eq1);
            EquationSystem es1(equations, definitions, finals, size);
            es1.solve(results);
            equations.pop_front();
            set<int> muls2;
            muls2.insert(toDefine);
            set<set<int> > sums2;
            sums2.insert(muls2);
            Equation eq2(true, sums2);
            equations.push_front(eq2);
            EquationSystem es2(equations, definitions, finals, size);
            es2.solve(results);
        } else {
            // in other case add new results to results
            int as = size / 32;
            as = as == 0 ? 1 : as;
            unsigned int rx[as];
            unsigned int ry[as];
            for (int i = 0; i < as; ++i) {
                rx[i] = 0;
                ry[i] = 0;
            }
            for (map<int, bool>::iterator it = finals.begin(); it != finals.end(); ++it) {
                if (it->first >= MINY) {
                    int y = it->first - MINY;
                    ry[y / 32] |= ((it->second ? 0x1 : 0x0) << (y % 32));
                } else {
                    rx[it->first / 32] |= ((it->second ? 0x1 : 0x0) << (it->first % 32));
                }
            }
            stringstream s;
            for (int i = 0; i < as; ++i) {
                if (i > 0) {
                    s << " ";
                }
                s << setfill('0') << setw(8) << hex << rx[i];
            }
            for (int i = 0; i < as; ++i) {
                s << " " << setfill('0') << setw(8) << hex << ry[i];
            }
            results.push_back(s.str());
            cout << "res " << results.back() << "\n";
        }
    }
};
ostream & operator << (ostream &out, EquationSystem &es) {
    list<Equation> *eqs = es.getEquations();
    map<int, Equation> *dfs = es.getDefinitions();
    map<int, bool> *fns = es.getFinals();
    for (map<int, bool>::iterator it = fns->begin(); it != fns->end(); ++it) {
        if (it->first < MINY) {
            out << "f x" << it->first << "=" << (it->second ? "1" : "0") << "\n";
        } else {
            out << "f y" << (it->first - MINY) << "=" << (it->second ? "1" : "0") << "\n";
        }
    }
    for (map<int, Equation>::iterator it = dfs->begin(); it != dfs->end(); ++it) {
        if (it->first < MINY) {
            out << "d x" << it->first << "=";
        } else {
            out << "d y" << (it->first - MINY) << "=";
        }
        set<set<int> > *sums = it->second.getSums();
        for (set<set<int> >::iterator it = sums->begin(); it != sums->end(); ++it) {
            set<int> muls = *it;
            if (it != sums->begin()) {
                out << "+";
            }
            for (set<int>::iterator it1 = muls.begin(); it1 != muls.end(); ++it1) {
                if (it1 != muls.begin()) {
                    out << "*";
                }
                if (*it1 < MINY) {
                    out << "x" << *it1;
                } else {
                    out << "y" << (*it1 - MINY);
                }
            }
        }
        if (sums->size() == 0) {
            out << it->second.getResult();
        } else if (it->second.getResult()) {
            out << "+1";
        }
        out << "\n";
    }
    for (list<Equation>::iterator it = eqs->begin(); it != eqs->end(); ++it) {
        out << "e " << *it << "\n";
    }
    return out;
}

int main()
{
    int size;
    cin >> size;
    int as = size / 16;
    as = as == 0 ? 1 : as;
    unsigned int* a = new unsigned int[as];
    for (int i = 0; i < as; i++) {
        cin >> hex >> a[i];
    }

    EquationSystem es(a, size);
    list<string> results;
    es.solve(results);

    // sort(results.begin(), results.end(), strcmp);

    for (list<string>::iterator it = results.begin(); it != results.end(); ++it) {
        cout << *it << "\n";
    }
    cout << cnt << "\n";
}
