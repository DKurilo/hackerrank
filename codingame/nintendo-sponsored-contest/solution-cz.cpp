/*
 * Solution is based on this great set of articles.
 * Thanks to Firas Kraïem for them. I hope this person feel good now. For some reason they blog is not working anymore.
 * https://web.archive.org/web/20200301213437/https://blog.fkraiem.org/2013/04/09/polynomial-factorisation-over-finite-fields-part-0-overview/
 * https://web.archive.org/web/20200301213438/https://blog.fkraiem.org/2013/11/30/polynomial-factorisation-over-finite-fields-part-1-squarefree-factorisation/
 * https://web.archive.org/web/20200301213400/https://blog.fkraiem.org/2013/11/30/polynomial-factorisation-over-finite-fields-part-2-distinct-degree-factorisation/
 * https://web.archive.org/web/20200301213349/https://blog.fkraiem.org/2013/12/01/polynomial-factorisation-over-finite-fields-part-3-final-splitting-cantor-zassenhaus-in-odd-characteristic/
 */
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include <random>

using namespace std;

int random(int min_n, int max_n)
{
    std::random_device dev;
    std::mt19937 rng(dev());
    std::uniform_int_distribution<std::mt19937::result_type> dist6(min_n, max_n);
    return dist6(rng);
}

int ipow(int base, int exp)
{
    int result = 1;
    for (;;)
    {
        if (exp & 1)
            result *= base;
        exp >>= 1;
        if (!exp)
            break;
        base *= base;
    }

    return result;
}

class Polynomial : public vector<int>
{
    public:
        Polynomial() : vector<int>(){};
        Polynomial(Polynomial::iterator first, Polynomial::iterator second) : vector<int>(first, second){};
        void reduce()
        {
            if (this->size() == 0 || this->at(0)) {
                return;
            }
            Polynomial::iterator last = this->begin();
            while (last != this->end() && *last == 0) {
                last++;
            }
            this->erase(this->begin(), last);
        }

        static pair<Polynomial, Polynomial> divmod(Polynomial lhs, Polynomial rhs)
        {
            Polynomial div;
            lhs.reduce();
            rhs.reduce();
            if (rhs.size() > lhs.size()) {
                return pair<Polynomial, Polynomial>(div, lhs);
            }
            Polynomial::iterator cp = lhs.begin() + rhs.size();
            Polynomial current (lhs.begin(), cp);
            while (current.size() >= rhs.size()) {
                if (*current.begin() >= *rhs.begin()) {
                    div.push_back(1);
                    current = current + rhs;
                } else {
                    div.push_back(0);
                }
                current.erase(current.begin());
                if (cp != lhs.end()) {
                    current.push_back(*cp);
                    cp++;
                }
            }
            div.reduce();
            current.reduce();
            return pair<Polynomial, Polynomial>(div, current);
        }

        static Polynomial gcd(Polynomial p1, Polynomial p2)
        {
            p1.reduce();
            p2.reduce();

            while (p2.size() > 1) {
                pair<Polynomial, Polynomial> dm = Polynomial::divmod(p1, p2);
                p1 = p2;
                p2 = dm.second;
            }

            if (p2.size() == 0) {
                return p1;
            }

            Polynomial res;
            res.push_back(1);
            return res;
        }

        static Polynomial random_element(int min_degree, int max_degree)
        {
            int degree = random(min_degree, max_degree);
            Polynomial p;
            for (int i = 0; i < degree; i++) {
                int k = random(0, 1);
                p.push_back(k);
            }
            return p;
        }

    friend ostream & operator<< (ostream &out, Polynomial &p)
    {
        int k = p.size() - 1;
        bool printed = false;
        for (Polynomial::iterator it = p.begin(); it != p.end(); ++it) {
            if (*it) {
                if (printed) {
                    out << "+";
                }
                if (k == 0) {
                    out << "1";
                } else if (k == 1) {
                    out << "x";
                } else {
                    out << "x^" << k;
                }
                printed = true;
            }
            k--;
        }
        if (!printed) {
            out << "0";
        }
        return out;
    }

    friend bool operator>=(Polynomial lhs, Polynomial rhs)
    {
        rhs.reduce();
        lhs.reduce();
        if (lhs.size() > rhs.size()) {
            return true;
        } else if (lhs.size() < rhs.size()) {
            return false;
        }
        Polynomial::iterator lit = lhs.begin();
        for (Polynomial::iterator rit = rhs.begin(); rit != rhs.end(); ++rit) {
            if (*lit != *rit) {
                return *lit >= *rit;
            }
            lit++;
        }
        return true;
    }

    friend Polynomial operator+(const Polynomial &lhs, const Polynomial &rhs)
    {
        Polynomial res = lhs.size() >= rhs.size() ? lhs : rhs;
        const Polynomial *to_add = lhs.size() < rhs.size() ? &lhs : &rhs;
        for (int i = 1; i <= to_add->size(); ++i) {
            int k = res.size() - i;
            res[k] = res[k] != (*to_add)[to_add->size() - i];
        }
        return res;
    }

    friend Polynomial operator*(const Polynomial &lhs, const Polynomial &rhs)
    {
        int lmax = lhs.size() - 1;
        int rmax = rhs.size() - 1;
        int size = lhs.size() + rhs.size() - 1;
        Polynomial res;
        for (int i = 0; i < size; ++i) {
            res.push_back(0);
            int jb = i < lmax ? i : lmax;
            int je = i - rmax;
            if (je < 0) {
                je = 0;
            }
            int kb = i - lmax;
            if (kb < 0) {
                kb = 0;
            }
            for (int j = jb; j >= je; --j) {
                if (lhs[j] == 1) {
                    int k = jb - j + kb;
                    res[i] = res[i] != rhs[k];
                }
            }
        }
        return res;
    }

    friend Polynomial operator/(Polynomial lhs, Polynomial rhs)
    {
        pair<Polynomial, Polynomial> dm = Polynomial::divmod(lhs, rhs);
        return dm.first;
    }

    friend Polynomial operator%(Polynomial lhs, Polynomial rhs)
    {
        pair<Polynomial, Polynomial> dm = Polynomial::divmod(lhs, rhs);
        return dm.second;
    }

    Polynomial derivative() const
    {
        int k = this->size() - 1;
        Polynomial der;
        for (Polynomial::const_iterator it = this->begin(); it != this->end() && k > 0; ++it) {
            if (*it) {
              der.push_back(k % 2);
            } else {
                der.push_back(0);
            }
            k--;
        }
        der.reduce();
        return der;
    }
    
    int degree()
    {
        this->reduce();
        if (this->size() == 0) {
            return 0;
        }
        return this->size() - 1;
    }
};

Polynomial make_polynomial(const int size, const unsigned int *a)
{
    Polynomial p;
    for (int i = 2 * size - 1; i >= 0; --i) {
        bool r = (a[i / 32] >> (i % 32)) & 0x1;
        p.push_back(r);
    }
    p.reduce();
    return p;
}

class Factors : public vector<Polynomial> {};

ostream & operator << (ostream &out, Factors &fs)
{
    out << "(";
    for (Factors::iterator it = fs.begin(); it != fs.end(); ++it) {
        if (it != fs.begin()) {
            out << ", ";
        }
        out << *it;
    }
    out << ")";
    return out;
}

class ConvFactor : public pair<Polynomial, int> {
    public:
        ConvFactor(Polynomial p, int n) : pair<Polynomial, int>(p, n){};
};

ostream & operator << (ostream &out, ConvFactor &cf)
{
    out << "(" << cf.first << ", " << cf.second << ")";
    return out;
}

class ConvFactors : public vector<ConvFactor> {};

ostream & operator << (ostream &out, ConvFactors &cfs)
{
    out << "[" << endl;
    for (ConvFactors::iterator it = cfs.begin(); it != cfs.end(); ++it) {
        if (it != cfs.begin()) {
            out << "," << endl;
        }
        out << (*it);
    }
    out << endl << "]";
    return out;
}

ConvFactors squarefree_helper(const Polynomial p, const int pmult)
{
    ConvFactors fs;
    Polynomial der_p = p.derivative();
    Polynomial tk = Polynomial::gcd(p, der_p);
    Polynomial vk = p / tk;
    int k = 1;
    Polynomial temp = p;
    while (vk.degree() > 0) {
        Polynomial vkPlus1;
        if (k % 2 != 0) {
            vkPlus1 = Polynomial::gcd(tk, vk);
            Polynomial f = vk / vkPlus1;
            if (f.size() != 1) {
                fs.push_back(ConvFactor(f, ipow(2, pmult) * k));
            }
        } else {
            vkPlus1 = vk;
        }
        tk = tk / vkPlus1;
        vk = vkPlus1;
        k++;
    }
    if (tk.size() <= 1) {
        return fs;
    }
    Polynomial newP;
    for (int i = tk.degree() / 2; i >= 0; --i) {
        newP.push_back(tk[2 * i]);
    }
    ConvFactors fs1 = squarefree_helper(newP, pmult + 1);
    for (ConvFactors::iterator it = fs1.begin(); it != fs1.end(); ++it) {
        fs.push_back(*it);
    }
    return fs;
}

ConvFactors squarefree(const Polynomial p) 
{
    return squarefree_helper(p, 0);
}

ConvFactors distinct_degree_factors(Polynomial p)
{
    ConvFactors fs;
    // pol = x^2
    Polynomial pol;
    pol.push_back(1);
    pol.push_back(0);
    pol.push_back(0);
    int d = 1;
    while (p.degree() > 0) {
        Polynomial polx = pol;
        // polx = pol - x;
        polx[polx.size() - 2] = !polx[polx.size() - 2];
        Polynomial t = Polynomial::gcd(polx, p);
        if (t.degree() != 0) {
            fs.push_back(ConvFactor(t, d));
        }
        p = p / t;
        d++;
        pol = (pol * pol) % p;
    }
    return fs;
}

Factors cz2(Polynomial p, int d)
{
    Factors fs;
    if (p.degree() == d) {
        fs.push_back(p);
        return fs;
    }
    while (true) {
        Polynomial t = Polynomial::random_element(1, d * 2 - 1);
        Polynomial w = t;
        for (int i = 0; i < d - 1; ++i) {
            t = (t * t) % p;
            w = w + t;
        }
        Polynomial u = Polynomial::gcd(p, w);
        if (u.degree() > 0 && u.degree() < p.degree()) {
            fs = cz2(u, d);
            Polynomial pu = p / u;
            Factors fs1 = cz2(pu, d);
            for (int i = 0; i < fs1.size(); ++i) {
                fs.push_back(fs1[i]);
            }
            return fs;
        }
    }
}

Factors factorize(const Polynomial p)
{
    Factors factors;
    ConvFactors sqfree = squarefree(p);
    for (ConvFactors::iterator it = sqfree.begin(); it != sqfree.end(); ++it) {
        ConvFactors ddfs = distinct_degree_factors(it->first);
        for (ConvFactors::iterator jt = ddfs.begin(); jt != ddfs.end(); ++jt) {
            Factors fs = cz2(jt->first, jt->second);
            for (Factors::iterator kt = fs.begin(); kt != fs.end(); ++kt) {
                for (int i = 0; i < it->second; ++i) {
                    factors.push_back(*kt);
                }
            }
        }
    }
    return factors;
}

string polynomial2string(Polynomial p, int max_degree)
{
    int size = (max_degree + max_degree % 32) / 32;
    unsigned int* a = new unsigned int[size];
    for (int i = 0; i < size; ++i) {
        a[i] = 0;
    }
    int i = 0;
    for (Polynomial::reverse_iterator it = p.rbegin(); it != p.rend(); ++it) {
        if (*it) {
            a[i / 32] ^= 1 << (i % 32);
        }
        i++;
    }
    stringstream s;
    for (int i = 0; i < size; ++i) {
        if (i > 0) {
            s << " ";
        }
        s << setfill('0') << setw(8) << hex << a[i];
    }
    return s.str();
}

vector<Polynomial> combinations(vector<Polynomial> elems, int k)
{
    vector<Polynomial> res;
    if (k == 0) {
        Polynomial trivial;
        trivial.push_back(1);
        res.push_back(trivial);
        return res;
    }
    int i = 0;
    for (vector<Polynomial>::iterator it = elems.begin(); it != elems.end(); ++it) {
        vector<Polynomial> new_elems = elems;
        new_elems.erase(new_elems.begin() + i);
        vector<Polynomial> cmbs = combinations(new_elems, k - 1);
        for (vector<Polynomial>::iterator jt = cmbs.begin(); jt != cmbs.end(); ++jt) {
            *jt = (*jt) * (*it);
            res.push_back(*jt);
        }
        i++;
    }
    return res;
}

vector<string> make_results(const Polynomial &p, const Factors &fs, int max_degree)
{
    vector<string> results;

    for (int i = 0; i < fs.size(); ++i) {
        vector<Polynomial> cmbs = combinations(fs, i);
        for (vector<Polynomial>::iterator it = cmbs.begin(); it != cmbs.end(); ++it) {
            if (it->degree() > max_degree) {
                continue;
            }
            Polynomial second = p / (*it);
            if (second.degree() > max_degree) {
                continue;
            }
            string res = polynomial2string(*it, max_degree) + " " + polynomial2string(second, max_degree);
            results.push_back(res);
        }
    }
    return results;
}

bool rescmp(const string& a, const string& b)
{
    return a < b;
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

    Polynomial trivial;
    trivial.push_back(1);

    Polynomial polynomial = make_polynomial(size, a);
    Factors factors = factorize(polynomial);
    factors.push_back(trivial);

    vector<string> results = make_results(polynomial, factors, size);
    sort(results.begin(), results.end(), rescmp);
    vector<string>::iterator last = unique(results.begin(), results.end());
    results.erase(last, results.end());

    for (vector<string>::iterator it = results.begin(); it != results.end(); ++it) {
        cout << *it << "\n";
    }

    // Tests:
    // unsigned int* cm = new unsigned int[2];
    // cm[0] = 0x46508fb7;
    // cm[1] = 0x6677e201;
    // Polynomial pcm = make_polynomial(32, cm);
    // unsigned int* b = new unsigned int[1];
    // b[0] = 0xb0c152f9;
    // Polynomial pb = make_polynomial(16, b);
    // unsigned int* c = new unsigned int[1];
    // c[0] = 0xebf2831f;
    // Polynomial pc = make_polynomial(16, c);
    // unsigned int* d = new unsigned int[1];
    // d[0] = b[0] ^ c[0];
    // Polynomial pd = make_polynomial(16, d);
    // Polynomial s1 = pb + pc;
    // Polynomial m1 = pb * pc;
    // Polynomial s2 = pc + pb;
    // Polynomial m2 = pc * pb;
    // Polynomial d1 = m1 / pc;
    // Polynomial d2 = m1 / pb;
    // cout << "Control sum:\n" << pd << "\n";
    // cout << "Sums:\n" << s1 << "\n";
    // cout << s2 << "\n";
    // cout << "Control mul:\n" << pcm << "\n";
    // cout << "Muls:\n" << m1 << "\n";
    // cout << m2 << "\n";
    // pair<Polynomial, Polynomial> dm = Polynomial::divmod(m1, pb);
    // cout << "Control div1:\n" << pc << "\n";
    // cout << "divmod:\n" << dm.first << "\n" << dm.second << "\n";
    // cout << "Control div1:\n" << pb << "\n";
    // cout << "Div1:\n" << d1 << "\n";
    // cout << "Control div2:\n" << pc << "\n";
    // cout << "Div2:\n" << d2 << "\n";
    // pair<Polynomial, Polynomial> dm1 = Polynomial::divmod(pc, pb);
    // cout << "divmod:\n" << dm1.first << "\n" << dm1.second << "\n";
    // Polynomial gcd_pb_pc = Polynomial::gcd(pb, pc);
    // cout << "gcd1:\n" << gcd_pb_pc << "\n";
    // Polynomial gcd_m1_pb = Polynomial::gcd(m1, pb);
    // cout << "gcd2:\n" << gcd_m1_pb << "\n" << pb << "\n";
    // Polynomial gcd_m1_pc = Polynomial::gcd(m1, pc);
    // cout << "gcd3:\n" << gcd_m1_pc << "\n" << pc << "\n";
    // Polynomial gcd_pc_m1 = Polynomial::gcd(pc, m1);
    // cout << "gcd4:\n" << gcd_pc_m1 << "\n" << pc << "\n";
    // unsigned int* e = new unsigned int[1];
    // e[0] = 0xbd;
    // Polynomial pe = make_polynomial(16, e);
    // unsigned int* f = new unsigned int[1];
    // f[0] = 0xd1;
    // Polynomial pf = make_polynomial(16, f);
    // Polynomial gcd_pe_pf = Polynomial::gcd(pe, pf);
    // cout << "gcd5:\n" << gcd_pe_pf << "\n" << "x^3+1" << "\n";
    // Polynomial der_pb = pb.derivative();
    // cout << "control derivative:" << endl << "x^30+x^28+x^22+x^8+x^6+x^4+x^2" << endl;
    // cout << "derivative:\n" << der_pb << endl;

    // unsigned int* g = new unsigned int[1];
    // g[0] = 0x1b8d;
    // Polynomial pg = make_polynomial(16, g);
    // ConvFactors sqs = squarefree(pg);
    // cout << "sq factors for:" << endl << pg << endl << "are:" << endl << sqs << endl;
    // cout << "end of sq factors" << endl;
    // ConvFactors ddfacs = distinct_degree_factors(sqs[0].first);
    // cout << "dd factors should be:" << endl << "[(x + 1, 1), (x^3 + x + 1, 3)]" << endl << "we have:" << endl << ddfacs << endl;

    // unsigned int* h = new unsigned int[1];
    // h[0] = 0b10100010000101011;
    // Polynomial ph = make_polynomial(16, h);
    // Factors fs_ph = cz2(ph, 8);
    // cout << "factors for:" << endl << ph << endl << "are:" << endl << fs_ph << endl;
}
