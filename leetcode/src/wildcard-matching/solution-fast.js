// I really hope nobody uses such challenges during whiteboard interview.
// I solved regular expressions exercises earlier, for example:
// https://github.com/DKurilo/hackerrank/blob/master/count-strings/solution.hs
// But I was need to open Wiki to remmeber how it works, just because algorithm is a bit complex
// and it's difficult to memorize it and even more difficult to build it from scratch.
// Primitive solution with backtracking works fine here, but very slow.
// Very strightforward and simplified implementation of regular expressions
// Without any optimization except ** to *.
// Information:
// https://en.wikipedia.org/wiki/Regular_expression#Implementations_and_running_times
// https://en.wikipedia.org/wiki/Glushkov%27s_construction_algorithm - I didn't use it here, but it's interesting
// https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
// https://en.wikipedia.org/wiki/Powerset_construction
// https://en.wikipedia.org/wiki/Deterministic_finite_automaton
// So first we are building NFA from pattern (buildNFA), then DFA from NFA (transformNFAtoDFA)
// and then applying it to given string (runDFA)
// It's easy to use it for morecomplex finite languages and it's easy to add pattern like 'a*' (zero or more 'a')
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var __read = (this && this.__read) || function (o, n) {
    var m = typeof Symbol === "function" && o[Symbol.iterator];
    if (!m) return o;
    var i = m.call(o), r, ar = [], e;
    try {
        while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
    }
    catch (error) { e = { error: error }; }
    finally {
        try {
            if (r && !r.done && (m = i["return"])) m.call(i);
        }
        finally { if (e) throw e.error; }
    }
    return ar;
};
var __spreadArray = (this && this.__spreadArray) || function (to, from) {
    for (var i = 0, il = from.length, j = to.length; i < il; i++, j++)
        to[j] = from[i];
    return to;
};
var optimizePatternA = function (p) {
    return p.reduce(function (o, c) { return (c === '*' && o.c === '*' ? o : { p: o.p.concat([c]), c: c }); }, {
        p: [],
        c: ''
    }).p;
};
// epsilon - transition without consuming string
var e = Symbol('e');
// our alphabet
var abc = [
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
];
var buildNFA = function (p, ab) {
    var d = function (st, c) {
        switch (p[st]) {
            case '*': // in case of * we can move to the same state or to the next state, even for epsilon (without consume character)
                return new Set([st, st + 1]);
            case '?': // in case of ? we consume any character except epsilon (we have to consume string)
                return c === e ? new Set([]) : new Set([st + 1]);
            default:
                // first condition (c !== epsilon) is not necessary, because e is not part of string,
                // but it's easier to understand it this way. We just consume character if it's proper character
                return c !== e && c === p[st] ? new Set([st + 1]) : new Set([]);
        }
    };
    var abce = __spreadArray(__spreadArray([], __read(ab)), [e]);
    var q = new Set(__spreadArray([], __read(new Array(p.length + 1))).map(function (_, i) { return i; }));
    return {
        tag: 'NFA',
        q: q,
        abc: ab,
        d: __spreadArray([], __read(q)).reduce(function (r, st) {
            var _a;
            return (__assign(__assign({}, r), (_a = {}, _a[st] = abce.reduce(function (rst, c) {
                var _a;
                return (__assign(__assign({}, rst), (_a = {}, _a[c] = d(st, c), _a)));
            }, {}), _a)));
        }, {}),
        q0: 0,
        f: new Set([p.length])
    };
};
var transformNFAtoDFA = function (nfa) {
    var nfaGetNextStates = function (st, c) {
        var nextSts = new Set([]);
        nfa.d[st][c].forEach(function (st1) {
            if (st1 !== undefined) {
                nextSts.add(st1);
            }
        });
        var done = false;
        var eSts = nextSts;
        var _loop_2 = function () {
            done = true;
            var nextESts = new Set([]);
            eSts.forEach(function (st1) {
                nfa.d[st1][e].forEach(function (st2) {
                    if (!nextSts.has(st2)) {
                        nextESts.add(st2);
                    }
                });
            });
            if (nextESts.size !== 0) {
                done = false;
                nextESts.forEach(function (st1) {
                    nextSts.add(st1);
                });
                eSts = nextESts;
            }
        };
        while (!done) {
            _loop_2();
        }
        return nextSts;
    };
    var setStToString = function (sts) {
        return __spreadArray([], __read(sts)).sort(function (a, b) { return a - b; }).join(':+:');
    };
    var isFinal = function (sts) {
        return __spreadArray([], __read(sts)).reduce(function (p, st) { return p || nfa.f.has(st); }, false);
    };
    var q0st = nfaGetNextStates(nfa.q0, e);
    q0st.add(nfa.q0);
    var q0 = setStToString(q0st);
    var q = new Set([q0]);
    var f = isFinal(q0st) ? new Set([q0]) : new Set([]);
    // to make it working faster I'm using imperative programming aproach here.
    // TypeScript is not very good in expressing some parts.
    // Strictly functional programming aproach you can check here:
    // https://github.com/DKurilo/hackerrank/blob/master/count-strings/solution.hs
    // it's solution for https://www.hackerrank.com/challenges/count-strings/problem
    var curSts = [q0st];
    var curs = [q0];
    var d = {};
    var done = false;
    while (!done) {
        done = true;
        var nextSts = [];
        var nexts = [];
        for (var i = 0; i < curs.length; i += 1) {
            var curSt = curSts[i];
            var cur = curs[i];
            var nextStatesForABC = {};
            var _loop_1 = function (k) {
                var nst = new Set([]);
                curSt.forEach(function (st) {
                    nfaGetNextStates(st, nfa.abc[k]).forEach(function (st1) { return nst.add(st1); });
                });
                var n = setStToString(nst);
                nextStatesForABC[nfa.abc[k]] = n;
                if (!q.has(n)) {
                    done = false;
                    q.add(n);
                    if (isFinal(nst)) {
                        f.add(n);
                    }
                    nextSts.push(nst);
                    nexts.push(n);
                }
            };
            for (var k = 0; k < nfa.abc.length; k += 1) {
                _loop_1(k);
            }
            d[cur] = nextStatesForABC;
        }
        curSts = nextSts;
        curs = nexts;
    }
    return {
        tag: 'DFA',
        q: q,
        abc: nfa.abc,
        d: d,
        q0: q0,
        f: f
    };
};
var compilePattern = function (p, ab) { return transformNFAtoDFA(buildNFA(p, ab)); };
var runDFA = function (dfa) { return function (s) {
    return dfa.f.has(s.reduce(function (st, c) { return dfa.d[st][c]; }, dfa.q0));
}; };
var isMatch = function (s, p) {
    return runDFA(compilePattern(optimizePatternA(p.split('')), abc))(s.split(''));
};
console.log(isMatch('aa', 'a'), false);
console.log(isMatch('a', 'a'), true);
console.log(isMatch('aa', '*'), true);
console.log(isMatch('cb', '?a'), false);
console.log(isMatch('adceb', '*a*b'), true);
console.log(isMatch('acdcb', '*a*c?b'), false);
console.log(isMatch('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab', '*a'), false);
console.log(isMatch('aaaaaaaaaaaaaaaaaaaaaa', '********b'), false);
console.log(isMatch('aaabababaaabaababbbaaaabbbbbbabbbbabbbabbaabbababab', '*ab***ba**b*b*aaab*b'), true);
console.log(isMatch('abbaabbbbababaababababbabbbaaaabbbbaaabbbabaabbbbbabbbbabbabbaaabaaaabbbbbbaaabbabbbbababbbaaabbabbabb', '***b**a*a*b***b*a*b*bbb**baa*bba**b**bb***b*a*aab*a**'), true);
console.log(isMatch('babbbbaabababaabbababaababaabbaabababbaaababbababaaaaaabbabaaaabababbabbababbbaaaababbbabbbbbbbbbbaabbb', 'b*a*b*a*b*a'), false);
console.log(isMatch('babbbbaabababaabbababaababaabbaabababbaaababbababaaaaaabbabaaaabababbabbababbbaaaababbbabbbbbbbbbbaabbb', 'b**bb**a**bba*b**a*bbb**aba***babbb*aa****aabb*bbb***a'), false);
console.log(isMatch('baaaababbbaabbbbabbababaababaaabaababbbaabaabbbbaabbbbbbaabaabbababaaaaaaaabbbaaabbbababbbbbabbbbabbbbabbaabaababababbbababbbbbbbaaaaabbbbabbbbbaabbbaaaabaaabbabbabaabbbbabbbabbbaababbabbaaaababbababa', '*bb*abb*a**ba**aba*b*bbb*abbaaa*bb*b**a*b*b**a**abb***ab*b**b*b*a******a*a*babaa*bab*a*b****bb*babb*baa'), false);
