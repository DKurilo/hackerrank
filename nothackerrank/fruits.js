// https://medium.com/javascript-in-plain-english/interviewing-at-google-javascript-assessment-questions-f9bf0c0df157

// Utils.
const generateWord = k => l => 'a'.repeat(l).split('').map(_ => k[Math.round(Math.random() * k.length + .5)]).join('');

// Exercise 1.
const collectFruits = bins => trees => {
    let btypes = new Set(), fruits = [], collected = 0;
    for (let i = 0; i < trees.length; i++) {
        if (btypes.size < bins) {
            btypes.add(trees[i]);
        }
        if (!btypes.has(trees[i])) {
            collected = Math.max(collected, fruits.length);
            btypes = new Set();
            btypes.add(trees[i]);
            for (let k = fruits.length - 1; k >= 0; k--) {
                if (!btypes.has(fruits[k])) {
                    if (btypes.size < bins) {
                        btypes.add(fruits[k]);
                    } else {
                        fruits = fruits.slice(k + 1);
                        break;
                    }
                }
            }
        }
        fruits.push(trees[i]);
    }
    return Math.max(collected, fruits.length);
};

const hugeGarden = [];
for (let i = 0; i < 4000; i++){
    hugeGarden.push(Math.round(Math.random() * 100 + .5));
}
console.log([[], [1], [1,2,1], [0,1,2,2], [1,2,3,2,2], [3,3,3,1,2,1,1,2,3,3,4], hugeGarden].map(collectFruits(2)));


// Exercise 2.
const timeToType = keyboard => word => word.split('')
                                           .map(c => keyboard.indexOf(c))
                                           .reduce((o, c) => ({lc: c, t: o.t + Math.abs(c - o.lc)}), {lc: 0, t: 0}).t;

const myKeyboard = 'abcdefghijklmnopqrstuvwxyz', myKeyboardTimeToType = timeToType(myKeyboard);
const hugeWord = generateWord(myKeyboard)(4000)
console.log(myKeyboardTimeToType('cba'), myKeyboardTimeToType(''), myKeyboardTimeToType(hugeWord));

//Exercise 3.
const reformatPlate = k => plate => {
    if (k == 0) {
        return plate;
    }
    const plateArr = plate.split('-').join('').split('');
    const fg = plateArr.length % k;
    return plateArr.reduce((r, c, i) => {
        r.push(c.toUpperCase());
        if ((i - fg + 1) % k == 0 && i < plateArr.length - 1) {
            r.push('-');
        }
        return r;
    },[]).join('');
}

const plateChars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-';
const hugePlate = generateWord(plateChars)(12000);
console.log(
    reformatPlate(4)('9B2A-2c-9-y'),
    reformatPlate(2)('2-7g-6-E'),
    reformatPlate(1)('9B2A-2c-9-y'),
    reformatPlate(2)('9B2A-2c-9-y'),
    reformatPlate(10)(hugePlate).length,
    reformatPlate(13)(hugePlate).length
);

// https://medium.com/javascript-in-plain-english/microsoft-online-assessment-questions-js-f68ecdb6e927

// Exercise 1.
const removeConsecutive = k => s => s.split('').reduce((o, c) => ({
                                                                      s: o.s + ((c === o.c && o.l >= k - 1) ? '' : c),
                                                                      c: c,
                                                                      l: c === o.c ? (o.l + 1) : 1
                                                                  }), {s: '', c: '', l: 0}).s;

const removeConsecutive3 = removeConsecutive(3);
const hugeString = generateWord(myKeyboard)(200000);
console.log(
    removeConsecutive3('eedaaad'),
    removeConsecutive3('xxxtxxx'),
    removeConsecutive3('uuuuxaaaaxuuu'),
    removeConsecutive3(hugeString).length
);

// Exercise 2.
const digitsSum = d => (d < 10 ? d : (d % 10 + digitsSum(Math.round(d / 10))));
const sumOfMax = ns => {
    let m1, m2;
    for (let i = 0; i < ns.length; i++) {
        if (m2 === undefined) {
            m2 = ns[i];
        } else if (m2 < ns[i]) {
            m1 = m2;
            m2 = ns[i];
        } else if (m1 === undefined || m1 < ns[i]) {
            m1 = ns[i];
        }
    }
    return m1 !== undefined && m2 !== undefined ? m1 + m2 : -1;
};
// it's possible just to sort array and to get two first members. like:
// const sumOfMax = ns => ns.length < 2 ? -1 : ns.sort((a, b) => b - a).slice(0, 2).reduce((s, x) => s + x, 0);
// but then you need O(n*log(n)) time.
const maxSum = ns => {
    const ps = ns.reduce(
        (o, n) => {
            const ds = digitsSum(n);
            const arr = o[ds];
            if (arr) {
                arr.push(n);
            } else {
                o[ds] = [n];
            }
            return o;
        }, {}
    );
    return Math.max(...Object.keys(ps).map(i => sumOfMax(ps[i])));
};

const hugeArray = 'a'.repeat(200000).split('').map(_ => Math.round(Math.random() * 1000000000 +  .5));
console.log(maxSum([51, 71, 17, 42]), maxSum([42, 33, 60]), maxSum([51, 32, 43]), maxSum(hugeArray));

// Exercise 3.
const notChar = c => c == 'a' ? 'b' :'a';
// uncomment to see what was changed
const replaceConsecutive = s => (s + '$').split('').reduce(
    (o, c) => {
        if (c !== o.c) {
            if (o.buf.length === 3) {
                // o.s += o.buf[0] + c + o.buf[2];
                o.m++;
            }/* else {
                o.s += o.buf;
            }*/
            o.buf = c;
            o.c = c;
        } else if (o.buf.length == 3) {
            // o.s += o.buf[0] + o.buf[1] + notChar(o.buf[2]);
            o.buf = c;
            o.m++;
        } else {
            o.buf += c;
        }
        return o;
    },
    {/*s: '',*/buf: '', c: '', m: 0}
).m;

const hugeAB = generateWord('ab')(200000);
console.log(
    replaceConsecutive('baaaaa'),
    replaceConsecutive('baaabbaabbba'),
    replaceConsecutive('baabab'),
    replaceConsecutive('baaabbbbbbaabbba'),
    replaceConsecutive(hugeAB)
);
