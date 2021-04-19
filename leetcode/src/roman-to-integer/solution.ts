const romanDigitsM: Record<string, number> = {
  I: 1,
  V: 5,
  X: 10,
  L: 50,
  C: 100,
  D: 500,
  M: 1000,
};

function romanToInt(s: string): number {
  return s
    .split('')
    .reverse()
    .reduce(
      ({ n, last }, ds) => {
        const d = romanDigitsM[ds] ? romanDigitsM[ds] : 0;
        return { n: d < last ? n - d : n + d, last: d };
      },
      { n: 0, last: 0 },
    ).n;
}

console.log(romanToInt('III'), 3);
console.log(romanToInt('IV'), 4);
console.log(romanToInt('IX'), 9);
console.log(romanToInt('LVIII'), 58);
console.log(romanToInt('MCMXCIV'), 1994);
console.log(romanToInt('MMMCMXCIX'), 3999);
