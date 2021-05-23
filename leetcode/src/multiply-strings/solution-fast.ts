const strToIntArr = (s: string): number[] =>
  s.split('').map((c) => parseInt(c, 10));

const multiply = (num1: string, num2: string): string => {
  const ns1 = strToIntArr(num1);
  const ns2 = strToIntArr(num2);
  const ps = [...new Array(ns1.length + ns2.length)].map(() => 0);
  for (let i = ns1.length - 1; i >= 0; i -= 1) {
    for (let j = ns2.length - 1; j >= 0; j -= 1) {
      const s = ns1[i] * ns2[j] + ps[i + j + 1];
      ps[i + j] = ps[i + j] + Math.trunc(s / 10);
      ps[i + j + 1] = s % 10;
    }
  }
  const p = ps.reduce((s, n) => (s === '' && n === 0 ? s : `${s}${n}`), '');
  return p === '' ? '0' : p;
};

console.log(multiply('2', '3'), '6');
console.log(multiply('123', '456'), '56088');
console.log(multiply('000123', '000456'), '56088');
console.log(multiply('42423', '0'), '0');
console.log(multiply('42423', '0000'), '0');
console.log(multiply('0', '43543'), '0');
console.log(multiply('10000', '43543'), '435430000');
console.log(multiply('42423', '10000'), '424230000');
console.log(multiply('0', '43543'), '0');
