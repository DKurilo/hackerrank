// It's very slow and inefficient solution.
// Just wanted to play strings and things.

const strToIntArr = (s: string): number[] =>
  s
    .split('')
    .map((c) => parseInt(c, 10))
    .reduce(
      ({ ns, isZero }, n) =>
        isZero && n === 0
          ? { ns, isZero }
          : { ns: ns.concat(n), isZero: false },
      { ns: [], isZero: true },
    ).ns;

const add = (cs1: string, cs2: string): string => {
  const len = Math.max(cs1.length, cs2.length);
  const ns1 = strToIntArr(cs1).reverse();
  const ns2 = strToIntArr(cs2).reverse();
  const st = [...new Array(len)]
    .map((_, i) => i)
    .reduce(
      ({ sum, t }, i) => {
        const s =
          (i < ns1.length ? ns1[i] : 0) + (i < ns2.length ? ns2[i] : 0) + t;
        return { sum: `${s % 10}${sum}`, t: Math.trunc(s / 10) };
      },
      { sum: '', t: 0 },
    );
  return st.t === 0 ? st.sum : `${st.t}${st.sum}`;
};

const multiply = (num1: string, num2: string): string => {
  const ns1 = strToIntArr(num1).reverse();
  const ns2 = strToIntArr(num2).reverse();
  const ns: string[] = ns2.reduce(
    (
      { ps, sn }: { ps: string[]; sn: number },
      n2: number,
    ): { ps: string[]; sn: number } => ({
      ps: ps.concat(
        n2 === 0
          ? '0'
          : (({ t, v }) =>
              t === 0 && v === ''
                ? '0'
                : `${t > 0 ? t.toString() : ''}${v}${'0'.repeat(sn)}`)(
              ns1.reduce(
                ({ t, v }, n1) => {
                  const p = n1 * n2 + t;
                  return { t: Math.trunc(p / 10), v: `${p % 10}${v}` };
                },
                { t: 0, v: '' },
              ),
            ),
      ),
      sn: sn + 1,
    }),
    { ps: [], sn: 0 },
  ).ps;
  return ns.reduce(add, '0');
};

console.log(strToIntArr('000234'));
console.log(strToIntArr('234000'));
console.log(strToIntArr('000234000'));
console.log(add('49200', '6150'));
console.log(multiply('2', '3'), '6');
console.log(multiply('123', '456'), '56088');
console.log(multiply('000123', '000456'), '56088');
console.log(multiply('42423', '0'), '0');
console.log(multiply('42423', '0000'), '0');
console.log(multiply('0', '43543'), '0');
console.log(multiply('10000', '43543'), '435430000');
console.log(multiply('42423', '10000'), '424230000');
console.log(multiply('0', '43543'), '0');
