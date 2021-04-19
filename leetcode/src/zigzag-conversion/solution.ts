function convert(s: string, numRows: number): string {
  const n = numRows + numRows - 2 || 1;
  return s
    .split('')
    .reduce(
      (zz: string[][], c: string, i: number): string[][] => {
        const pos = i % n;
        const line = pos < numRows ? pos : n - pos;
        return zz
          .slice(0, line)
          .concat(zz[line].concat([c]))
          .concat(zz.slice(line + 1));
        // this will be faster, but not functional programming way
        // surely we can change reduce to for loop and so on, it will make it faster
        // zz[line].push(c);
        // return zz;
      },
      [...new Array(numRows)].map(() => []),
    )
    .flat()
    .join('');
}

console.log(convert('PAYPALISHIRING', 3), 'PAHNAPLSIIGYIR');
console.log(convert('PAYPALISHIRING', 4), 'PINALSIGYAHRPI');
console.log(convert('PAYPALISHIRING', 1), 'PAYPALISHIRING');
console.log(convert('PAYPALISHIRING', 100), 'PAYPALISHIRING');
console.log(convert('A', 2), 'A');
console.log(convert('A', 1), 'A');
