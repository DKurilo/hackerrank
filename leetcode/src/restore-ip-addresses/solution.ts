const restoreIpAddressesEx = () => {
  const restoreIpAddressesReq = (s: string): string[] => {
    const res: string[] = [];
    const ps: number[] = [];
    const parseNumber = (i: number): void => {
      if (i >= s.length) {
        if (ps.length === 4) {
          res.push(ps.join('.'));
        }
        return;
      }
      if (ps.length >= 4) {
        return;
      }
      if (s[i] === '0') {
        ps.push(0);
        parseNumber(i + 1);
        ps.pop();
        return;
      }
      for (let j = i + 1; j < Math.min(s.length + 1, i + 4); j += 1) {
        const p = parseInt(s.slice(i, j), 10);
        if (p <= 255) {
          ps.push(p);
          parseNumber(j);
          ps.pop();
        }
      }
    };
    parseNumber(0);
    return res;
  };

  // no recursion
  const restoreIpAddresses = (s: string): string[] => {
    const res: string[] = [];
    const ps: number[] = [];
    const is: number[] = [0];
    while (is.length > 0) {
      const i = is[is.length - 1];
      if (i >= s.length && ps.length === 4 && ps[ps.length - 1] <= 255) {
        res.push(ps.join('.'));
      }
      let last: number | null = null;
      if (
        ps.length === 4 ||
        (ps.length > 0 && ps[ps.length - 1] > 255) ||
        i >= s.length
      ) {
        while (
          (ps.length > 0 &&
            (ps[ps.length - 1] >= 100 || ps[ps.length - 1] === 0)) ||
          is[is.length - 1] >= s.length
        ) {
          is.pop();
          ps.pop();
        }
        is.pop();
        last = ps.pop();
      }
      const i1 = is[is.length - 1];
      if (is.length > 0) {
        if (last === null) {
          const n = parseInt(s.slice(i1, i1 + 1), 10);
          is.push(i1 + 1);
          ps.push(n);
        } else if (last < 10) {
          const n = parseInt(s.slice(i1, i1 + 2), 10);
          is.push(i1 + 2);
          ps.push(n);
        } else if (last > 0 && last < 100) {
          const n = parseInt(s.slice(i1, i1 + 3), 10);
          is.push(i1 + 3);
          ps.push(n);
        }
      }
    }
    return res;
  };

  console.log(restoreIpAddresses('25525511135'), [
    '255.255.11.135',
    '255.255.111.35',
  ]);

  console.log(restoreIpAddresses('0000'), ['0.0.0.0']);

  console.log(restoreIpAddresses('101023'), [
    '1.0.10.23',
    '1.0.102.3',
    '10.1.0.23',
    '10.10.2.3',
    '101.0.2.3',
  ]);

  console.log(restoreIpAddresses('172162541'), [
    '17.216.25.41',
    '17.216.254.1',
    '172.16.25.41',
    '172.16.254.1',
    '172.162.5.41',
    '172.162.54.1',
  ]);
};

restoreIpAddressesEx();
