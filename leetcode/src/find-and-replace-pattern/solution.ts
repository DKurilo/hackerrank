const findAndReplacePatternEx = () => {
  const findAndReplacePattern = (
    words: string[],
    pattern: string,
  ): string[] => {
    const tokenize = (cs: string): [number, number][] => {
      const map: Record<string, number> = {};
      const res: [number, number][] = [];
      let k = 0;
      for (let i = 0; i < cs.length; i += 1) {
        if (res.length === 0 || res[res.length - 1][0] !== map[cs[i]]) {
          let token;
          if (map[cs[i]] !== undefined) {
            token = map[cs[i]];
          } else {
            token = k;
            map[cs[i]] = k;
            k += 1;
          }
          res.push([token, 1]);
        } else {
          res[res.length - 1][1] += 1;
        }
      }
      return res;
    };

    const tokenizedPattern = tokenize(pattern);

    const compareToPattern = (p1: [number, number][]): boolean => {
      if (p1.length !== tokenizedPattern.length) {
        return false;
      }
      for (let i = 0; i < p1.length; i += 1) {
        if (
          p1[i][0] !== tokenizedPattern[i][0] ||
          p1[i][1] !== tokenizedPattern[i][1]
        ) {
          return false;
        }
      }
      return true;
    };

    return words.filter((cs) => compareToPattern(tokenize(cs)));
  };

  console.log(
    findAndReplacePattern(['abc', 'deq', 'mee', 'aqq', 'dkd', 'ccc'], 'abb'),
    ['mee', 'aqq'],
  );

  console.log(findAndReplacePattern(['a', 'b', 'c'], 'a'), ['a', 'b', 'c']);

  console.log(
    findAndReplacePattern(['abc', 'cba', 'xyx', 'yxx', 'yyx'], 'abc'),
    ['abc', 'cba'],
  );
};

findAndReplacePatternEx();
