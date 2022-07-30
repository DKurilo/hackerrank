const wordSubsetsEx = () => {
  const wordSubsets = (words1: string[], words2: string[]): string[] => {
    const patternMap: Record<string, number> = {};
    const mkMap = (cs: string): Record<string, number> => {
      const m: Record<string, number> = {};
      for (let i = 0; i < cs.length; i += 1) {
        if (m[cs[i]] !== undefined) {
          m[cs[i]] += 1;
        } else {
          m[cs[i]] = 1;
        }
      }
      return m;
    };
    const addToPattern = (m: Record<string, number>): void => {
      Object.keys(m).forEach((c) => {
        if (patternMap[c] === undefined || patternMap[c] < m[c]) {
          patternMap[c] = m[c];
        }
      });
    };
    words2.forEach((cs) => addToPattern(mkMap(cs)));
    const patternKeys = Object.keys(patternMap);
    const compareToPattern = (m: Record<string, number>): boolean => {
      for (let i = 0; i < patternKeys.length; i += 1) {
        if (
          m[patternKeys[i]] === undefined ||
          m[patternKeys[i]] < patternMap[patternKeys[i]]
        ) {
          return false;
        }
      }
      return true;
    };

    return words1.filter((cs) => compareToPattern(mkMap(cs)));
  };

  console.log(wordSubsets(['warrior', 'world'], ['wrr']), ['warrior']);

  console.log(
    wordSubsets(
      ['amazon', 'apple', 'facebook', 'google', 'leetcode'],
      ['e', 'o'],
    ),
    ['facebook', 'google', 'leetcode'],
  );

  console.log(
    wordSubsets(
      ['amazon', 'apple', 'facebook', 'google', 'leetcode'],
      ['l', 'e'],
    ),
    ['apple', 'google', 'leetcode'],
  );
};

wordSubsetsEx();
