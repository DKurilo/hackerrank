const minimumDeletionsToMakeCharacterFrequenciesUnique = () => {
  const minDeletions = (s: string): number => {
    const freq: Record<string, number> = {};
    for (let i = 0; i < s.length; i += 1) {
      if (freq[s[i]] === undefined) {
        freq[s[i]] = 0;
      }
      freq[s[i]] += 1;
    }
    let replacements = 0;
    const seen = new Set();
    Object.keys(freq).forEach((k: string): void => {
      while (freq[k] > 0 && seen.has(freq[k])) {
        freq[k] -= 1;
        replacements += 1;
      }
      seen.add(freq[k]);
    });
    return replacements;
  };

  console.log(minDeletions('aab'), 0);

  console.log(minDeletions('aaabbbcc'), 2);

  console.log(minDeletions('ceabaacb'), 2);

  console.log(minDeletions('nhgffkjjlnglnegfmcmjamdgjdf'), 17);
};

minimumDeletionsToMakeCharacterFrequenciesUnique();
