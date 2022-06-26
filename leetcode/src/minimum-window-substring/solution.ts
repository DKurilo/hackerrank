const minimumWindowSubstring = () => {
  const isFound = (t: string, hash: Record<string, number>): boolean => {
    for (let i = 0; i < t.length; i += 1) {
      if (hash[t[i]] > 0) {
        return false;
      }
    }
    return true;
  };

  const minWindow = (s: string, t: string): string => {
    const hash: Record<string, number> = {};
    for (let i = 0; i < t.length; i += 1) {
      if (hash[t[i]] === undefined) {
        hash[t[i]] = 0;
      }
      hash[t[i]] += 1;
    }
    let minStart = -1;
    let minEnd = -1;
    let start = -1;
    let found = false;
    for (let i = 0; i < s.length; i += 1) {
      if (s[i] in hash) {
        if (start === -1) {
          start = i;
        }
        hash[s[i]] -= 1;
        while (!(s[start] in hash) || hash[s[start]] < 0) {
          if (s[start] in hash) {
            hash[s[start]] += 1;
          }
          start += 1;
        }
        if (found || isFound(t, hash)) {
          found = true;
          if (minStart === -1 || minEnd - minStart > i - start) {
            minStart = start;
            minEnd = i;
          }
        }
      }
    }
    if (minStart >= 0) {
      return s.slice(minStart, minEnd + 1);
    }
    return '';
  };

  console.log(minWindow('ADOBECODEBANC', 'ABC'), 'BANC');

  console.log(minWindow('a', 'a'), 'a');

  console.log(minWindow('a', 'aa'), '');

  console.log(minWindow('bba', 'ab'), 'ba');

  console.log(minWindow('acbbaca', 'aba'), 'baca');

  console.log(minWindow('cabwefgewcwaefgcf', 'cae'), 'cwae');

  console.log(minWindow('aabccccabcb', 'ac'), 'ca');

  console.log(
    minWindow(
      'baaccacaccacbcaabbccbbccaababcaccbccbbabcbc',
      'bacbcacccbccbabcbbacbcbc',
    ) === 'ccbbccaababcaccbccbbabcbc',
  );

  console.log(
    minWindow(
      'wegdtzwabazduwwdysdetrrctotpcepalxdewzezbfewbabbseinxbqqplitpxtcwwhuyntbtzxwzyaufihclztckdwccpeyonumbpnuonsnnsjscrvpsqsftohvfnvtbphcgxyumqjzltspmphefzjypsvugqqjhzlnylhkdqmolggxvneaopadivzqnpzurmhpxqcaiqruwztroxtcnvhxqgndyozpcigzykbiaucyvwrjvknifufxducbkbsmlanllpunlyohwfsssiazeixhebipfcdqdrcqiwftutcrbxjthlulvttcvdtaiwqlnsdvqkrngvghupcbcwnaqiclnvnvtfihylcqwvderjllannflchdklqxidvbjdijrnbpkftbqgpttcagghkqucpcgmfrqqajdbynitrbzgwukyaqhmibpzfxmkoeaqnftnvegohfudbgbbyiqglhhqevcszdkokdbhjjvqqrvrxyvvgldtuljygmsircydhalrlgjeyfvxdstmfyhzjrxsfpcytabdcmwqvhuvmpssingpmnpvgmpletjzunewbamwiirwymqizwxlmojsbaehupiocnmenbcxjwujimthjtvvhenkettylcoppdveeycpuybekulvpgqzmgjrbdrmficwlxarxegrejvrejmvrfuenexojqdqyfmjeoacvjvzsrqycfuvmozzuypfpsvnzjxeazgvibubunzyuvugmvhguyojrlysvxwxxesfioiebidxdzfpumyon',
      'ozgzyywxvtublcl',
    ) === 'tcnvhxqgndyozpcigzykbiaucyvwrjvknifufxducbkbsmlanl',
  );
};

minimumWindowSubstring();
