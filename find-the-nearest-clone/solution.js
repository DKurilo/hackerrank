'use strict';

const fs = require('fs');

process.stdin.resume();
process.stdin.setEncoding('utf-8');

let inputString = '';
let currentLine = 0;

process.stdin.on('data', inputStdin => {
    inputString += inputStdin;
});

process.stdin.on('end', function() {
    inputString = inputString.replace(/\s*$/, '')
        .split('\n')
        .map(str => str.replace(/\s*$/, ''));

    main();
});

function readLine() {
    return inputString[currentLine++];
}

// Complete the findShortest function below.

/*
 * For the unweighted graph, <name>:
 *
 * 1. The number of nodes is <name>Nodes.
 * 2. The number of edges is <name>Edges.
 * 3. An edge exists between <name>From[i] to <name>To[i].
 *
 */
function findShortest(graphNodes, graphFrom, graphTo, ids, val) {
    // solve here
    const front = [], oldFront = [];
    for (let i = 0; i < graphNodes; ++i) {
        if (ids[i] === val) {
            front.push([i + 1]);
            oldFront.push([]);
        }
    }
    if (front.length < 2) {
        return -1;
    }
    const g = {}
    for (let i = 0, n = graphFrom.length; i < n; ++i) {
        const c1 = graphFrom[i];
        const c2 = graphTo[i];
        if (g[c1] === undefined) {
            g[c1] = {to: [], d: -1};
        }
        if (g[c2] === undefined) {
            g[c2] = {to: [], d: -1};
        }
        g[c1].to.push(c2);
        g[c2].to.push(c1);
    }
    const n = front.length;
    let fd = -1;
    let d = 0;
    do {
        for (let i = 0; i < n; ++i) {
            const fl = front[i].length;
            const newFront = [];
            for (let j = 0; j < fl; j++) {
                if (g[front[i][j]].d === -1) {
                    g[front[i][j]].d = d;
                    g[front[i][j]].to.filter(x => oldFront[i].indexOf(x) < 0)
                                  .forEach(x => newFront.push(x));
                } else {
                    fd = g[front[i][j]].d + d;
                    break;
                }
            }
            if (fd !== -1) {
                break;
            }
            oldFront[i] = front[i];
            front[i] = newFront;
        }
        d++;
    } while (fd < 0)

    return fd;
}

function main() {
    const ws = fs.createWriteStream(process.env.OUTPUT_PATH);

    const graphNodesEdges = readLine().split(' ');
    const graphNodes = parseInt(graphNodesEdges[0], 10);
    const graphEdges = parseInt(graphNodesEdges[1], 10);

    let graphFrom = [];
    let graphTo = [];

    for (let i = 0; i < graphEdges; i++) {
        const graphFromTo = readLine().split(' ');

        graphFrom.push(parseInt(graphFromTo[0], 10));
        graphTo.push(parseInt(graphFromTo[1], 10));
    }

    const ids = readLine().split(' ').map(idsTemp => parseInt(idsTemp, 10));

    const val = parseInt(readLine(), 10);

    const ans = findShortest(graphNodes, graphFrom, graphTo, ids, val);

    console.log(ans);
    ws.write(ans + '\n');

    ws.end();
}

