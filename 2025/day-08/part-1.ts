import Heap from "npm:heap";
import { Element, initializeDistances, parseInput } from "./shared.ts";

export function part1(input: string, connections: number): number {
    const nodes = parseInput(input);

    const distances = initializeDistances(nodes);

    const circuits: Set<[number, number, number]>[] = [];

    for (let i = 0; i < connections; i++) {
        const elem = distances.pop() as Element;
        const a = elem.first;
        const b = elem.second;

        let indices = [];
        for (let i = 0; i < circuits.length; i++) {
            if (circuits[i].has(a) || circuits[i].has(b)) {
                circuits[i].add(a);
                circuits[i].add(b);
                indices.push(i);
            }
        }
        if (indices.length === 0) {
            circuits.push(new Set<[number, number, number]>([a, b]));
        } else if (indices.length > 1) {
            for (let idx of indices.slice(1)) {
                for (let elem of circuits[idx]) {
                    circuits[indices[0]].add(elem);
                }
            }
            for (let idx of indices.slice(1).sort((a, b) => b - a)) {
                circuits.splice(idx, 1);
            }
        }
    }

    return circuits
        .map((s) => s.size)
        .toSorted((a, b) => b - a)
        .slice(0, 3)
        .reduce((a, b) => a * b, 1);
}

if (import.meta.main) {
    const input = await Deno.readTextFile("input.txt");

    console.log("Part 1:", part1(input, 1000));
}
