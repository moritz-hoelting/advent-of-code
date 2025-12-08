import Heap from "npm:heap";

export function parseInput(input: string): [number, number, number][] {
    return input
        .trim()
        .split("\n")
        .map(
            (line) =>
                line
                    .trim()
                    .split(",", 3)
                    .map((s) => Number(s.trim())) as [
                    x: number,
                    y: number,
                    z: number
                ]
        );
}

export function initializeDistances(
    nodes: [number, number, number][]
): Heap<Element> {
    const distances = new Heap<Element>(
        (a: Element, b: Element) => a.distance - b.distance
    );
    for (let i = 0; i < nodes.length; i++) {
        for (let j = i + 1; j < nodes.length; j++) {
            distances.push(new Element(nodes[i], nodes[j]));
        }
    }
    return distances;
}

export class Element {
    private a: [number, number, number];
    private b: [number, number, number];
    private dist: number;

    constructor(a: [number, number, number], b: [number, number, number]) {
        this.a = a;
        this.b = b;
        this.dist = Element.euclideanDistance(a, b);
    }

    // Distance from origin (or adjust if you need distance to another fixed point)
    private static euclideanDistance(
        a: [number, number, number],
        b: [number, number, number]
    ): number {
        return Math.sqrt(
            (a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2 + (a[2] - b[2]) ** 2
        );
    }

    get distance(): number {
        return this.dist;
    }

    get first(): [number, number, number] {
        return this.a;
    }

    get second(): [number, number, number] {
        return this.b;
    }
}