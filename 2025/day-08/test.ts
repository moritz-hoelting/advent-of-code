import { assertEquals } from "@std/assert";
import { part1 } from "./part-1.ts";
import { part2 } from "./part-2.ts";

const example = `162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689`;

Deno.test(function part1Test() {
    assertEquals(part1(example, 10), 40);
});

Deno.test(function part2Test() {
    assertEquals(part2(example), 25272);
});
