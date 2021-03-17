#lang forge

option solver "/Users/rajpaul/cs1710/projects/sat/run.sh"

sig Node {
    next: one Node
}

pred empty {
    no Node
}

pred injective {
    next.~next in iden
}

pred surjective {
    Node in Node.next
}

pred connected {
    some n1: Node | { all n2: Node | n2 in n1.^next }
}

pred isRing {
    empty or
    (injective and
    surjective and
    connected)
}

run isRing for exactly 4 Node