% Copyright Prolog Development Center

interface card
    open core
    [presenter]

properties
    name : string.
    id : integer.
    manaType : rules::mana*.
    manaUsePriority : integer.
    manaCost : rules::mana*.
    cardType : rules::cardType.
    tapped : boolean.
    canTap : boolean.
    untapStyle : predicate{card}.
    canCombo : boolean.
    saveForCombo : boolean.
    onPlay : predicate{player}.
    onEnchant : predicate{card, card}.
    canBeEnchanted : boolean.
    sacrifice : boolean.
    exile : boolean.
    power : integer.
    toughness : integer.
    attackStyle : predicate{player}.
    keywords : setM{rules::keyword}.
    countersList : rules::counters*.

predicates
    tap : ().
    untap : ().
    getPower : () -> integer.

predicates
    addKeyword : (rules::keyword).
    removeKeyword : (rules::keyword).
    addCounter : (rules::counters).
    removeCounter : (rules::counters).

end interface card
