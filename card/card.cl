% Copyright Prolog Development Center

class card : card
    open core, rules

constructors
    land : (string Name, mana* ManaType).
    creature : (string Name, mana* ManaCost, integer Power, integer Toughness).
    sorcery : (string Name, mana* ManaCost).
    instant : (string Name, mana* ManaCost).
    enchantment : (string Name, mana* ManaCost, enchantTarget Target).

constructors
    make : (card).

predicates
    copy : (card, integer) -> card*.

end class card
