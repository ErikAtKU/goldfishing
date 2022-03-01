% Copyright Prolog Development Center

class rules
    open core

domains
    mana =
        w;
        u;
        b;
        r;
        g;
        c;
        % colorless
        a;
        % any mana type
        q;
        % untap potential mana source
        i(integer);
        h(mana, mana);
        m(mana*).
    cardType =
        sorcery;
        instant;
        land;
        creature;
        artifact;
        enchantment(enchantTarget).
    keyword =
        haste;
        jumpstart;
        escape(mana*, integer Exile, predicate{card, player} OnEscape);
        adventure(card);
        ritual;
        boon;
        discards(integer);
        flying.
    counters = p11; m11.
    enchantTarget =
        n(string);
        c(cardType);
        board.
    boardState = win(card* Play, card* Hand, card* Graveyard, mana*).
    cardState =
        p(integer*);
        h(integer*);
        g(integer*).

constants
    basicMana : mana* = [w, u, b, r, g, c, i(0), i(1)].

predicates
    sortMana : (mana*) -> mana*.

predicates
    manaRelation : comparator{mana}.

predicates
    subtractCost : (mana* Initial, mana* Return) -> mana* Final determ.
    addCosts : (mana* Mana1, mana* Mana2) -> mana* Final.

predicates
    insertWincon : (mapM{cardState, setM{boardState}}, boardState).

end class rules
