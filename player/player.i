% Copyright Prolog Development Center

interface player
    open core

properties
    name : string.
    deck : deck.
    library : queueM{card}.
    hasLost : boolean.
    hasWon : boolean.
    reason : string.
    handSize : integer.
    hand : card*.
    graveyard : card*.
    exile : card*.
    companion : card*.
    play : card*.
    scryPriority : string**.
    landPlays : integer.
    opponent : player.
    spellsThisTurn : integer.
    life : integer.
    onDraw : predicate{integer}.

predicates
    draw : ().
    firstDraw : ().
    draw : (integer).
    mill : (integer).
    scry : (positive).
    reveal : (positive) -> card*.
    putInGraveyard : (card*).
    damage : (integer).
    discard : (integer).
    shuffle : (card* CardsToShuffle, card* RemainingLibrary = []).
    removeFloatingMana : ().

predicates
    untap : ().
    upkeep : ().
    main : ().
    combat : ().
    endPhase : ().

end interface player
