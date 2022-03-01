% Copyright Prolog Development Center

interface deck
    open core

constants
    arclightPhoenixName : string = "Arclight Phoenix".
    songOfCreationName : string = "Song of Creation".
    runawaySteamkinName : string = "Runaway Steam-Kin".
    simianSpiritGuideName : string = "Simian Spirit Guide".
    oxOfAgonasName : string = "Ox of Agonas".
    maximizeVelocityName : string = "Maximize Velocity".

properties
    winCon : mapM{rules::cardState, setM{rules::boardState}}.
    decklist : card*.
    companion : card*.
    mulliganPriority : string*.
    mulliganRemove : string*.
    manaPriority : rules::mana*.
    playPriority : string*.
    scryPriority : string**.
    shortName : string.
    discardPriority : string*.

predicates
    showIzzetWinCons_nd : () -> string nondeterm.
    getDecklist : () -> string.

end interface deck
