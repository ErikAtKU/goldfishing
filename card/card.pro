% Copyright Prolog Development Center

implement card
    open core, rules

constants
    length : integer = 8.

facts
    name : string.
    id : integer.
    manaType : mana* := [].
    manaUsePriority : integer := 1.
    manaCost : mana* := [].
    cardType : cardType := erroneous.
    tapped : boolean := false.
    canTap : boolean := false.
    canCombo : boolean := true.
    saveForCombo : boolean := true.
    onPlay : predicate{player} := { (_Player) :- ! }.
    onEnchant : predicate{card, card} := { (_Card, _Self) :- ! }.
    canBeEnchanted : boolean := true.
    untapStyle : predicate{card} :=
        { (Card) :-
            Card:canTap := true,
            Card:tapped := false,
            Card:canCombo := true
        }.
    sacrifice : boolean := false.
    exile : boolean := false.
    power : integer := 0.
    toughness : integer := 0.
    attackStyle : predicate{player} := { (_Player) :- ! }.
    keywords : setM{keyword} := setM_redBlack::new().
    countersList : rules::counters* := [].

constructors
    private_new : (string).
clauses
    private_new(Name) :-
        name := Name,
        NoSpace = string::replaceAll(Name, " ", "", string::caseInsensitive),
        LowerCase = string::toLowerCase(NoSpace),
        CharList = string::toCharList(LowerCase),
        SimpleList = list::removeDuplicates(CharList),
        FirstEight = list::take(8, SimpleList),
        id := list::fold(FirstEight, { (Char, Count) = 10 * Count + string::getCharValue(Char) }, 0).

clauses
    land(Name, ManaType) :-
        private_new(Name),
        manaType := ManaType,
        canTap := true,
        cardType := rules::land.

clauses
    creature(Name, ManaCost, Power, Toughness) :-
        private_new(Name),
        manaCost := sortMana(ManaCost),
        canCombo := false,
        cardType := rules::creature,
        power := Power,
        toughness := Toughness.

clauses
    sorcery(Name, ManaCost) :-
        private_new(Name),
        manaCost := sortMana(ManaCost),
        saveForCombo := true,
        cardType := rules::sorcery.

clauses
    instant(Name, ManaCost) :-
        private_new(Name),
        manaCost := sortMana(ManaCost),
        saveForCombo := true,
        cardType := rules::instant.

clauses
    enchantment(Name, ManaCost, Target) :-
        private_new(Name),
        manaCost := sortMana(ManaCost),
        saveForCombo := true,
        cardType := enchantment(Target).

clauses
    addKeyword(Keyword) :-
        keywords:insert(Keyword).

clauses
    removeKeyword(Keyword) :-
        keywords:remove(Keyword).

clauses
    addCounter(p11) :-
        list::isMember(m11, countersList),
        !,
        removeCounter(m11).
    addCounter(m11) :-
        list::isMember(p11, countersList),
        !,
        removeCounter(p11).
    addCounter(Counter) :-
        countersList := [Counter | countersList].

clauses
    removeCounter(Counter) :-
        countersList := list::remove(countersList, Counter).

clauses
    make(CardToCopy) :-
        name := CardToCopy:name,
        id := CardToCopy:id,
        manaType := [ ManaType || ManaType in CardToCopy:manaType ],
        manaUsePriority := CardToCopy:manaUsePriority,
        manaCost := CardToCopy:manaCost,
        cardType := CardToCopy:cardType,
        tapped := CardToCopy:tapped,
        canTap := CardToCopy:canTap,
        canCombo := CardToCopy:canCombo,
        saveForCombo := CardToCopy:saveForCombo,
        onPlay := CardToCopy:onPlay,
        onEnchant := CardToCopy:onEnchant,
        canBeEnchanted := CardToCopy:canBeEnchanted,
        untapStyle := CardToCopy:untapStyle,
        sacrifice := CardToCopy:sacrifice,
        exile := CardToCopy:exile,
        power := CardToCopy:power,
        toughness := CardToCopy:toughness,
        attackStyle := CardToCopy:attackStyle,
        keywords := CardToCopy:keywords:clone(),
        countersList := CardToCopy:countersList.

clauses
    copy(CardToCopy, Times) = CardList :-
        CardList =
            [ Card ||
                _ = std::fromTo(1, Times),
                Card = make(CardToCopy)
            ].

clauses
    tap() :-
        tapped := true,
        canTap := false.

clauses
    untap() :-
        untapStyle(This).

clauses
    getPower() = Power:value :-
        Power = varM_integer::new(power),
        foreach Counter in countersList do
            if Counter = p11 then
                Power:inc()
            elseif Counter = m11 then
                Power:dec()
            end if
        end foreach.

clauses
    presenter() = Presenter :-
        NameShort = presenter_h(name),
        Presenter = presenter::mkPresenter_fixed(presenter::fp_string(NameShort)).

predicates
    presenter_h : (string Name) -> string CutDown.
clauses
    presenter_h(Name) = Result :-
        Name1 = string::replaceAll(Name, " ", ""),
        [Name2 | _Tail] = string::split(Name1, ","),
        Name2 <> Name,
        !,
        Result = if check_dt(Name2) then Name2 else presenter_h(Name2) end if.
    presenter_h(Name) = Result :-
        !,
        Name1 = string::replaceAll(Name, "a", "", string::caseSensitive),
        Name2 = string::replaceAll(Name1, "i", "", string::caseSensitive),
        Name3 = string::replaceAll(Name2, "o", "", string::caseSensitive),
        Name4 = string::replaceAll(Name3, "u", "", string::caseSensitive),
        if check_dt(Name4) then
            Result = Name4
        else
            string::front(Name, length, Result, _Rest)
        end if.

predicates
    check_dt : (string) determ.
clauses
    check_dt(Name) :-
        string::length(Name) <= length.

end implement card
