% Copyright Prolog Development Center

implement rules
    open core

clauses
    manaRelation(Mana, Mana) = equal :-
        !.
    manaRelation(BasicMana1, BasicMana2) = compare(BasicMana1, BasicMana2) :-
        list::isMember(BasicMana1, basicMana),
        list::isMember(BasicMana2, basicMana),
        !.
    manaRelation(BasicMana, Other) = less :-
        list::isMember(BasicMana, basicMana),
        not(list::isMember(Other, basicMana)),
        !.
    manaRelation(Other, BasicMana) = greater :-
        list::isMember(BasicMana, basicMana),
        not(list::isMember(Other, basicMana)),
        !.
    manaRelation(h(A, _B), h(C, _D)) = manaRelation(A, C) :-
        !.
    manaRelation(h(A, B), _Other) = less :-
        (list::isMember(A, basicMana) or list::isMember(B, basicMana)),
        !.
    manaRelation(Other, h(A, B)) = manaRelation(h(A, B), Other) :-
        !.
    manaRelation(i(A), i(B)) = compare(A, B) :-
        !.
    manaRelation(A, B) = compare(A, B).

clauses
    sortMana(Mana) = SortedMana :-
        list::filter(Mana, { (i(A)) :- A > 0 }, Colorless, Regular),
        ColorlessCount =
            list::fold(Colorless,
                { (ColorlessMana, Tally) =
                    if i(Amount) = ColorlessMana then
                        Amount + Tally
                    else
                        Tally
                    end if
                },
                0),
        SortedRegularMana = list::sortBy(manaRelation, Regular),
        ColorlessSingles = [ i(1) || _ = std::fromTo(1, ColorlessCount) ],
        SortedMana = list::append(SortedRegularMana, ColorlessSingles).

clauses
    subtractCost([], _Return) = [] :-
        !.
    subtractCost(Initial, [i(Val) | Rest]) = Final :-
        Val > 1,
        !,
        Final = subtractCost(Initial, [i(1), i(Val - 1) | Rest]).
    subtractCost(Initial, [i(Val) | Rest]) = Final :-
        Val < 1,
        !,
        Final = subtractCost(Initial, Rest).
    subtractCost(Initial, [Mana | Rest]) = Final :-
        list::isMember(Mana, Initial),
        !,
        Reduced = list::remove(Initial, Mana),
        Final = subtractCost(Reduced, Rest).
    subtractCost(Initial, [Mana | Rest]) = Final :-
        Hybrid =
            list::exists(Initial,
                { (ManaCost) :-
                    h(A, B) = ManaCost,
                    (A = Mana orelse B = Mana)
                }),
        !,
        Reduced = list::remove(Initial, Hybrid),
        Final = subtractCost(Reduced, Rest).
    subtractCost(Initial, [q | Rest]) = Final :-
        !,
        Final = subtractCost(Initial, [a | Rest]).
    subtractCost(Initial, [a | Rest]) = Final :-
        Mana in Initial,
        list::isMember(Mana, [w, u, b, r, g, c]),
        !,
        Reduced = list::remove(Initial, Mana),
        Final = subtractCost(Reduced, Rest).
    subtractCost(Initial, Rest) = Final :-
        i(Val) in Initial,
        Val > 1,
        !,
        Final = subtractCost([i(1), i(Val - 1) | list::remove(Initial, i(Val))], Rest).
    subtractCost(Initial, Rest) = Final :-
        i(Val) in Initial,
        Val < 1,
        !,
        Final = subtractCost(list::remove(Initial, i(Val)), Rest).
    subtractCost(Initial, [_Mana | Rest]) = Final :-
        i(1) in Initial,
        !,
        Reduced = list::remove(Initial, i(1)),
        Final = subtractCost(Reduced, Rest).
    subtractCost(Initial, [_Mana | Rest]) = Final :-
        !,
        Final = subtractCost(Initial, Rest).
    subtractCost(Initial, _) = Initial :-
        !.

clauses
    addCosts(Mana1, Mana2) = Final :-
        ManaSet = list::append(Mana1, Mana2),
        if Final = subtractCost([i(1) | ManaSet], [i(1)]) then
            % for formatting
        else
            Final = ManaSet
        end if.

clauses
    insertWincon(Map, win(Play, Hand, Graveyard, Mana)) :-
        PlayLength = list::length(Play),
        HandLength = list::length(Hand),
        GraveyardLength = list::length(Graveyard),
        PKey = p(list::map(Play, { (Card) = Card:id })),
        HKey = h(list::map(Hand, { (Card) = Card:id })),
        GKey = g(list::map(Graveyard, { (Card) = Card:id })),
        if GraveyardLength >= HandLength and GraveyardLength >= PlayLength then
            Key = GKey
        elseif HandLength >= PlayLength then
            Key = HKey
        else
            Key = PKey
        end if,
        WinSet = Map:get_default(Key, setM_redBlack::new()),
        WinSet:insert(win(Play, Hand, Graveyard, sortMana(Mana))).

end implement rules
