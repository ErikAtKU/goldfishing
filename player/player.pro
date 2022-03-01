% Copyright Prolog Development Center

implement player
    open core, rules

facts
    name : string.
    deck : deck.
    life : integer.
    hasLost : boolean := false.
    hasWon : boolean := false.
    reason : string := "".
    library : queueM{card} := erroneous.
    hand : card* := [].
    graveyard : card* := [].
    exile : card* := [].
    play : card* := [].
    landPlays : integer := 0.
    manaPriority : mana* := [].
    playPriority : string* := [].
    scryPriority : string** := [].
    handSize : integer := 7.
    companion : card* := erroneous.
    opponent : player := erroneous.
    spellsThisTurn : integer := 0.
    onDraw : predicate{integer} := erroneous.
    floatingMana : card* := [].

clauses
    new(Deck, Name) :-
        deck := Deck,
        name := Name,
        life := 20,
        onDraw := { (_) :- ! }.

clauses
    goldfish() :-
        new(deck::blank(60), "Goldfish"),
        handSize := 0.

clauses
    goldfish(Turns) :-
        new(deck::blank(7 + Turns), "Goldfish"),
        handSize := 0.

clauses
    shuffle(CardsToShuffle, RemainingLibrary) :-
        RandomList =
            [ tuple(Card, Num) ||
                (Card in CardsToShuffle or Card in RemainingLibrary), %+
                    Num = math::random()
            ],
        SortedList = list::sortBy({ (tuple(_A1, A2), tuple(_B1, B2)) = compare(A2, B2) }, RandomList),
        list::unzip(SortedList, List, _Rands),
        library := queueM_fact::new(List).

clauses
    player(Player) :-
        new(Player:deck, Player:name).

clauses
    firstDraw() :-
        manaPriority := deck:manaPriority,
        playPriority := deck:playPriority,
        scryPriority := deck:scryPriority,
        companion := deck:companion,
        Decklist =
            [ Card ||
                OriginalCard in deck:decklist,
                Card in card::copy(OriginalCard, 1)
            ],
        shuffle(Decklist),
        draw(7),
        if [] = deck:mulliganPriority then
        elseif Card in hand and list::isMember(Card:name, deck:mulliganPriority) then
        else
            hand := [],
            shuffle(Decklist),
            draw(7),
            if Card in hand and list::isMember(Card:name, deck:mulliganPriority) then
                mulliganRemove()
            else
                hand := [],
                shuffle(Decklist),
                draw(7),
                mulliganRemove(),
                mulliganRemove()
            end if
        end if.

predicates
    mulliganRemove : ().
clauses
    mulliganRemove() :-
        if PriorityRemove in hand and list::isMember(PriorityRemove:name, deck:mulliganRemove) then
            ToRemoveCard = PriorityRemove
        elseif NotPriorityKeep in hand and not(list::isMember(NotPriorityKeep:name, deck:mulliganPriority)) then
            ToRemoveCard = NotPriorityKeep
        elseif SingleList = list::removeDuplicates(hand) and DuplicateList = list::difference(hand, SingleList) and RemoveDuplicate in DuplicateList
        then
            ToRemoveCard = RemoveDuplicate
        else
            [ToRemoveCard | _Rest] = hand
        end if,
        !,
        hand := list::remove(hand, ToRemoveCard),
        library:insert(ToRemoveCard).
    mulliganRemove().

clauses
    draw() :-
        draw(1).

clauses
    draw(Draw) :-
        onDraw(Draw),
        foreach _ = std::fromTo(1, Draw) do
            if Card = library:tryRemoveFirst() then
                hand := [Card | hand]
            else
                hasLost := true,
                reason := "Mill"
            end if
        end foreach.

clauses
    mill(Amount) :-
        foreach _ = std::fromTo(1, Amount) and Card = library:tryRemoveFirst() do
            graveyard := [Card | graveyard]
        end foreach.

clauses
    scry(Scry) :-
        ToScry =
            [ Card ||
                _ = std::fromTo(1, Scry),
                Card = library:tryRemoveFirst()
            ],
        CurrentCards = list::map(list::append(play, hand), { (Card) = Card:name }),
        RemainingScry = list::map(scryPriority, { (PriorityList) = list::difference(PriorityList, CurrentCards) }),
        SortedList = list::sortBy({ (A, B) = compare(list::length(A), list::length(B)) }, RemainingScry),
        ScryList = varM::new([]),
        foreach List in SortedList and Item in List do
            if ScryCard = list::exists(ToScry, { (ScryCard) :- ScryCard:name = Item }) then
                ScryList:value := list::removeDuplicatesBy({ (A, B) = compare(A:name, B:name) }, [ScryCard | ScryList:value])
            end if
        end foreach,
        foreach ScryCard in ScryList:value do
            library:insertFirst(ScryCard)
        end foreach,
        BottomCards = list::difference(ToScry, ScryList:value),
        foreach BottomCard in BottomCards do
            library:insert(BottomCard)
        end foreach.

clauses
    reveal(Reveal) =
        [ Card ||
            _ = std::fromTo(1, Reveal),
            Card = library:tryRemoveFirst()
        ].

clauses
    putInGraveyard(CardList) :-
        graveyard := list::append(CardList, graveyard).

clauses
    damage(Amount) :-
        life := life - Amount,
        if life <= 0 then
            hasLost := true,
            reason := "0 life"
        end if.

clauses
    discard(Val) :-
        Val <= 0,
        !.
    discard(Val) :-
        if Name in deck:discardPriority and Card = list::exists(hand, { (Elem) :- string::equalIgnoreCase(Elem:name, Name) }) then
        elseif Card in hand and not(list::isMember(Card:name, playPriority)) then
        else
            Card in hand
        end if,
        !,
        hand := list::remove(hand, Card),
        putInGraveyard([Card]),
        discard(Val - 1).
    discard(_).

clauses
    removeFloatingMana() :-
        floatingMana := [].

clauses
    untap() :-
        spellsThisTurn := 0,
        foreach Card in play do
            Card:untap()
        end foreach,
        landPlays := 1,
        foreach Card in play and Card:name = deck::songOfCreationName do
            landPlays := landPlays + 1
        end foreach.

clauses
    upkeep().

clauses
    main() :-
        checkAllWinCon(),
        !.
    main() :-
        doLand(),
        !,
        main().
    main() :-
        (doCreature() or doGraveyard() or doEnchant() or doCompanion() or doSpell() or doAdventure()),
        !,
        spellsThisTurn := spellsThisTurn + 1,
        main().
    main() :-
        removeFloatingMana().

predicates
    checkAllWinCon : () determ.
clauses
    checkAllWinCon() :-
        opponent:life <= 0,
        !.
    checkAllWinCon() :-
        opponent:library:isEmpty(),
        !.
    checkAllWinCon() :-
        ManaLands = getManaSources(3),
        WinCon = quickFilterWinCon_nd(deck:winCon), %+
            win(_, _, _, ManaCost) = WinCon,
            checkWinCon(play, hand, graveyard, WinCon),
            _ = checkCanCast(ManaCost, ManaLands),
        !,
        hasWon := true,
        win(Play, Hand, Grave, Mana) = WinCon,
        reason := string::format("%p - %p - %p - %p", Play, Hand, Grave, Mana).

predicates
    quickFilterWinCon_nd : (mapM{cardState, setM{boardState}}) -> boardState nondeterm.
clauses
    quickFilterWinCon_nd(WinConMap) = WinCon :-
        CardState in WinConMap:keyList, %+
            if g(Key) = CardState then
                quickCheck_dt(Key, graveyard)
            elseif h(Key) = CardState then
                quickCheck_dt(Key, hand)
            elseif p(Key) = CardState then
                quickCheck_dt(Key, play)
            end if,
            WinCon in WinConMap:get(CardState).

predicates
    quickCheck_dt : (integer* Key, card* Zone) determ.
clauses
    quickCheck_dt([], _).
    quickCheck_dt([FirstKey | Rest], ZoneCards) :-
        ResultCards = quickCheck_h(FirstKey, ZoneCards),
        quickCheck_dt(Rest, ResultCards).

predicates
    quickCheck_h : (integer, card*) -> card* determ.
clauses
    quickCheck_h(Key, [FirstCard | RestCards]) = RestCards :-
        Key = FirstCard:id,
        !.
    quickCheck_h(Key, [FirstCard | RestCards]) = [FirstCard | ResultCards] :-
        ResultCards = quickCheck_h(Key, RestCards).

predicates
    checkWinCon : (card* Play, card* Hand, card* Graveyard, boardState WinCon) determ.
clauses
    checkWinCon(_, _, _, win([], [], [], _)) :-
        !.
    checkWinCon(PlayList, HandList, GraveyardList, win([ToPlay | PlayWinCon], HandWinCon, GraveWinCon, Mana)) :-
        !,
        PlayCard =
            list::exists(PlayList,
                { (Card) :-
                    true = Card:canCombo,
                    Card:name = ToPlay:name
                }),
        checkWinCon(list::remove(PlayList, PlayCard), HandList, GraveyardList, win(PlayWinCon, HandWinCon, GraveWinCon, Mana)).
    checkWinCon(PlayList, HandList, GraveyardList, win(PlayWinCon, [ToHand | HandWinCon], GraveWinCon, Mana)) :-
        !,
        HandCard = list::exists(HandList, { (Card) :- Card:name = ToHand:name }),
        checkWinCon(PlayList, list::remove(HandList, HandCard), GraveyardList, win(PlayWinCon, HandWinCon, GraveWinCon, Mana)).
    checkWinCon(PlayList, HandList, GraveyardList, win(PlayWinCon, HandWinCon, [ToGrave | GraveWinCon], Mana)) :-
        !,
        GraveCard = list::exists(GraveyardList, { (Card) :- Card:name = ToGrave:name }),
        checkWinCon(PlayList, HandList, list::remove(GraveyardList, GraveCard), win(PlayWinCon, HandWinCon, GraveWinCon, Mana)).

predicates
    doLand : () determ.
clauses
    doLand() :-
        landPlays > 0,
        Lands = list::filter(hand, { (Card) :- land = Card:cardType }),
        if list::length(play) = 0 and Land in Lands and false = Land:canTap then
        elseif Mana in manaPriority and Land in Lands and ManaType = Land:manaType and Mana in ManaType then
            manaPriority := list::remove(manaPriority, Mana)
        elseif Land in Lands then
        else
            fail
        end if,
        landPlays := landPlays - 1,
        hand := list::remove(hand, Land),
        play := [Land | play].

predicates
    doCreature : () determ.
clauses
    doCreature() :-
        Creatures =
            list::filter(hand,
                { (Card) :-
                    creature = Card:cardType,
                    if discards(Num) = Card:keywords:getAll_nd() then
                        list::length(This:hand) > Num
                    end if,
                    not([] = Card:manaCost)
                }),
        not([] = Creatures),
        PriorityCreature =
            [ Card ||
                Name in playPriority,
                if Card in Creatures and Card:name = Name then
                else
                    fail
                end if
            ],
        SortedCreatures = list::append(PriorityCreature, Creatures),
        ShortenedCreaturesList = list::removeDuplicatesBy({ (A, B) = string::compareIgnoreCase(A:name, B:name) }, SortedCreatures),
        ManaSource = getManaSources(3),
        CanCastList =
            [ tuple(Creature, Solution) ||
                Creature in ShortenedCreaturesList, %+
                    ManaCost = Creature:manaCost,
                    TempManaSources = list::remove(ManaSource, Creature),
                    Solution = checkCanCast(ManaCost, TempManaSources)
            ],
        if tuple(Creature, Solution) in CanCastList and not(list::isMemberBy({ (A, B) = string::compareIgnoreCase(A:name, B:name) }, Creature, play))
        then
        elseif tuple(Creature, Solution) in CanCastList then
        else
            fail
        end if,
        playPriority := list::remove(playPriority, Creature:name),
        if discards(Num) = Creature:keywords:getAll_nd() then
            This:discard(Num)
        end if,
        payMana(Solution),
        hand := list::remove(hand, Creature),
        play := [Creature | play],
        if Creature:keywords:contains(haste) then
            Creature:canTap := true
        end if.

predicates
    doSpell : () determ.
clauses
    doSpell() :-
        SpellList =
            list::filter(hand,
                { (MainCard) :-
                    (adventure(Card) = MainCard:keywords:getAll_nd() or Card = MainCard),
                    !,
                    if discards(Num) = Card:keywords:getAll_nd() then
                        list::length(This:hand) > Num
                    end if,
                    if boon = Card:keywords:getAll_nd() then
                        CreatureList = list::filter(This:play, { (Elem) :- Elem:cardType = creature }),
                        CreatureList <> []
                    end if,
                    false = Card:saveForCombo,
                    not([] = Card:manaCost),
                    (instant = Card:cardType orelse sorcery = Card:cardType)
                }),
        not([] = SpellList),
        ManaSource = getManaSources(),
        CanCastList =
            [ tuple(MainCard, Solution) ||
                MainCard in SpellList, %+
                    if adventure(Spell) = MainCard:keywords:getAll_nd() then
                    else
                        Spell = MainCard
                    end if,
                    TempManaSources = list::remove(ManaSource, MainCard),
                    ManaCost = Spell:manaCost,
                    Solution = checkCanCast(ManaCost, TempManaSources)
            ],
        if tuple(Spell, Solution) in CanCastList then
        else
            fail
        end if,
        payMana(Solution),
        hand := list::remove(hand, Spell),
        if adventure(AdventureSpell) = Spell:keywords:getAll_nd() then
            if discards(Num) = AdventureSpell:keywords:getAll_nd() then
                This:discard(Num)
            end if,
            AdventureSpell:onPlay(This),
            exile := [Spell | exile]
        else
            if discards(Num) = Spell:keywords:getAll_nd() then
                This:discard(Num)
            end if,
            Spell:onPlay(This),
            putInGraveyard([Spell])
        end if.

predicates
    doEnchant : () determ.
clauses
    doEnchant() :-
        Enchantments =
            list::filter(hand,
                { (Card) :-
                    enchantment(_X) = Card:cardType,
                    not([] = Card:manaCost),
                    false = Card:saveForCombo,
                    if discards(Num) = Card:keywords:getAll_nd() then
                        list::length(This:hand) > Num
                    end if
                }),
        not([] = Enchantments),
        PriorityEnchantments =
            [ Card ||
                Name in playPriority,
                if Card in Enchantments and Card:name = Name then
                else
                    fail
                end if
            ],
        SortedEnchantments = list::append(PriorityEnchantments, Enchantments),
        ShortenedEnchantmentsList = list::removeDuplicatesBy({ (A, B) = string::compareIgnoreCase(A:name, B:name) }, SortedEnchantments),
        ManaSource = getManaSources(3),
        CanCastList =
            [ tuple(Enchantment, Solution, Target) ||
                Enchantment in ShortenedEnchantmentsList, %+
                    enchantment(CardTarget) = Enchantment:cardType,
                    if n(Name) = CardTarget then
                        Target = list::exists(play, { (Card) :- Card:name = Name })
                    elseif c(Type) = CardTarget then
                        Target = list::exists(play, { (Card) :- Card:cardType = Type })
                    else
                        board = CardTarget,
                        Target = Enchantment
                    end if,
                    ManaCost = Enchantment:manaCost,
                    Solution = checkCanCast(ManaCost, ManaSource)
            ],
        if tuple(Enchantment, Solution, Target) in CanCastList
            and not(list::isMemberBy({ (A, B) = string::compareIgnoreCase(A:name, B:name) }, Enchantment, play))
        then
        elseif tuple(Enchantment, Solution, Target) in CanCastList then
        else
            fail
        end if,
        playPriority := list::remove(playPriority, Enchantment:name),
        if discards(Num) = Enchantment:keywords:getAll_nd() then
            This:discard(Num)
        end if,
        payMana(Solution),
        hand := list::remove(hand, Enchantment),
        play := [Enchantment | play],
        Enchantment:onEnchant(Target, Enchantment),
        Enchantment:onPlay(This).

predicates
    doGraveyard : () determ.
clauses
    doGraveyard() :-
        ManaSources = getManaSources(2),
        Card in graveyard,
        escape(ManaCost, Exile, OnEscape) = Card:keywords:getAll_nd(),
        if discards(Num) = Card:keywords:getAll_nd() then
            list::length(This:hand) >= Num
        end if,
        list::length(graveyard) > Exile,
        Solution = checkCanCast(ManaCost, ManaSources),
        !,
        graveyard := list::remove(graveyard, Card),
        if discards(NumDisc) = Card:keywords:getAll_nd() then
            This:discard(NumDisc)
        end if,
        payMana(Solution),
        list::split(Exile, graveyard, _ExiledCards, Remaining),
        graveyard := Remaining,
        if creature = Card:cardType then
            play := [Card | play]
        else
        end if,
        OnEscape(Card, This),
        if Card:keywords:contains(haste) then
            Card:canTap := true
        end if.
    doGraveyard() :-
        ManaSources = getManaSources(2),
        Card in graveyard,
        if jumpstart = Card:keywords:getAll_nd() then
            list::length(This:hand) >= 0
        else
            fail
        end if,
        if discards(Num) = Card:keywords:getAll_nd() then
            list::length(This:hand) >= Num
        end if,
        if boon = Card:keywords:getAll_nd() then
            CreatureList = list::filter(This:play, { (Elem) :- Elem:cardType = creature }),
            CreatureList <> []
        end if,
        ManaCost = Card:manaCost,
        Solution = checkCanCast(ManaCost, ManaSources),
        !,
        graveyard := list::remove(graveyard, Card),
        if jumpstart = Card:keywords:getAll_nd() then
            This:discard(1)
        end if,
        if discards(NumDisc) = Card:keywords:getAll_nd() then
            This:discard(NumDisc)
        end if,
        payMana(Solution),
        exile := [Card | exile],
        if creature = Card:cardType then
            play := [Card | play]
        else
        end if,
        Card:onPlay(This),
        if Card:keywords:contains(haste) then
            Card:canTap := true
        end if.

predicates
    doAdventure : () determ.
clauses
    doAdventure() :-
        ManaSources = getManaSources(2),
        Card in exile,
        adventure(_Something) = Card:keywords:getAll_nd(),
        Solution = checkCanCast(Card:manaCost, ManaSources),
        !,
        exile := list::remove(exile, Card),
        payMana(Solution),
        if creature = Card:cardType then
            play := [Card | play]
        else
        end if,
        Card:onPlay(This),
        if Card:keywords:contains(haste) then
            Card:canTap := true
        end if.

predicates
    doCompanion : () determ.
clauses
    doCompanion() :-
        [Companion] = companion,
        not((Card in hand and Card:name = Companion:name)),
        ManaSource = getManaSources(1),
        Solution = checkCanCast([i(1), i(1), i(1)], ManaSource), %+
            !,
            payMana(Solution),
            companion := [],
            hand := [Companion | hand].

predicates
    getManaSources : (integer Priority = 2) -> card*.
clauses
    getManaSources(Priority) = OrderedManaSources :-
        ManaSources =
            list::filter(play,
                { (Card) :-
                    not([] = Card:manaType),
                    true = Card:canTap,
                    false = Card:tapped
                }),
        Steamkin =
            list::filter(play,
                { (Card) :-
                    Card:name = deck::runawaySteamkinName,
                    P11List = list::filter(Card:countersList, { (Elem) :- Elem = p11 }),
                    list::length(P11List) >= 3
                }),
        HandMana =
            list::filter(hand,
                { (MainCard) :-
                    (adventure(Card) = MainCard:keywords:getAll_nd() or Card = MainCard),
                    Card:keywords:contains(ritual),
                    not([] = Card:manaType),
                    !
                }),
        ManaTypesList =
            list::removeDuplicates(
                [ ManaType ||
                    Card in play,
                    not(true = Card:sacrifice),
                    not(true = Card:exile),
                    ManaType in Card:manaType,
                    not(q = ManaType)
                ]),
        foreach Card in ManaSources and q in Card:manaType do
            Card:manaType := ManaTypesList
        end foreach,
        PriorityManaSources =
            list::filter(list::append(floatingMana, ManaSources, Steamkin, HandMana), { (Card) :- Card:manaUsePriority <= Priority }),
        OrderedManaSources = list::sortBy(manaPriorityComparator, PriorityManaSources, core::ascending).

predicates
    manaPriorityComparator : comparator{card, card}.
clauses
    manaPriorityComparator(CardA, CardB) = compare(CardA:manaUsePriority, CardB:manaUsePriority).

predicates
    checkCanCast : (mana* ManaCost, card* ManaSources) -> card* Solution determ.
clauses
    checkCanCast(ManaCost, ManaSources) = _ :-
        ManaTypesList =
            [ Mana ||
                MainSource in ManaSources,
                (adventure(Source) = MainSource:keywords:getAll_nd() or Source = MainSource),
                Mana in Source:manaType
            ],
        not(quickCheckCanCast_dt(ManaCost, ManaTypesList)),
        !,
        fail.
    checkCanCast(ManaCost, ManaSources) = Solution :-
        Solution = checkCanCast_dt(ManaCost, ManaSources),
        !.
    checkCanCast(ManaCost, ManaSources) = Solution :-
        Solution = checkCanCast_nd(ManaCost, ManaSources).

predicates
    quickCheckCanCast_dt : (mana* ManaCost, mana* ManaTypesList) determ.
clauses
    quickCheckCanCast_dt(ManaCost, ManaTypesList) :-
        Remaining = subtractCost(ManaCost, ManaTypesList),
        [] = Remaining.

predicates
    checkCanCast_nd : (mana* ManaCost, card* ManaSources) -> card* Solution determ.
clauses
    checkCanCast_nd([], _) = [] :-
        !.
    checkCanCast_nd(_, []) = _ :-
        !,
        fail.
    checkCanCast_nd([i(0)], _) = [] :-
        !.
    checkCanCast_nd(RemainingCost, ManaSources) = [Source | Solution] :-
        Source in ManaSources, %+
            list::isMember(Source, play),
            ManaType in Source:manaType, %+
                UnpaidCost = filterMana_dt(RemainingCost, ManaType, ManaType),
                Solution = checkCanCast_nd(UnpaidCost, list::remove(ManaSources, Source)),
        !.
    checkCanCast_nd(RemainingCost, ManaSources) = [MainSource | list::append(NeedToCast, Solution)] :-
        MainSource in ManaSources, %+
            not(list::isMember(MainSource, play)),
            if adventure(Source) = MainSource:keywords:getAll_nd() then
            else
                Source = MainSource
            end if,
            Source:keywords:contains(ritual),
            NeedToCast = checkCanCast_nd(Source:manaCost, list::remove(ManaSources, MainSource)),
            RemainingManaSourcesVar = varM::new(ManaSources),
            foreach RemovedSource in [MainSource | NeedToCast] do
                RemainingManaSourcesVar:value := list::remove(RemainingManaSourcesVar:value, RemovedSource)
            end foreach,
            ManaType in Source:manaType, %+
                UnpaidCost = filterMana_dt(RemainingCost, ManaType, ManaType),
                Solution = checkCanCast_nd(UnpaidCost, RemainingManaSourcesVar:value),
        !.

predicates
    checkCanCast_dt : (mana* ManaCost, card* ManaSources) -> card* Solution determ.
clauses
    checkCanCast_dt([], _) = [] :-
        !.
    checkCanCast_dt([i(0)], _) = [] :-
        !.
    checkCanCast_dt(RemainingCost, [Source | ManaSources]) = [Source | Solution] :-
        list::isMember(Source, play),
        ManaType in Source:manaType, %+
            UnpaidCost = filterMana_dt(RemainingCost, ManaType, ManaType),
            Solution = checkCanCast_dt(UnpaidCost, ManaSources),
        !.
    checkCanCast_dt(RemainingCost, [MainSource | ManaSources]) = [MainSource | list::append(NeedToCast, Solution)] :-
        not(list::isMember(MainSource, play)),
        if adventure(Source) = MainSource:keywords:getAll_nd() then
        else
            Source = MainSource
        end if,
        Source:keywords:contains(ritual),
        NeedToCast = checkCanCast_dt(Source:manaCost, ManaSources),
        RemainingManaSourcesVar = varM::new(ManaSources),
        foreach RemovedSource in NeedToCast do
            RemainingManaSourcesVar:value := list::remove(RemainingManaSourcesVar:value, RemovedSource)
        end foreach,
        ManaType in Source:manaType, %+
            UnpaidCost = filterMana_dt(RemainingCost, ManaType, ManaType),
            Solution = checkCanCast_dt(UnpaidCost, RemainingManaSourcesVar:value),
        !.

predicates
    filterMana_dt : (mana* RemainingCost, mana ManaType, mana OriginalMana) -> mana* determ.
clauses
    filterMana_dt([], RemainingMana, OriginalMana) = [] :-
        !,
        not(RemainingMana = OriginalMana).
    filterMana_dt(RemainingCost, m([Mana | ManaLeft]), OriginalMana) = LeftoverCost :-
        UnpaidCost = filterMana_dt(RemainingCost, Mana, OriginalMana),
        !,
        LeftoverCost = filterMana_dt(UnpaidCost, m(ManaLeft), OriginalMana).
    filterMana_dt(RemainingCost, m([_Mana | ManaLeft]), OriginalMana) = LeftoverCost :-
        !,
        LeftoverCost = filterMana_dt(RemainingCost, m(ManaLeft), OriginalMana).
    filterMana_dt(RemainingCost, Mana, _OriginalMana) = LeftoverCost :-
        PaidFor = list::exists(RemainingCost, { (ManaCost) :- checkCanPayFor_dt(ManaCost, Mana) }),
        !,
        LeftoverCost = list::remove(RemainingCost, PaidFor),
        if i(Val) = PaidFor then
            Val == 1
        end if.
    filterMana_dt(RemainingCost, RemainingMana, OriginalMana) = RemainingCost :-
        !,
        not(RemainingMana = OriginalMana).

predicates
    checkCanPayFor_dt : (mana, mana) determ.
clauses
    checkCanPayFor_dt(i(_Val), _) :-
        !.
    checkCanPayFor_dt(Mana, Mana) :-
        !.
    checkCanPayFor_dt(h(A, B), Mana) :-
        !,
        Hybrid in [A, B],
        checkCanPayFor_dt(Hybrid, Mana),
        !.
    checkCanPayFor_dt(_Mana, a) :-
        !.

predicates
    payMana : (card*).
clauses
    payMana(Solution) :-
        foreach Mana in Solution do
            if Mana:name = deck::runawaySteamkinName then
                Mana:removeCounter(p11),
                Mana:removeCounter(p11),
                Mana:removeCounter(p11)
            elseif not(list::isMember(Mana, play)) and Mana:name = deck::simianSpiritGuideName then
                hand := list::remove(hand, Mana),
                exile := [Mana | exile]
            elseif not(list::isMember(Mana, play)) and adventure(Source) = Mana:keywords:getAll_nd() and Source:keywords:contains(ritual) then
                Source:onPlay(This),
                hand := list::remove(hand, Mana),
                exile := [Mana | exile]
            elseif not(list::isMember(Mana, play)) and Mana:keywords:contains(ritual) then
                Mana:onPlay(This),
                hand := list::remove(hand, Mana),
                graveyard := [Mana | graveyard]
            else
                Mana:tapped := true,
                Mana:canTap := false,
                if true = Mana:sacrifice then
                    play := list::remove(play, Mana),
                    graveyard := [Mana | graveyard]
                end if,
                if true = Mana:exile then
                    play := list::remove(play, Mana),
                    exile := [Mana | exile]
                end if
            end if
        end foreach.

clauses
    combat() :-
        if spellsThisTurn >= 3 and CombatReviveList = list::filter(graveyard, { (Phoenix) :- Phoenix:name = deck::arclightPhoenixName })
            and [] <> CombatReviveList
        then
            foreach Revive in CombatReviveList do
                graveyard := list::remove(graveyard, Revive),
                play := [Revive | play],
                if Revive:keywords:contains(haste) then
                    Revive:canTap := true
                end if
            end foreach
        end if,
        CombatReadyCritters =
            [ Card ||
                Card in play,
                Card:cardType = creature,
                false = Card:tapped,
                true = Card:canTap,
                Card:power > 0
            ],
        CombatReadyCritters <> [],
        !,
        foreach Critter in CombatReadyCritters do
            Critter:tap()
        end foreach,
        foreach Critter in CombatReadyCritters do
            opponent:damage(Critter:getPower())
        end foreach.
    combat().

clauses
    endPhase() :-
        reduceHand().

predicates
    reduceHand : ().
clauses
    reduceHand() :-
        list::length(hand) <= handSize,
        !.
    reduceHand() :-
        removeDuplicate_dt(hand),
        !,
        reduceHand().
    reduceHand() :-
        First in hand,
        !,
        hand := list::remove(hand, First),
        graveyard := [First | graveyard],
        reduceHand().
    reduceHand().

predicates
    removeDuplicate_dt : (card*) determ.
clauses
    removeDuplicate_dt([First | Rest]) :-
        list::isMemberEq({ (A, B) :- A:id = B:id }, First, Rest),
        !,
        hand := list::remove(hand, First),
        graveyard := [First | graveyard].
    removeDuplicate_dt([_ | Rest]) :-
        removeDuplicate_dt(Rest).

end implement player
