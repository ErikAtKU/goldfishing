% Copyright Prolog Development Center

implement deck
    open core, rules

facts
    winCon : mapM{cardState, setM{boardState}}.
    decklist : card*.
    mulliganPriority : string*.
    mulliganRemove : string*.
    manaPriority : rules::mana*.
    playPriority : string*.
    scryPriority : string**.
    companion : card*.
    shortName : string.
    discardPriority : string*.

/*----------------------------------------------------------------------------------
    Izzet Decks
----------------------------------------------------------------------------------*/
clauses
    blank(Size) :-
        Blank = card::creature("Blank", [], 1, 1),
        decklist := [ Blank || _ = std::fromTo(1, Size) ],
        winCon := mapM_redBlack::new(),
        mulliganPriority := [],
        mulliganRemove := [],
        manaPriority := [],
        playPriority := [],
        scryPriority := [],
        companion := [],
        shortName := "Blank",
        discardPriority := [].

/*----------------------------------------------------------------------------------
    Izzet Decks
----------------------------------------------------------------------------------*/
facts
    island : card := erroneous.
    mountain : card := erroneous.
    aetherHub : card := erroneous.
    frontierBivouac : card := erroneous.
    izzetGuildmage : card := erroneous.
    biomancersFamiliar : card := erroneous.
    nivixGuildmage : card := erroneous.
    leagueGuildmage : card := erroneous.
    goblinElectromancer : card := erroneous.
    lavaSpike : card := erroneous.
    seethingSong : card := erroneous.
    openTheOmenpaths : card := erroneous.
    irencragFeat : card := erroneous.
    riteOfFlame : card := erroneous.
    channelTheSuns : card := erroneous.
    eyeOfNowhere : card := erroneous.
    desperateRitual : card := erroneous.
    manamorphose : card := erroneous.
    psychicPuppetry : card := erroneous.
    toilsOfNightAndDay : card := erroneous.
    glacialRay : card := erroneous.
    opt : card := erroneous.
    dampenThought : card := erroneous.
    simianSpiritGuide : card := erroneous.
    wildCantor : card := erroneous.
    zirdaTheDawnwalker : card := erroneous.
    dualCasting : card := erroneous.

predicates
    izzetInitialize : ().
clauses
    izzetInitialize() :-
        companion := [],
        % lands
        forest := card::land("Forest", [g]),
        island := card::land("Island", [u]),
        mountain := card::land("Mountain", [r]),
        aetherHub := card::land("Aether Hub", [a]),
        frontierBivouac := card::land("Frontier Bivouac", [r, g, u]),
        frontierBivouac:canTap := false,
        % simian is a "land"
        simianSpiritGuide := card::land(simianSpiritGuideName, [r]),
        simianSpiritGuide:manaUsePriority := 3,
        simianSpiritGuide:canTap := true,
        simianSpiritGuide:exile := true,
        simianSpiritGuide:canBeEnchanted := false,
        simianSpiritGuide:onPlay := { (Player) :- Player:landPlays := Player:landPlays + 1 },
        wildCantor := card::creature("Wild Cantor", [h(g, r)], 1, 1),
        wildCantor:manaType := [r, g, u],
        wildCantor:manaUsePriority := 3,
        wildCantor:canTap := true,
        wildCantor:sacrifice := true,
        wildCantor:canBeEnchanted := false,
        % creatures
        izzetGuildmage := card::creature("Izzet Guildmage", [h(u, r), h(u, r)], 1, 1),
        izzetGuildmage:canCombo := true,
        biomancersFamiliar := card::creature("Biomancer's Familiar", [u, g], 1, 1),
        biomancersFamiliar:canCombo := true,
        zirdaTheDawnwalker := card::creature("Zirda, the Dawnwaker", [i(1), r, r], 1, 1),
        zirdaTheDawnwalker:canCombo := true,
        nivixGuildmage := card::creature("Nivix Guildmage", [u, r], 1, 1),
        nivixGuildmage:canCombo := true,
        leagueGuildmage := card::creature("League Guildmage", [u, r], 1, 1),
        leagueGuildmage:canCombo := true,
        goblinElectromancer := card::creature("Goblin Electromancer", [u, r], 1, 1),
        goblinElectromancer:canCombo := true,
        %enchantments
        dualCasting := card::enchantment("Dual Casting", [i(1), r], c(creature)),
        dualCasting:onEnchant :=
            { (Target, Self) :-
                Self:canCombo := Target:canCombo,
                Self:canTap := Target:canTap,
                Self:tapped := Target:tapped
            },
        dualCasting:saveForCombo := false,
        % sorceries
        lavaSpike := card::sorcery("Lava Spike", [r]),
        seethingSong := card::sorcery("Seething Song", [i(2), r]),
        seethingSong:manaType := [m([r, r, r, r, r])],
        seethingSong:addKeyword(ritual),
        seethingSong:saveForCombo := true,
        openTheOmenpaths := card::sorcery("Open the Omenpaths", [i(2), r]),
        openTheOmenpaths:manaType := [m([r, r, u, u])],
        openTheOmenpaths:addKeyword(ritual),
        openTheOmenpaths:saveForCombo := true,
        irencragFeat := card::sorcery("Irencrag Feat", [i(1), r, r, r]),
        riteOfFlame := card::sorcery("Rite of Flame", [r]),
        riteOfFlame:manaType := [m([r, r])],
        riteOfFlame:addKeyword(ritual),
        riteOfFlame:onPlay :=
            { (Player) :-
                GraveCount = list::filter(Player:graveyard, { (Elem) :- Elem:name = riteOfFlame:name }),
                NextMana = [m(list::nDuplicates(r, list::length(GraveCount) + 2))],
                foreach (Card in Player:hand or Card in Player:library or Card in GraveCount or Card in Player:exile) and Card:name = riteOfFlame:name
                do
                    Card:manaType := NextMana
                end foreach
            },
        riteOfFlame:saveForCombo := true,
        riteOfFlame:manaUsePriority := 1,
        channelTheSuns := card::sorcery("Channel the Suns", [i(3), g]),
        eyeOfNowhere := card::sorcery("Eye of Nowhere", [u, u]),
        % instants
        desperateRitual := card::instant("Desperate Ritual", [i(1), r]),
        manamorphose := card::instant("Manamorphose", [i(1), h(g, r)]),
        manamorphose:onPlay := { (Player) :- Player:draw(1) },
        psychicPuppetry := card::instant("Psychic Puppetry", [i(1), u]),
        toilsOfNightAndDay := card::instant("Toils of Night and Day", [i(2), u]),
        glacialRay := card::instant("Glacial Ray", [i(1), r]),
        dampenThought := card::instant("Dampen Thought", [i(1), u]),
        opt := card::instant("Opt", [u]),
        opt:onPlay :=
            { (Player) :-
                Player:scry(1),
                Player:draw(1)
            },
        opt:saveForCombo := false,
        % Common Conditions,
        winCon := mapM_redBlack::new(),
        generateIzzetWinCons(winCon),
        mulliganPriority := [izzetGuildmage:name],
        mulliganRemove := [],
        manaPriority := [],
        playPriority := [izzetGuildmage:name],
        scryPriority :=
            [
                [izzetGuildmage:name, lavaSpike:name, desperateRitual:name],
                [izzetGuildmage:name, lavaSpike:name, desperateRitual:name, seethingSong:name]
            ],
        discardPriority := [],
        shortName := "Izzet".

predicates
    generateIzzetWinCons : (mapM{cardState, setM{boardState}}).
clauses
    generateIzzetWinCons(WinCons) :-
        GenerateWinsSet = setM_redBlack::new(),
        ArcaneSpliceEnablers = [tuple([i(1), r], [i(1), r], [r, r, r], desperateRitual), tuple([i(1), u], [u], [q], psychicPuppetry)],
        StandardEnablers =
            [
                tuple([i(2), r], [], [r, r, u, u], openTheOmenpaths),
                tuple([i(1), i(1), r], [], [r, r, r, r, r], seethingSong),
                tuple([i(1), r, r, r], [], [r, r, r, r, r, r, r], irencragFeat),
                tuple([r], [], [r, r], riteOfFlame),
                tuple([i(0)], [], [r], simianSpiritGuide),
                tuple([i(1), i(1), i(1), g], [], [w, u, b, r, g], channelTheSuns)
            ],
        AllEnablers = list::append(ArcaneSpliceEnablers, StandardEnablers),
        CopyCheapSorceryAbilities = [tuple([i(2), r], [], izzetGuildmage)],
        CopyCheapInstantAbilities = [tuple([i(2), u], [], izzetGuildmage)],
        CopyAnyAbility = [tuple([i(2), u, r], [], nivixGuildmage)], %tuple([r, q], [], dualCasting)],
        CopyOneCostAbilities = [tuple([i(1), r, q], [], leagueGuildmage)],
        CopyTwoCostAbilities = [tuple([i(2), r, q], [], leagueGuildmage)],
        CopyThreeCostAbilities = [tuple([i(3), r, q], [], leagueGuildmage)],
        AbilityReducers = [tuple([], [i(2)], zirdaTheDawnwalker), tuple([], [i(2)], biomancersFamiliar)],
        ManamorphoseFinishers = [[tuple([i(1), h(r, g)], [a, a], manamorphose)]],
        NivixSunFinishers = [[tuple([i(1), i(1), i(1), g], [w, u, b, r, g], channelTheSuns), tuple([], [], manamorphose)]],
        OneManaArcaneSorcFinishers = [[tuple([r], [], lavaSpike)]],
        TwoManaArcaneSorcFinishers = [[tuple([u, u], [], eyeOfNowhere)]],
        TwoManaArcaneInstFinishers = [[tuple([i(1), r], [], glacialRay)], [tuple([i(1), u], [], dampenThought)]],
        ThreeManaArcaneInstFinishers =
            [
                [tuple([i(1), r], [], glacialRay), tuple([i(1), i(1), u], [q, q], toilsOfNightAndDay)],
                [tuple([i(1), u], [], dampenThought), tuple([i(1), i(1), u], [q, q], toilsOfNightAndDay)]
            ],
        foreach Win = calculateIzzetWinCon_nd(list::append(CopyAnyAbility, CopyCheapInstantAbilities), AbilityReducers, ManamorphoseFinishers, []) do
            addOptimized(Win, GenerateWinsSet)
        end foreach,
        foreach
            Win =
                calculateIzzetWinCon_nd(list::append(CopyAnyAbility, CopyCheapSorceryAbilities), AbilityReducers,
                    list::append(OneManaArcaneSorcFinishers, TwoManaArcaneSorcFinishers), AllEnablers)
        do
            addOptimized(Win, GenerateWinsSet)
        end foreach,
        foreach Win = calculateIzzetWinCon_nd(CopyAnyAbility, AbilityReducers, NivixSunFinishers, StandardEnablers) do
            addOptimized(Win, GenerateWinsSet)
        end foreach,
        foreach
            Win =
                calculateIzzetWinCon_nd(list::append(CopyAnyAbility, CopyCheapSorceryAbilities), AbilityReducers,
                    list::append(OneManaArcaneSorcFinishers, TwoManaArcaneSorcFinishers), AllEnablers)
        do
            addOptimized(Win, GenerateWinsSet)
        end foreach,
        foreach
            Win =
                calculateIzzetWinCon_nd(list::append(CopyAnyAbility, CopyCheapInstantAbilities, CopyTwoCostAbilities), AbilityReducers,
                    TwoManaArcaneInstFinishers, AllEnablers)
        do
            addOptimized(Win, GenerateWinsSet)
        end foreach,
        foreach Win = calculateIzzetWinCon_nd(CopyOneCostAbilities, AbilityReducers, OneManaArcaneSorcFinishers, AllEnablers) do
            addOptimized(Win, GenerateWinsSet)
        end foreach,
        foreach
            Win =
                calculateIzzetWinCon_nd(CopyTwoCostAbilities, AbilityReducers, list::append(TwoManaArcaneSorcFinishers, TwoManaArcaneInstFinishers),
                    AllEnablers)
        do
            addOptimized(Win, GenerateWinsSet)
        end foreach,
        foreach
            Win =
                calculateIzzetWinCon_nd(list::append(CopyAnyAbility, CopyThreeCostAbilities), AbilityReducers, ThreeManaArcaneInstFinishers,
                    AllEnablers)
        do
            addOptimized(Win, GenerateWinsSet)
        end foreach,
        /*ManualList =
            [
                win([izzetGuildmage], [riteOfFlame, lavaSpike], [riteOfFlame, riteOfFlame], [r, r, i(1), i(1)]),
                win([izzetGuildmage], [riteOfFlame, lavaSpike, riteOfFlame], [riteOfFlame], [r, i(1)]),
                win([izzetGuildmage], [riteOfFlame, lavaSpike, riteOfFlame, riteOfFlame], [], [r]),
                win([izzetGuildmage], [riteOfFlame, eyeOfNowhere], [riteOfFlame, riteOfFlame], [u, u, r, r, i(1), i(1)]),
                win([izzetGuildmage], [riteOfFlame, eyeOfNowhere, riteOfFlame], [riteOfFlame], [u, u, r, i(1)]),
                win([izzetGuildmage], [riteOfFlame, eyeOfNowhere, riteOfFlame, riteOfFlame], [], [u, u, r])
            ],
        foreach Win in ManualList do
            addOptimized(Win, GenerateWinsSet)
        end foreach,*/
        foreach Win in GenerateWinsSet do
            rules::insertWincon(WinCons, Win)
        end foreach.

predicates
    calculateIzzetWinCon_nd : (tuple{mana* RecurseCost, mana* RecurseReturn, card Play}* CopyAbility,
        tuple{mana* ReducerCost, mana* AbilityReduction, card Play}* Reducer, tuple{mana* InitialCost, mana* InitialReturn, card Hand}** Finisher,
        tuple{mana* SplicePlayCost, mana* SpliceCost, mana* SpliceReturn, card Hand}* SplicesAndEnablers) -> rules::boardState nondeterm.
clauses
    calculateIzzetWinCon_nd(CopyAbility, Reducers, Finishers, SplicesAndEnablers) = Win :-
        FilterRiteOfFlamePredicate = { (CardToCheck) :- CardToCheck = riteOfFlame },
        tuple(OriginalRecurseCost, _RecurseReturn, Card1) in CopyAbility,
        %Using 0 or 1 reducer
        (tuple(_ReductionCost, RecurseReduction, Card2) in Reducers and PlayCards = [Card1, Card2] or RecurseReduction = [] and PlayCards = [Card1]),
        ReducedRecurseCost = subtractCost(OriginalRecurseCost, RecurseReduction),
        FinisherList in Finishers,
        InitialCost = varM::new([]),
        InitialReturn = varM::new([]),
        InitialSpells = varM::new([]),
        foreach tuple(FinisherInitialCost, FinisherInitialReturn, InitialSpell) in FinisherList do
            InitialCost:value := addCosts(FinisherInitialCost, InitialCost:value),
            InitialReturn:value := addCosts(FinisherInitialReturn, InitialReturn:value),
            InitialSpells:value := [InitialSpell | InitialSpells:value]
        end foreach,
        StandardRecurseCost = subtractCost(ReducedRecurseCost, InitialReturn:value),
        %Using 0 to 3 Splices
        (SpliceTuple1 in SplicesAndEnablers and SpliceList1 = [SpliceTuple1] or SpliceList1 = []),
        (SpliceTuple2 in SplicesAndEnablers and SpliceList2 = [SpliceTuple2] and SpliceList1 <> [] or SpliceList2 = []),
        (SpliceTuple3 in SplicesAndEnablers and SpliceList3 = [SpliceTuple3] and SpliceList2 <> [] or SpliceList3 = []),
        not((SpliceList3 = SpliceList2 and SpliceList2 = SpliceList1 and SpliceList1 <> [])),
        (CanSplice1 = true and SpliceList1 <> [] or CanSplice1 = false),
        (CanSplice2 = true and SpliceList2 <> [] or CanSplice2 = false),
        (CanSplice3 = true and SpliceList3 <> [] or CanSplice3 = false),
        FirstRecurseCost = varM::new(ReducedRecurseCost),
        RecurseCost = varM::new(StandardRecurseCost),
        Spells = varM::new(InitialSpells:value),
        SpliceCost = varM::new([]),
        if [tuple(SpliceBetweenPlayCost1, SpliceCost1, SpliceReturn1, SpliceCard1)] = SpliceList1 and not(SpliceCost1 = []) and true = CanSplice1 then
            RecurseCost:value := subtractCost(RecurseCost:value, SpliceReturn1),
            SpliceCost:value := addCosts(SpliceCost:value, SpliceCost1),
            Spells:value := [SpliceCard1 | Spells:value],
            if SpliceBetweenPlayCost1 <> [] and CastBetween1 = addCosts(SpliceBetweenPlayCost1, subtractCost(FirstRecurseCost:value, SpliceReturn1))
                and list::length(CastBetween1) < list::length(FirstRecurseCost:value)
            then
                FirstRecurseCost:value := CastBetween1
            end if
        end if,
        if [tuple(SpliceBetweenPlayCost2, SpliceCost2, SpliceReturn2, SpliceCard2)] = SpliceList2 and not(SpliceCost2 = []) and true = CanSplice2 then
            RecurseCost:value := subtractCost(RecurseCost:value, SpliceReturn2),
            SpliceCost:value := addCosts(SpliceCost:value, SpliceCost2),
            Spells:value := [SpliceCard2 | Spells:value],
            if SpliceBetweenPlayCost2 <> [] and CastBetween2 = addCosts(SpliceBetweenPlayCost2, subtractCost(FirstRecurseCost:value, SpliceReturn2))
                and list::length(CastBetween2) < list::length(FirstRecurseCost:value)
            then
                FirstRecurseCost:value := CastBetween2
            end if
        end if,
        if [tuple(SpliceBetweenPlayCost3, SpliceCost3, SpliceReturn3, SpliceCard3)] = SpliceList3 and not(SpliceCost3 = []) and true = CanSplice3 then
            RecurseCost:value := subtractCost(RecurseCost:value, SpliceReturn3),
            SpliceCost:value := addCosts(SpliceCost:value, SpliceCost3),
            Spells:value := [SpliceCard3 | Spells:value],
            if SpliceBetweenPlayCost3 <> [] and CastBetween3 = addCosts(SpliceBetweenPlayCost3, subtractCost(FirstRecurseCost:value, SpliceReturn3))
                and list::length(CastBetween3) < list::length(FirstRecurseCost:value)
            then
                FirstRecurseCost:value := CastBetween3
            end if
        end if,
        RecurseCost:value = [],
        ManaCost = varM::new(addCosts(FirstRecurseCost:value, list::append(InitialCost:value, SpliceCost:value))),
        if CanSplice1 = false and [tuple(SplicePlayCost1, _SpliceCost1, SplicePlayVal1, SplicePlayCard1)] = SpliceList1 and not(SplicePlayCost1 = [])
            and PlayOnSide1 = addCosts(SplicePlayCost1, subtractCost(ManaCost:value, SplicePlayVal1))
            and list::length(PlayOnSide1) <= list::length(ManaCost:value)
        then
            ManaCost:value := PlayOnSide1,
            Spells:value := [SplicePlayCard1 | Spells:value]
        end if,
        if CanSplice2 = false and [tuple(SplicePlayCost2, _SpliceCost2, SplicePlayVal2, SplicePlayCard2)] = SpliceList2 and not(SplicePlayCost2 = [])
            and if SplicePlayCard2 = riteOfFlame and list::isMember(riteOfFlame, Spells:value) then
                ExtraVal2 = r
            else
                ExtraVal2 = i(0)
            end if
            and PlayOnSide2 = addCosts(SplicePlayCost2, subtractCost(ManaCost:value, [ExtraVal2 | SplicePlayVal2]))
            and list::length(PlayOnSide2) <= list::length(ManaCost:value)
        then
            ManaCost:value := PlayOnSide2,
            Spells:value := [SplicePlayCard2 | Spells:value]
        end if,
        if CanSplice3 = false and [tuple(SplicePlayCost3, _SpliceCost3, SplicePlayVal3, SplicePlayCard3)] = SpliceList3 and not(SplicePlayCost3 = [])
            and if SplicePlayCard3 = riteOfFlame and list::isMember(riteOfFlame, Spells:value) then
                ExtraVal3a = r
            else
                ExtraVal3a = i(0)
            end if
            and if SplicePlayCard3 = riteOfFlame and [riteOfFlame, riteOfFlame] = list::filter(Spells:value, FilterRiteOfFlamePredicate) then
                ExtraVal3b = r
            else
                ExtraVal3b = i(0)
            end if
            and PlayOnSide3 = addCosts(SplicePlayCost3, subtractCost(ManaCost:value, [ExtraVal3a, ExtraVal3b | SplicePlayVal3]))
            and list::length(PlayOnSide3) <= list::length(ManaCost:value)
        then
            ManaCost:value := PlayOnSide3,
            Spells:value := [SplicePlayCard3 | Spells:value]
        end if,
        ManaCost:value := list::filter(ManaCost:value, { (A) :- not(A = q) }),
        Win = rules::win(PlayCards, list::sort(Spells:value), [], list::sort(ManaCost:value)).

predicates
    addOptimized : (boardState, setM{boardState}).
clauses
    addOptimized(WinSet, GenerateWinsSet) :-
        if not(GenerateWinsSet:contains(WinSet)) then
            win(Play, Hand, Grave, Mana) = WinSet,
            if 2 * list::length(Play) + list::length(Mana) + 3 * list::length(Hand) < 16 then
                % my super professional limiter
                AddAtLast = varM_boolean::new(true),
                Stop = varM_boolean::new(false),
                foreach OtherWin in GenerateWinsSet and not(Stop:isTrue()) do
                    win(OtherPlay, OtherHand, OtherGrave, OtherMana) = OtherWin,
                    IntersectPlay = myIntersect(Play, OtherPlay),
                    IntersectHand = myIntersect(Hand, OtherHand),
                    IntersectGrave = myIntersect(Grave, OtherGrave),
                    IntersectMana = myIntersect(Mana, OtherMana),
                    if IntersectPlay = Play and IntersectHand = Hand and IntersectGrave = Grave and IntersectMana = Mana then
                        GenerateWinsSet:remove(OtherWin),
                        AddAtLast:setTrue()
                    elseif IntersectPlay = OtherPlay and IntersectHand = OtherHand and IntersectGrave = OtherGrave and IntersectMana = OtherMana then
                        Stop:setTrue(),
                        AddAtLast:setFalse()
                    end if
                end foreach,
                if AddAtLast:isTrue() then
                    GenerateWinsSet:insert(WinSet)
                end if
            end if
        end if.

class predicates
    myIntersect : (Elem*, Elem*) -> Elem*.
clauses
    myIntersect([], _) = [] :-
        !.
    myIntersect(_, []) = [] :-
        !.
    myIntersect([Elem | Left], Right) = [Elem | Final] :-
        list::isMember(Elem, Right),
        !,
        RightRest = list::remove(Right, Elem),
        Final = myIntersect(Left, RightRest).
    myIntersect([_Elem | Left], Right) = Final :-
        Final = myIntersect(Left, Right).

clauses
    showIzzetWinCons_nd() = WinConStr :-
        rules::win(Play, Hand, Grave, Mana) in winCon:getValue_nd(),
        WinConStr = string::format("%p - %p - %p - %p", Play, Hand, Grave, Mana).

clauses
    izzetTest() :-
        izzetInitialize(),
        decklist :=
            [ Card ||
                Card in card::copy(island, 10)
                or
                Card in card::copy(mountain, 6)
                or
                Card in card::copy(aetherHub, 4)
                or
                Card in card::copy(frontierBivouac, 4)
                or
                Card in card::copy(izzetGuildmage, 4)
                or
                Card in card::copy(biomancersFamiliar, 4)
                or
                Card in card::copy(goblinElectromancer, 2)
                or
                Card in card::copy(lavaSpike, 4)
                or
                Card in card::copy(seethingSong, 4)
                or
                Card in card::copy(desperateRitual, 4)
                or
                Card in card::copy(eyeOfNowhere, 2)
                or
                Card in card::copy(manamorphose, 4)
                or
                Card in card::copy(psychicPuppetry, 2)
                or
                Card in card::copy(opt, 6)
            ].

clauses
    myIzzetDeck() :-
        izzetInitialize(),
        decklist :=
            [ Card ||
                Card in card::copy(island, 10)
                or
                Card in card::copy(mountain, 6)
                or
                Card in card::copy(aetherHub, 4)
                or
                Card in card::copy(frontierBivouac, 4)
                or
                Card in card::copy(izzetGuildmage, 4)
                or
                Card in card::copy(biomancersFamiliar, 4)
                or
                Card in card::copy(goblinElectromancer, 2)
                or
                Card in card::copy(lavaSpike, 4)
                or
                Card in card::copy(seethingSong, 4)
                or
                Card in card::copy(desperateRitual, 4)
                or
                Card in card::copy(eyeOfNowhere, 2)
                or
                Card in card::copy(manamorphose, 4)
                or
                Card in card::copy(psychicPuppetry, 2)
                or
                Card in card::copy(opt, 6)
            ].

clauses
    izzetManaTest(Forest, Mountain, Island) :-
        izzetInitialize(),
        decklist :=
            [ Card ||
                Card in card::copy(island, Island)
                or
                Card in card::copy(mountain, Mountain)
                or
                Card in card::copy(forest, Forest)
                or
                Card in card::copy(aetherHub, 4)
                or
                Card in card::copy(frontierBivouac, 4)
                or
                Card in card::copy(izzetGuildmage, 4)
                or
                Card in card::copy(biomancersFamiliar, 4)
                or
                Card in card::copy(goblinElectromancer, 2)
                or
                Card in card::copy(lavaSpike, 4)
                or
                Card in card::copy(seethingSong, 4)
                or
                Card in card::copy(desperateRitual, 4)
                or
                Card in card::copy(eyeOfNowhere, 2)
                or
                Card in card::copy(manamorphose, 4)
                or
                Card in card::copy(psychicPuppetry, 2)
                or
                Card in card::copy(opt, 6)
            ].

clauses
    izzetSimianManaTest(Forest, Mountain, Island) :-
        izzetInitialize(),
        decklist :=
            [ Card ||
                Card in card::copy(island, Island)
                or
                Card in card::copy(mountain, Mountain)
                or
                Card in card::copy(forest, Forest)
                or
                Card in card::copy(aetherHub, 4)
                or
                Card in card::copy(frontierBivouac, 4)
                or
                Card in card::copy(izzetGuildmage, 4)
                or
                Card in card::copy(biomancersFamiliar, 4)
                or
                Card in card::copy(goblinElectromancer, 2)
                or
                Card in card::copy(simianSpiritGuide, 4)
                or
                Card in card::copy(lavaSpike, 4)
                or
                Card in card::copy(seethingSong, 4)
                or
                Card in card::copy(desperateRitual, 4)
                or
                Card in card::copy(eyeOfNowhere, 0)
                or
                Card in card::copy(manamorphose, 4)
                or
                Card in card::copy(psychicPuppetry, 0)
                or
                Card in card::copy(opt, 2)
                or
                Card in card::copy(wildCantor, 4)
            ].

clauses
    izzetFullVariable(Aether, Guildmage, Biomancer, Nivix, Electro, Lava, Eye, Seething, Desperate, Mana, Pup, Glacial, Opt) :-
        izzetInitialize(),
        decklist :=
            [ Card ||
                Card in card::copy(aetherHub, Aether)
                or
                Card in card::copy(izzetGuildmage, Guildmage)
                or
                Card in card::copy(biomancersFamiliar, Biomancer)
                or
                Card in card::copy(nivixGuildmage, Nivix)
                or
                Card in card::copy(goblinElectromancer, Electro)
                or
                Card in card::copy(lavaSpike, Lava)
                or
                Card in card::copy(eyeOfNowhere, Eye)
                or
                Card in card::copy(seethingSong, Seething)
                or
                Card in card::copy(desperateRitual, Desperate)
                or
                Card in card::copy(manamorphose, Mana)
                or
                Card in card::copy(psychicPuppetry, Pup)
                or
                Card in card::copy(glacialRay, Glacial)
                or
                Card in card::copy(opt, Opt)
            ],
        list::length(decklist) == 60.

clauses
    izzetZirda(Mountain, Island, Nivix) :-
        izzetZirdaFullVariable(Mountain, Island, 4, 0, 4, Nivix, 2, 4, 4, 4, 2, 4, 4, 4, 2, 4, 0, 0, 0, 0, true),
%        decklist :=
%            [ Card ||
%                Card in card::copy(island, Island)
%                or
%                Card in card::copy(mountain, Mountain)
%                or
%                Card in card::copy(aetherHub, 4)
%                or
%                Card in card::copy(izzetGuildmage, 4)
%                or
%                Card in card::copy(biomancersFamiliar, 0)
%                or
%                Card in card::copy(nivixGuildmage, Nivix)
%                or
%                Card in card::copy(leagueGuildmage, 2)
%                or
%                Card in card::copy(goblinElectromancer, 0)
%                or
%                Card in card::copy(lavaSpike, 4)
%                or
%                Card in card::copy(eyeOfNowhere, 2)
%                or
%                Card in card::copy(seethingSong, 4)
%                or
%                Card in card::copy(desperateRitual, 4)
%                or
%                Card in card::copy(manamorphose, 4)
%                or
%                Card in card::copy(psychicPuppetry, 4)
%                or
%                Card in card::copy(glacialRay, 4)
%                or
%                Card in card::copy(opt, 2)
%                or
%                Card in card::copy(wildCantor, 0)
%                or
%                Card in card::copy(simianSpiritGuide, 4)
%            ],
%        companion := card::copy(zirdaTheDawnwalker, 1),
        list::length(decklist) == 60.

clauses
    izzetFrozenStandard() :-
        Mountain = 12,
        Island = 10,
        Aether = 0,
        Frontier = 0,
        Izzet = 4,
        Nivix = 0,
        League = 0,
        Mana = 0,
        Lava = 4,
        Desperate = 4,
        Eye = 4,
        Song = 4,
        Pup = 4,
        Ray = 4,
        Opt = 10,
        Simian = 0,
        Biomancer = 0,
        Zirda = 0,
        Toils = 0,
        DualCast = 0,
        ZirdaCompanion = false,
        izzetZirdaFullVariable(Mountain, Island, Aether, Frontier, Izzet, Nivix, League, Mana, Lava, Desperate, Eye, Song, Pup, Ray, Opt, Simian,
            Biomancer, Zirda, Toils, DualCast, ZirdaCompanion).

clauses
    izzetZirdaFullVariable(Mountain, Island, Aether, Frontier, Izzet, Nivix, League, Mana, Lava, Desperate, Eye, Song, Pup, Ray, Opt, Simian,
            Biomancer, Zirda, Toils, DualCast, ZirdaCompanion) :-
        izzetInitialize(),
        decklist :=
            list::sort(
                [ Card ||
                    Card in card::copy(island, Island)
                    or
                    Card in card::copy(mountain, Mountain)
                    or
                    Card in card::copy(aetherHub, Aether)
                    or
                    Card in card::copy(frontierBivouac, Frontier)
                    or
                    Card in card::copy(izzetGuildmage, Izzet)
                    or
                    Card in card::copy(nivixGuildmage, Nivix)
                    or
                    Card in card::copy(leagueGuildmage, League)
                    or
                    Card in card::copy(manamorphose, Mana)
                    or
                    Card in card::copy(lavaSpike, Lava)
                    or
                    Card in card::copy(desperateRitual, Desperate)
                    or
                    Card in card::copy(eyeOfNowhere, Eye)
                    or
                    Card in card::copy(seethingSong, Song)
                    or
                    Card in card::copy(psychicPuppetry, Pup)
                    or
                    Card in card::copy(glacialRay, Ray)
                    or
                    Card in card::copy(opt, Opt)
                    or
                    Card in card::copy(simianSpiritGuide, Simian)
                    or
                    Card in card::copy(biomancersFamiliar, Biomancer)
                    or
                    Card in card::copy(zirdaTheDawnwalker, Zirda)
                    or
                    Card in card::copy(toilsOfNightAndDay, Toils)
                    or
                    Card in card::copy(dualCasting, DualCast)
                ]),
        if true = ZirdaCompanion then
            companion := card::copy(zirdaTheDawnwalker, 1),
            Title = "Zirda"
        else
            Title = "Izzet"
        end if,
        list::length(decklist) == 60,
        shortName :=
            string::format("%s M%d I%d A%d F%d I%d N%d L%d M%d L%d D%d E%d S%d P%d R%d O%d S%d B%d Z%d T%d D%d", Title, Mountain, Island, Aether,
                Frontier, Izzet, Nivix, League, Mana, Lava, Desperate, Eye, Song, Pup, Ray, Opt, Simian, Biomancer, Zirda, Toils, DualCast).

/*----------------------------------------------------------------------------------
    Genju Decks
----------------------------------------------------------------------------------*/
facts
    forest : card := erroneous.
    arborElf : card := erroneous.
    voyagingSatyr : card := erroneous.
    bloomTender : card := erroneous.
    benefactionOfRhonas : card := erroneous.
    communeWithTheGods : card := erroneous.
    fertileGround : card := erroneous.
    utopiaSprawl : card := erroneous.
    freedFromTheReal : card := erroneous.

predicates
    genjuInitialize : ().
clauses
    genjuInitialize() :-
        companion := [],
        % lands
        forest := card::land("Forest", [g]),
        island := card::land("Island", [u]),
        % creatures
        arborElf := card::creature("Arbor Elf", [g], 1, 1),
        arborElf:manaUsePriority := 2,
        arborElf:untapStyle :=
            { (Card) :-
                Card:canTap := true,
                Card:tapped := false,
                Card:canCombo := true,
                Card:manaType := [q]
            },
        voyagingSatyr := card::creature("Voyaging Satyr", [i(1), g], 1, 1),
        voyagingSatyr:manaUsePriority := 2,
        voyagingSatyr:untapStyle :=
            { (Card) :-
                Card:canTap := true,
                Card:tapped := false,
                Card:canCombo := true,
                Card:manaType := [q]
            },
        bloomTender := card::creature("Bloom Tender", [i(1), g], 1, 1),
        % sorceries
        benefactionOfRhonas := card::sorcery("Benefation of Rhonas", [i(2), g]),
        benefactionOfRhonas:onPlay :=
            { (Player) :-
                RevealedCards = Player:reveal(5),
                CurrentCards = list::map(list::append(Player:play, Player:hand), { (Card) = Card:name }),
                RemainingScry = list::map(Player:scryPriority, { (PriorityList) = list::difference(PriorityList, CurrentCards) }),
                SortedList = list::sortBy({ (A, B) = compare(list::length(A), list::length(B)) }, RemainingScry),
                TakeList = varM::new([]),
                foreach List in SortedList and Item in List do
                    if TakeCard = list::exists(RevealedCards, { (TakeCard) :- TakeCard:name = Item }) then
                        TakeList:value := list::removeDuplicatesBy({ (A, B) = compare(A:name, B:name) }, [TakeCard | TakeList:value])
                    end if
                end foreach,
                TakeCreature = varM::new([]),
                foreach CreatureCard in TakeList:value and CreatureCard:cardType = creature do
                    TakeCreature:value := [CreatureCard]
                end foreach,
                TakeList:value := list::difference(TakeList:value, TakeCreature:value),
                TakeEnchant = varM::new([]),
                foreach EnchantCard in TakeList:value and EnchantCard:cardType = enchantment(_X) do
                    TakeEnchant:value := [EnchantCard]
                end foreach,
                TakeList:value := list::difference(TakeList:value, TakeEnchant:value),
                Player:putInGraveyard(TakeList:value),
                Player:hand := list::append(TakeCreature:value, TakeEnchant:value, Player:hand)
            },
        benefactionOfRhonas:saveForCombo := false,
        communeWithTheGods := card::sorcery("Commune with the Gods", [i(1), g]),
        communeWithTheGods:onPlay :=
            { (Player) :-
                RevealedCards = Player:reveal(5),
                CurrentCards = list::map(list::append(Player:play, Player:hand), { (Card) = Card:name }),
                RemainingScry = list::map(Player:scryPriority, { (PriorityList) = list::difference(PriorityList, CurrentCards) }),
                SortedList = list::sortBy({ (A, B) = compare(list::length(A), list::length(B)) }, RemainingScry),
                TakeList = varM::new([]),
                foreach List in SortedList and Item in List do
                    if TakeCard = list::exists(RevealedCards, { (TakeCard) :- TakeCard:name = Item }) then
                        TakeList:value := list::removeDuplicatesBy({ (A, B) = compare(A:name, B:name) }, [TakeCard | TakeList:value])
                    end if
                end foreach,
                TakeCreatureOrEnchant = varM::new([]),
                foreach ToTakeCard in TakeList:value and (ToTakeCard:cardType = creature or ToTakeCard:cardType = enchantment(_X)) do
                    TakeCreatureOrEnchant:value := [ToTakeCard]
                end foreach,
                TakeList:value := list::difference(TakeList:value, TakeCreatureOrEnchant:value),
                Player:putInGraveyard(TakeList:value),
                Player:hand := list::append(TakeCreatureOrEnchant:value, Player:hand)
            },
        communeWithTheGods:saveForCombo := false,
        % instants
        manamorphose := card::instant("Manamorphose", [i(1), h(g, r)]),
        % enchantments
        utopiaSprawl := card::enchantment("Utopia Sprawl", [g], n("Forest")),
        utopiaSprawl:onEnchant :=
            { (Card, _Self) :-
                NewManaType =
                    [ NewMana ||
                        ManaType in Card:manaType,
                        if m(List) = ManaType then
                            NewMana = m([u | List])
                        else
                            NewMana = m([u, ManaType])
                        end if
                    ],
                Card:manaType := NewManaType
            },
        utopiaSprawl:saveForCombo := false,
        fertileGround := card::enchantment("Fertile Ground", [i(1), g], c(land)),
        fertileGround:onEnchant :=
            { (Card, _Self) :-
                NewManaType =
                    [ NewMana ||
                        ManaType in Card:manaType,
                        ManaSymbol in [u, g],
                        if m(List) = ManaType then
                            NewMana = m([ManaSymbol | List])
                        else
                            NewMana = m([ManaSymbol, ManaType])
                        end if
                    ],
                Card:manaType := NewManaType
            },
        fertileGround:saveForCombo := false,
        freedFromTheReal := card::enchantment("Freed from the Real", [i(2), u], c(creature)),
        % Common Conditions,
        winCon := mapM_redBlack::new(),
        rules::insertWincon(winCon, rules::win([bloomTender], [freedFromTheReal], [], [i(2), u])),
        rules::insertWincon(winCon, rules::win([arborElf, utopiaSprawl], [freedFromTheReal], [], [i(2), u, u])),
        rules::insertWincon(winCon, rules::win([arborElf], [utopiaSprawl, freedFromTheReal], [], [i(2), g])),
        rules::insertWincon(winCon, rules::win([arborElf, fertileGround], [freedFromTheReal], [], [i(2), u, u])),
        rules::insertWincon(winCon, rules::win([voyagingSatyr, utopiaSprawl], [freedFromTheReal], [], [i(2), u, u])),
        rules::insertWincon(winCon, rules::win([voyagingSatyr, fertileGround], [freedFromTheReal], [], [i(2), u, u])),
        mulliganPriority := [freedFromTheReal:name],
        mulliganRemove := [],
        manaPriority := [],
        playPriority := [arborElf:name],
        scryPriority :=
            [
                [freedFromTheReal:name, bloomTender:name],
                [freedFromTheReal:name, utopiaSprawl:name, arborElf:name],
                [freedFromTheReal:name, fertileGround:name, arborElf:name],
                [freedFromTheReal:name, utopiaSprawl:name, voyagingSatyr:name],
                [freedFromTheReal:name, fertileGround:name, voyagingSatyr:name]
            ],
        discardPriority := [],
        shortName := "Genju".

clauses
    genju() :-
        genjuInitialize(),
        decklist :=
            [ Card ||
                Card in card::copy(forest, 20)
                or
                Card in card::copy(island, 8)
                or
                Card in card::copy(arborElf, 4)
                or
                Card in card::copy(voyagingSatyr, 4)
                or
                Card in card::copy(bloomTender, 4)
                or
                Card in card::copy(utopiaSprawl, 4)
                or
                Card in card::copy(fertileGround, 4)
                or
                Card in card::copy(freedFromTheReal, 4)
                or
                Card in card::copy(communeWithTheGods, 4)
                or
                Card in card::copy(benefactionOfRhonas, 4)
            ].
/*----------------------------------------------------------------------------------
    Narset Decks
----------------------------------------------------------------------------------*/

facts
    narsetEnlightenedMaster : card := erroneous.
    pullFromEternity : card := erroneous.
    goryosVengeance : card := erroneous.

predicates
    narsetInitialize : ().
clauses
    narsetInitialize() :-
        companion := [],
        % lands
        aetherHub := card::land("Aether Hub", [r, g, u, w, b]),
        % creatures
        narsetEnlightenedMaster := card::creature("Narset, Enlightened Master", [i(3), w, u, r], 3, 2),
        narsetEnlightenedMaster:attackStyle :=
            { (Player) :-
                Reveal = Player:reveal(4),
                list::filter(Reveal,
                    { (Card) :-
                        Card:cardType <> creature,
                        Card:cardType <> land,
                        Card:manaCost := []
                    },
                    Castable, Exile),
                Player:exile := list::append(Exile, Player:exile),
                Player:hand := list::append(Castable, Player:hand)
            },
        simianSpiritGuide := card::creature(simianSpiritGuideName, [i(0)], 1, 1),
        simianSpiritGuide:manaType := [r],
        simianSpiritGuide:manaUsePriority := 3,
        simianSpiritGuide:canTap := true,
        simianSpiritGuide:exile := true,
        shortName := "Narset".

/*----------------------------------------------------------------------------------
    Arclight Phoenix
----------------------------------------------------------------------------------*/
facts
    arclightPhoenix : card := erroneous.
    runawaySteamkin : card := erroneous.
    oxOfAgonas : card := erroneous.
    merchantOfTheVale : card := erroneous.
    haggle : card := erroneous.
    thrillOfPossibility : card := erroneous.
    rosethornAcolyte : card := erroneous.
    seasonalRitual : card := erroneous.
    maximizeVelocity : card := erroneous.
    shock : card := erroneous.
    catharticReunion : card := erroneous.
    incubation : card := erroneous.
    electrostaticField : card := erroneous.
    songOfCreation : card := erroneous.
    teferisTutelage : card := erroneous.
    enhancedSurveillance : card := erroneous.
    ketriaTriome : card := erroneous.

clauses
    arclightInitialize() :-
        %onPlay Effects
        AllIncidentalEffects =
            { (Player) :-
                %song of creation
                foreach Card in Player:play and Card <> This and Card:name = songOfCreation:name do
                    Player:draw(2)
                end foreach,
                %electrostatic field effect
                foreach Card in Player:play and Card <> This and Card:name = electrostaticField:name do
                    Player:opponent:damage(1)
                end foreach
            },
        RedIncidentalEffects =
            { (Player) :-
                %steamkin
                foreach
                    Card in Player:play and Card <> This and Card:name = runawaySteamkin:name
                    and P11List = list::filter(Card:countersList, { (Elem) :- Elem = p11 }) and list::length(P11List) < 3
                do
                    Card:addCounter(p11)
                end foreach,
                AllIncidentalEffects(Player)
            },
        OxOfAgonasOnPlay =
            { (Player) :-
                RedIncidentalEffects(Player),
                HandSize = list::length(Player:hand),
                Player:discard(HandSize + 1),
                Player:draw(3)
            },
        companion := [],
        % lands
        ketriaTriome := card::land("Ketria Triome", [g, u, r]),
        ketriaTriome:tapped := true,
        ketriaTriome:canTap := false,
        forest := card::land("Forest", [g]),
        island := card::land("Island", [u]),
        mountain := card::land("Mountain", [r]),
        % creatures
        runawaySteamKin := card::creature(runawaySteamkinName, [i(1), r], 1, 1),
        runawaySteamkin:manaUsePriority := 1,
        runawaySteamkin:onPlay := RedIncidentalEffects,
        arclightPhoenix := card::creature(arclightPhoenixName, [i(3), r], 3, 2),
        arclightPhoenix:addKeyword(haste),
        arclightPhoenix:onPlay := RedIncidentalEffects,
        oxOfAgonas := card::creature(oxOfAgonasName, [i(3), r, r], 4, 2),
        oxOfAgonas:addKeyword(
            escape([r, r], 8,
                { (Card, Player) :-
                    Card:addCounter(p11),
                    OxOfAgonasOnPlay(Player)
                })),
        oxOfAgonas:onPlay := OxOfAgonasOnPlay,
        electrostaticField := card::creature("Electrostatic Field", [i(1), r], 0, 4),
        electrostaticField:onPlay := RedIncidentalEffects,
        %adventures
        haggle := card::instant("Haggle", [r]),
        haggle:onPlay :=
            { (Player) :-
                if list::length(Player:hand) > 1 then
                    Player:discard(1),
                    Player:draw(1)
                end if,
                RedIncidentalEffects(Player)
            },
        haggle:saveForCombo := false,
        merchantOfTheVale := card::creature("Merchant of the Vale", [i(2), r], 2, 3),
        merchantOfTheVale:addKeyword(adventure(haggle)),
        merchantOfTheVale:onPlay := RedIncidentalEffects,
        seasonalRitual := card::sorcery("Seasonal Ritual", [g]),
        seasonalRitual:manaType := [a],
        seasonalRitual:manaUsePriority := 2,
        seasonalRitual:saveForCombo := true,
        seasonalRitual:addKeyword(ritual),
        seasonalRitual:onPlay := AllIncidentalEffects,
        rosethornAcolyte := card::creature("Rosethorn Acolyte", [i(5), g], 2, 3),
        rosethornAcolyte:addKeyword(adventure(seasonalRitual)),
        rosethornAcolyte:manaType := [a],
        rosethornAcolyte:manaUsePriority := 1,
        rosethornAcolyte:saveForCombo := true,
        rosethornAcolyte:onPlay := AllIncidentalEffects,
        %instant
        shock := card::instant("Shock", [r]),
        ShockEffect =
            { (Player) :-
                Player:opponent:damage(2),
                RedIncidentalEffects(Player)
            },
        shock:onPlay := ShockEffect,
        shock:saveForCombo := false,
        thrillOfPossibility := card::instant("Thrill of Possibility", [i(1), r]),
        thrillOfPossibility:addKeyword(discards(1)),
        thrillOfPossibility:onPlay :=
            { (Player) :-
                Player:draw(2),
                RedIncidentalEffects(Player)
            },
        %sorcery
        maximizeVelocity := card::sorcery(maximizeVelocityName, [r]),
        maximizeVelocity:addKeyword(jumpstart),
        maximizeVelocity:onPlay :=
            { (Player) :-
                if Card in Player:play and Card:cardType = creature and not(Card:keywords:contains(haste)) and Card:tapped = false
                    and Card:canTap = false
                then
                    Card:addKeyword(haste),
                    Card:canTap := true
                elseif Card in Player:play and Card:cardType = creature then
                    Card:addKeyword(haste)
                end if,
                RedIncidentalEffects(Player)
            },
        catharticReunion := card::sorcery("Cathartic Reunion", [i(1), r]),
        catharticReunion:addKeyword(discards(2)),
        catharticReunion:onPlay :=
            { (Player) :-
                Player:draw(3),
                RedIncidentalEffects(Player)
            },
        catharticReunion:saveForCombo := false,
        incubation := card::sorcery("Incubation", [h(g, u)]),
        incubation:onPlay :=
            { (Player) :-
                AllIncidentalEffects(Player),
                Top5 = varM::new([]),
                foreach _ = std::fromTo(1, 5) and Scry = Player:library:tryRemoveFirst() do
                    Top5:value := [Scry | Top5:value]
                end foreach,
                if Card in Top5:value and Card:name = runawaySteamkin:name
                    or Card in Top5:value and Card:name = rosethornAcolyte:name
                    or Card in Top5:value and Card:name = arclightPhoenix:name
                    or Card in Top5:value and Card:cardType = creature
                then
                    Top5:value := list::remove(Top5:value, Card),
                    Player:hand := [Card | Player:hand]
                end if,
                foreach Remaining in Top5:value do
                    Player:library:insert(Remaining)
                end foreach
            },
        incubation:saveForCombo := false,
        %enchantment
        songOfCreation := card::enchantment(songOfCreationName, [i(1), g, u, r], board),
        songOfCreation:onPlay :=
            { (Player) :-
                Player:landplays := Player:landplays + 1,
                RedIncidentalEffects(Player)
            },
        songOfCreation:saveForCombo := false,
        teferisTutelage := card::enchantment("Teferi's Tutelage", [i(1), u, u], board),
        teferisTutelage:onPlay :=
            { (Player) :-
                AllIncidentalEffects(Player),
                PrevOnDraw = Player:onDraw,
                Player:onDraw :=
                    { (Draw) :-
                        Player:opponent:mill(2 * Draw),
                        PrevOnDraw(Draw)
                    },
                Player:draw(1),
                Player:discard(1)
            },
        teferisTutelage:saveForCombo := false,
        enhancedSurveillance := card::enchantment("Enhanced Surveillance", [i(1), u], board),
        enhancedSurveillance:onPlay :=
            { (Player) :-
                AllIncidentalEffects(Player),
                PrevOnDraw = Player:onDraw,
                Player:onDraw :=
                    { (Draw) :-
                        if Draw >= list::length(Player:library:asList) and Card in Player:play and Card:name = enhancedSurveillance:name then
                            Player:play := list::remove(Player:play, Card),
                            Player:exile := [Card | Player:exile],
                            Grave = Player:graveyard,
                            Player:graveyard := [],
                            Player:shuffle(Grave, Player:library:asList)
                        end if,
                        PrevOnDraw(Draw)
                    }
            },
        enhancedSurveillance:saveForCombo := false,
        shortName := "Arclight",
        winCon := mapM_redBlack::new(),
        mulliganPriority := [runawaySteamkin:name, arclightPhoenix:name, songOfCreation:name],
        mulliganRemove := [],
        decklist := [],
        manaPriority := [r, g, u],
        playPriority := [ketriaTriome:name, songOfCreation:name, runawaySteamkin:name, runawaySteamkin:name, electrostaticField:name],
        discardPriority := [arclightPhoenix:name, oxOfAgonas:name, maximizeVelocity:name],
        scryPriority := [].

clauses
    arclightFullVariable(Ketria, Forest, Mountain, Island, Phoenix, Ox, Runaway, Electrostatic, Merchant, Rosethorn, Shock, Cathartic, Song,
            Maximize, Teferis, Thrill, Surveillance, Incubation) :-
        arclightInitialize(),
        decklist :=
            list::sort(
                [ Card ||
                    Card in card::copy(ketriaTriome, Ketria)
                    or
                    Card in card::copy(forest, Forest)
                    or
                    Card in card::copy(mountain, Mountain)
                    or
                    Card in card::copy(island, Island)
                    or
                    Card in card::copy(arclightPhoenix, Phoenix)
                    or
                    Card in card::copy(oxOfAgonas, Ox)
                    or
                    Card in card::copy(runawaySteamkin, Runaway)
                    or
                    Card in card::copy(electrostaticField, Electrostatic)
                    or
                    Card in card::copy(merchantOfTheVale, Merchant)
                    or
                    Card in card::copy(rosethornAcolyte, Rosethorn)
                    or
                    Card in card::copy(shock, Shock)
                    or
                    Card in card::copy(catharticReunion, Cathartic)
                    or
                    Card in card::copy(songOfCreation, Song)
                    or
                    Card in card::copy(maximizeVelocity, Maximize)
                    or
                    Card in card::copy(teferisTutelage, Teferis)
                    or
                    Card in card::copy(thrillOfPossibility, Thrill)
                    or
                    Card in card::copy(enhancedSurveillance, Surveillance)
                    or
                    Card in card::copy(incubation, Incubation)
                ]),
        list::length(decklist) == 60.

clauses
    quickArclight() :-
        Ketria = 20,
        Forest = 0,
        Mountain = 0,
        Island = 0,
        Phoenix = 4,
        Ox = 4,
        Runaway = 4,
        Electrostatic = 0,
        Merchant = 4,
        Rosethorn = 4,
        Shock = 4,
        Cathartic = 4,
        Song = 4,
        Maximize = 4,
        Teferis = 0,
        Thrill = 0,
        Surveillance = 0,
        Incubation = 4,
        arclightFullVariable(Ketria, Forest, Mountain, Island, Phoenix, Ox, Runaway, Electrostatic, Merchant, Rosethorn, Shock, Cathartic, Song,
            Maximize, Teferis, Thrill, Surveillance, Incubation),
        shortName := "Quick Arclight".

clauses
    millArclight() :-
        Ketria = 24,
        Forest = 0,
        Mountain = 0,
        Island = 0,
        Phoenix = 0,
        Ox = 0,
        Runaway = 4,
        Electrostatic = 0,
        Merchant = 4,
        Rosethorn = 4,
        Shock = 0,
        Cathartic = 4,
        Song = 4,
        Maximize = 4,
        Teferis = 4,
        Thrill = 4,
        Surveillance = 4,
        Incubation = 0,
        arclightFullVariable(Ketria, Forest, Mountain, Island, Phoenix, Ox, Runaway, Electrostatic, Merchant, Rosethorn, Shock, Cathartic, Song,
            Maximize, Teferis, Thrill, Surveillance, Incubation),
        mulliganPriority := [runawaySteamkin:name, teferisTutelage:name, songOfCreation:name],
        shortName := "Song Mill".

clauses
    electroArclight() :-
        Ketria = 20,
        Forest = 0,
        Mountain = 0,
        Island = 0,
        Phoenix = 4,
        Ox = 4,
        Runaway = 4,
        Electrostatic = 4,
        Merchant = 4,
        Rosethorn = 4,
        Shock = 4,
        Cathartic = 4,
        Song = 4,
        Maximize = 4,
        Teferis = 0,
        Thrill = 0,
        Surveillance = 0,
        Incubation = 0,
        arclightFullVariable(Ketria, Forest, Mountain, Island, Phoenix, Ox, Runaway, Electrostatic, Merchant, Rosethorn, Shock, Cathartic, Song,
            Maximize, Teferis, Thrill, Surveillance, Incubation),
        shortName := "Electro Arclight".

clauses
    thrillArclight() :-
        Ketria = 20,
        Forest = 0,
        Mountain = 0,
        Island = 0,
        Phoenix = 4,
        Ox = 4,
        Runaway = 4,
        Electrostatic = 0,
        Merchant = 0,
        Rosethorn = 4,
        Shock = 4,
        Cathartic = 4,
        Song = 4,
        Maximize = 4,
        Teferis = 0,
        Thrill = 4,
        Surveillance = 0,
        Incubation = 4,
        arclightFullVariable(Ketria, Forest, Mountain, Island, Phoenix, Ox, Runaway, Electrostatic, Merchant, Rosethorn, Shock, Cathartic, Song,
            Maximize, Teferis, Thrill, Surveillance, Incubation),
        shortName := "Electro Arclight".
/*----------------------------------------------------------------------------------
    Print Decks
----------------------------------------------------------------------------------*/

clauses
    getDecklist() = FinalString :-
        Lands = presenter_h(decklist, land),
        Creatures = presenter_h(decklist, creature),
        Instants = presenter_h(decklist, instant),
        Sorceries = presenter_h(decklist, sorcery),
        Artifacts = presenter_h(decklist, artifact),
        Enchantments = presenter_h_ench(decklist),
        Companion = presenter_h(companion, creature),
        FullList =
            [ Row ||
                Lands <> [],
                (Row = "--Lands--" or Row in Lands)
                or
                Creatures <> [],
                (Row = "--Creatures--" or Row in Creatures)
                or
                Instants <> [],
                (Row = "--Instants--" or Row in Instants)
                or
                Sorceries <> [],
                (Row = "--Sorceries--" or Row in Sorceries)
                or
                Artifacts <> [],
                (Row = "--Artifacts--" or Row in Artifacts)
                or
                Enchantments <> [],
                (Row = "--Enchantments--" or Row in Enchantments)
                or
                Companion <> [],
                (Row = "--Companion--" or Row in Companion)
            ],
        FinalString = string::concatWithDelimiter([shortName | FullList], "\n").

predicates
    presenter_h : (card*, rules::cardType) -> string*.
clauses
    presenter_h(List, CardType) = ResultList :-
        CardsOfType = list::filter(List, { (Card) :- Card:cardType = CardType }),
        CardNames = list::map(CardsOfType, { (Card) = Card:name }),
        Grouping = list::groupByEq({ (Card1, Card2) :- Card1 = Card2 }, CardNames),
        ResultList = list::filteredMap(Grouping, { ([Head | Sublist]) = string::format("%02d %s", list::length([Head | Sublist]), Head) }).

predicates
    presenter_h_ench : (card*) -> string*.
clauses
    presenter_h_ench(List) = ResultList :-
        CardsOfType = list::filter(List, { (Card) :- Card:cardType = enchantment(_) }),
        CardNames = list::map(CardsOfType, { (Card) = Card:name }),
        Grouping = list::groupByEq({ (Card1, Card2) :- Card1 = Card2 }, CardNames),
        ResultList = list::filteredMap(Grouping, { ([Head | Sublist]) = string::format("%02d %s", list::length([Head | Sublist]), Head) }).

end implement deck
